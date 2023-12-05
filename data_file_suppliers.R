#### Getting Started ####
rm(list = ls())

#### Libraries ####
library(tidyverse)
library(readr)
library(readxl)
library(janitor)

#### Set up ####

# Choose level of granularity
# min 2, max 6

x <- 3

#### Data Tables ####

# Industry 
industry_naics <- read_csv("data/naics.csv",
                           col_types = cols(datadate = col_skip(),
                                            fyear = col_skip(),
                                            indfmt = col_skip(),
                                            consol = col_skip(),
                                            popsrc = col_skip(),
                                            datafmt = col_skip(),
                                            curcd = col_skip(),
                                            fyr = col_skip(),
                                            costat = col_skip(),
                                            naics = col_character())) %>%
  na.omit() %>%
  mutate(l = nchar(naics),
         naics = case_when(l == 5 ~ paste0(naics,"0"),
                           l == 4 ~ paste0(naics,"00"),
                           l == 3 ~ paste0(naics,"000"),
                           l == 2 ~ paste0(naics,"00000"),
                           l == 1 ~ paste0(naics,"000000"),
                           l == 6 ~ naics)) %>%
  select(gvkey,naics) %>%
  unique() %>%
  mutate(naics = substr(naics,1,x))

supply_ind <- industry_naics %>%
  set_names("gvkey","supp_naics")

client_ind <- industry_naics %>%
  set_names("cgvkey","client_naics")

# Supply data
supply_chain <- read_csv("data/supply_chain.csv", 
                         col_types = cols(conm = col_skip(),
                                          srcdate = col_date(format = "%d/%m/%Y"),
                                          sid = col_skip(), ctype = col_skip(), 
                                          scusip = col_skip(), stic = col_skip(), 
                                          ccusip = col_skip(), ctic = col_skip())) %>%
  na.omit() %>%
  mutate(y = year(srcdate)) %>%
  left_join(.,supply_ind) %>%
  left_join(.,client_ind) %>%
  group_by(supp_naics,client_naics,y) %>%
  reframe(volume = sum(salecs)) %>%
  filter(supp_naics != client_naics)

# Calculations for aij coefficients
# Calulation of distance based on Rank

coef_table <- supply_chain %>%
  group_by(supp_naics,y) %>%
  reframe(volume = sum(volume)) %>%
  set_names("client_naics","y","sales") %>%
  inner_join(.,supply_chain) %>%
  mutate(aij = volume/sales)

# Matrix form

coef_matrix <- coef_table %>%
  select(supp_naics,client_naics,y,aij) %>%
  pivot_wider(.,
              names_from = client_naics,
              values_from = aij,
              values_fill = 0) %>%
  arrange(y, supp_naics)

distance_table <- coef_matrix %>%
  pivot_longer(-c(y,supp_naics),
               names_to = "client_naics",
               values_to = "aij") %>%
  group_by(y,supp_naics) %>%
  mutate(D = rank(aij)) %>%
  slice_max(order_by = D, n = 5) %>%
  filter(D > 0) %>%
  ungroup() %>%
  select(-aij)

distance_table_reg <- distance_table %>%
  group_by(y,supp_naics) %>%
  reframe(N = n()) %>%
  filter(N == 5) %>%
  inner_join(.,distance_table) %>%
  select(y,supp_naics,client_naics) %>%
  group_by(y,supp_naics) %>%
  mutate(Nr = paste0("S",1:5)) %>%
  ungroup() %>%
  rename(naics = client_naics)

#### Aggregation ####
weights <- read_csv("data/d7jt94b9voief91j.csv",
                    col_types = cols(datadate = col_date(format = "%d/%m/%Y"),fyear = col_skip(),
                                     indfmt = col_skip(),
                                     consol = col_skip(), popsrc = col_skip(),
                                     datafmt = col_skip(), tic = col_skip(),
                                     conm = col_skip(), curcd = col_skip(),
                                     fyr = col_skip(), ao = col_skip(),
                                     cgti = col_skip(), ni = col_skip(),
                                     costat = col_skip())) %>%
  mutate(y = year(datadate)) %>%
  group_by(gvkey,y) %>%
  reframe(weight = sum(at,na.rm = T))

ratios <- read_csv("data/ratios.csv")

# Check missing values
non_missing_ratio <- colMeans(!is.na(ratios))

# Drop missing over 30%
keep <- names(non_missing_ratio)[which(non_missing_ratio>=.7)]

agg <- ratios %>%
  mutate(y = year(as.Date(qdate,"%d/%m/%Y"))) %>%
  left_join(.,industry_naics) %>%
  left_join(.,weights) %>%
  select(c(keep,naics,y,weight)) %>%
  select(-c(gvkey,permno,adate,qdate,public_date,TICKER,cusip)) %>%
  filter(!is.na(naics) & !is.na(weight)) %>%
  group_by(y,naics) %>%
  summarise_all(~ weighted.mean(., weight,na.rm = T)) %>%
  ungroup() %>%
  lapply(., function(x) ifelse(is.nan(x), NA, x)) %>%
  as_tibble()

controls_table_reg <- distance_table_reg %>%
  left_join(.,agg) %>%
  select(y,supp_naics,Nr,GProf,roe,pretret_earnat,npm) %>%
  pivot_wider(id_cols = c(y,supp_naics),
              names_from = Nr,
              values_from = c(GProf,roe,
                              pretret_earnat,npm),
              values_fill = 0) %>%
  rename(naics = supp_naics)


#### Events ####

events <- read_csv("data/events.csv",
                   col_types = cols(announcedate = col_date(format = "%d/%m/%Y"))) %>%
  mutate(event = case_when(keydeveventtypeid == 7 ~ 1,
                           keydeveventtypeid == 89 ~ 1,
                           keydeveventtypeid == 74 ~ 1,
                           TRUE ~ 0),
         y = year(announcedate)) %>%
  select(gvkey,y,event) %>%
  left_join(.,industry_naics) %>%
  group_by(y,naics) %>%
  reframe(D = mean(event)) %>%
  na.omit()

supp_events <- distance_table_reg %>%
  left_join(.,events) %>%
  pivot_wider(id_cols = c(y,supp_naics),
              names_from = Nr,
              values_from = D,
              values_fill = 0) %>%
  rename(naics = supp_naics)

supp_events[is.na(supp_events)] <- 0

#### Final Database ####

dat_final <- supp_events %>%
  left_join(.,events) %>%
  left_join(.,controls_table_reg) %>%
  left_join(.,agg) %>%
  select(naics,y,D,everything())
  
write.csv(dat_final,"export/dat_final_supplier.csv",row.names = F, na = "")