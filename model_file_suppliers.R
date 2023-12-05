#### Getting Started ####
rm(list = ls())

#### Libraries ####
library(tidyverse)
library(plm)
library(glmnet)
library(readr)
library(stargazer)

dat_final <- read_csv("export/dat_final_supplier.csv",
                      col_types = cols(weight = col_skip()))

regularization <- na.omit(dat_final)

#### Variable Selection (LASSO) ####

set.seed(123)
x <- as.matrix(regularization[-c(1,3:8)])
y <- regularization$D

# Extract coefficients
cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
final_coeffs <- coef(lasso_model)

vars <- final_coeffs@Dimnames[[1]][-1][final_coeffs@i]

formula <- vars %>%
  paste(.,collapse = " + ") %>%
  paste0("D ~ S1 + S2 + S3 + S4 + S5 + ",.)

#### Model ####

fixed <- plm(formula, data=dat_final,
             index=c("naics", "y"), model="within")  #fixed model
random <- plm(formula, data=dat_final,
              index=c("naics", "y"), model="random")  #random model
phtest(fixed,random) #Hausman test


m1 <- plm(D ~ S1 + S2, data=dat_final,
          index=c("naics", "y"), model="within")
m2 <- plm(D ~ S1 + S2 + S3, data=dat_final,
          index=c("naics", "y"), model="within")
m3 <- plm(D ~ S1 + S2 + S3 + S4, data=dat_final,
          index=c("naics", "y"), model="within")
m4 <- plm(D ~ S1 + S2 + S3 + S4 + S5, data=dat_final,
          index=c("naics", "y"), model="within")

# Create a list of models
model_list <- list(m1, m2, m3, m4, fixed)

# Create the regression table
stargazer(model_list,
          title = "Fixed Effects Regression Models",
          align = TRUE,
          omit.stat = "f", 
          type = "text", # Use "latex" if you want a LaTeX output
          out = "regression_supplier_table.txt") # Replace with desired output file name and extension
