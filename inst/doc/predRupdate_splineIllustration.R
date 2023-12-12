## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(predRupdate)

## ----echo = FALSE-------------------------------------------------------------
coefs_table <- data.frame("Coefficient" = c(-3.995, 
                                            0.72918, 
                                            0.06249, 
                                            1.67003, 
                                            0.75348, 
                                            0.47859))
row.names(coefs_table) <- c("(Intercept)", "Age spline1" , "Age spline2", "Age spline3", "Age spline4", "SexM")
knitr::kable(coefs_table, caption = "Table of coefficients for the existing logistic regression prediction model")

## -----------------------------------------------------------------------------
# create a data.frame of the model coefficients, with columns being variables
coefs_table <- data.frame("Intercept" = -3.995, #the intercept needs to be named exactly as given here
                          "Age_spline1" = 0.72918,
                          "Age_spline2" = 0.06249, 
                          "Age_spline3" = 1.67003,
                          "Age_spline4" = 0.75348,
                          "SexM" = 0.47859)

#pass this into pred_input_info()
Existing_Logistic_Model <- pred_input_info(model_type = "logistic",
                                           model_info = coefs_table)
summary(Existing_Logistic_Model)

## -----------------------------------------------------------------------------
Age_spline <- splines::bs(SYNPM$ValidationData$Age,
                          knots = c(50.09),
                          Boundary.knots = c(36, 64))

head(Age_spline)

## -----------------------------------------------------------------------------
ValidationData <- SYNPM$ValidationData
ValidationData$Age_spline1 <- Age_spline[,1]
ValidationData$Age_spline2 <- Age_spline[,2]
ValidationData$Age_spline3 <- Age_spline[,3]
ValidationData$Age_spline4 <- Age_spline[,4]

## -----------------------------------------------------------------------------
validation_results <- pred_validate(x = Existing_Logistic_Model,
                                    new_data = ValidationData,
                                    binary_outcome = "Y")
summary(validation_results) #use summary() to obtain a tidy output summary of the model performance

## ----fig.height=6, fig.width=6------------------------------------------------
validation_results$flex_calibrationplot

