## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(predRupdate)

## ---- echo = FALSE------------------------------------------------------------
coefs_table <- as.data.frame(round(t(SYNPM$Existing_logistic_models[1,which(!is.na(SYNPM$Existing_logistic_models[1,]))]), 3))
names(coefs_table) <- c("Coefficient")
knitr::kable(coefs_table, caption = "Table of coefficients for the existing logistic regression prediction model")

## -----------------------------------------------------------------------------
# create a data.frame of the model coefficients, with columns being variables
coefs_table <- data.frame("Intercept" = -3.995, #the intercept needs to be named exactly as given here
                          "Age" = 0.012,
                          "SexM" = 0.267, 
                          "Smoking_Status" = 0.751,
                          "Diabetes" = 0.523,
                          "Creatine" = 0.578)

#pass this into pred_input_info()
Existing_Logistic_Model <- pred_input_info(model_type = "logistic",
                                           model_info = coefs_table)
summary(Existing_Logistic_Model)

## -----------------------------------------------------------------------------
validation_results <- pred_validate(x = Existing_Logistic_Model,
                                    new_data = SYNPM$ValidationData,
                                    binary_outcome = "Y")
summary(validation_results) #use summary() to obtain a tidy output summary of the model performance

## ---- echo = FALSE------------------------------------------------------------
coefs_table <- as.data.frame(round(t(SYNPM$Existing_TTE_models[1,which(!is.na(SYNPM$Existing_TTE_models[1,]))]), 3))
names(coefs_table) <- c("Coefficient")
knitr::kable(coefs_table, caption = "Table of coefficients for the existing time-to-event regression prediction model")

## ---- echo = FALSE------------------------------------------------------------
BH_table <- SYNPM$TTE_mod1_baseline
knitr::kable(BH_table, caption = "Table of baseline cumulative hazard")

## -----------------------------------------------------------------------------
# create a data.frame of the model coefficients, with columns being variables
coefs_table <- data.frame("Age" = 0.007,
                          "SexM" = 0.225,
                          "Smoking_Status" = 0.685,
                          "Diabetes" = 0.425,
                          "Creatine" = 0.587)

#pass this into pred_input_info()
Existing_TTE_Model <- pred_input_info(model_type = "survival",
                                      model_info = coefs_table,
                                      cum_hazard = BH_table) #where BH_table is the baseline hazard above

#now validate against the time-to-event outcomes in the new dataset:
validation_results <- pred_validate(x = Existing_TTE_Model,
                                    new_data = SYNPM$ValidationData,
                                    survival_time = "ETime",
                                    event_indicator = "Status",
                                    time_horizon = 5)
summary(validation_results)

## -----------------------------------------------------------------------------
# create a data.frame of the model coefficients, with columns being variables
coefs_table <- data.frame("Age" = 0.007,
                          "SexM" = 0.225,
                          "Smoking_Status" = 0.685,
                          "Diabetes" = 0.425,
                          "Creatine" = 0.587)

#pass this into pred_input_info()
Existing_TTE_Model <- pred_input_info(model_type = "survival",
                                      model_info = coefs_table,
                                      cum_hazard = NULL) #leave as NULL if the baseline not available

#now validate against the time-to-event outcomes in the new dataset:
validation_results <- pred_validate(x = Existing_TTE_Model,
                                    new_data = SYNPM$ValidationData,
                                    survival_time = "ETime",
                                    event_indicator = "Status",
                                    time_horizon = 5)
summary(validation_results)

## -----------------------------------------------------------------------------
# create a data.frame of the model coefficients, with columns being variables
coefs_table <- data.frame("Intercept" = -3.995, 
                          "Age" = 0.012,
                          "SexM" = 0.267, 
                          "Smoking_Status" = 0.751,
                          "Diabetes" = 0.523,
                          "Creatine" = 0.578)

#pass this into pred_input_info()
Existing_Logistic_Model <- pred_input_info(model_type = "logistic",
                                           model_info = coefs_table)

#apply the pred_update function to update the model to the new dataset:
Updated_model <- pred_update(Existing_Logistic_Model,
                             update_type = "recalibration",
                             new_data = SYNPM$ValidationData,
                             binary_outcome = "Y")

summary(Updated_model)

## -----------------------------------------------------------------------------
summary(pred_validate(Updated_model, 
                      new_data = SYNPM$ValidationData, 
                      binary_outcome = "Y"))

## -----------------------------------------------------------------------------
coefs_table <- data.frame(rbind(c("Intercept" = -3.995,
                                  "Age" = 0.012,
                                  "SexM" = 0.267,
                                  "Smoking_Status" = 0.751,
                                  "Diabetes" = 0.523,
                                  "Creatine" = 0.578),
                                c("Intercept" = -2.282,
                                  "Age" = NA,
                                  "SexM" = 0.223,
                                  "Smoking_Status" = 0.528,
                                  "Diabetes" = 0.200,
                                  "Creatine" = 0.434),
                                c("Intercept" = -3.013,
                                  "Age" = NA,
                                  "SexM" = NA,
                                  "Smoking_Status" = 0.565,
                                  "Diabetes" = -0.122,
                                  "Creatine" = 0.731)))
multiple_mods <- pred_input_info(model_type = "logistic",
                                 model_info = coefs_table)
summary(multiple_mods)

## -----------------------------------------------------------------------------
SR <- pred_stacked_regression(x = multiple_mods,
                              new_data = SYNPM$ValidationData,
                              binary_outcome = "Y")
summary(SR)

## -----------------------------------------------------------------------------
summary(pred_validate(SR, 
              new_data = SYNPM$ValidationData, 
              binary_outcome = "Y"))

## -----------------------------------------------------------------------------
# create a data.frame of the model coefficients, with columns being variables
coefs_table <- data.frame("Intercept" = -3.995, 
                          "Smoking" = 0.751)

#pass this into pred_input_info()
Existing_Logistic_Model <- pred_input_info(model_type = "logistic",
                                           model_info = coefs_table)
try(pred_predict(Existing_Logistic_Model, 
                 new_data = SYNPM$ValidationData))

names(SYNPM$ValidationData)

## -----------------------------------------------------------------------------
new_df <- data.frame("Sex" = as.factor(c("M", "F", "M", "M", "F", "F", "M")),
                     "Smoking_Status" = c(1, 0, 0, 1, 1, 0, 1))

## -----------------------------------------------------------------------------
coefs_table <- data.frame("Intercept" = -3.4,
                          "Sex_M" = 0.306,
                          "Smoking_Status" = 0.628)
existing_Logistic_Model <- pred_input_info(model_type = "logistic",
                                           model_info = coefs_table)

#if we try to use functions within predRupdate using new_df it will give an error as Sex is a factor variable:
try(pred_predict(existing_Logistic_Model, 
                 new_data = new_df))

#we must first turn into dummy variables:
new_df_indicatorvars <- dummy_vars(new_df)
head(new_df_indicatorvars)

#and then pass to functions within predRupdate; e.g.:
pred_predict(existing_Logistic_Model, 
             new_data = new_df_indicatorvars)

