test_that("output of pred_validate is as expected - single models", {

  expect_error(pred_validate(x = "test S3 class",
                             new_data = SYNPM$ValidationData,
                             binary_outcome = "Y"))

  model1 <- pred_input_info(model_type = "logistic",
                            model_info = SYNPM$Existing_logistic_models[1,])
  expect_error(pred_validate(x = model1,
                             new_data = SYNPM$ValidationData)) #no outcome given

  val_results <- pred_validate(x = model1,
                               new_data = SYNPM$ValidationData,
                               binary_outcome = "Y")
  expect_s3_class(val_results, c("predvalidate_logistic", "predvalidate"))
  expect_type(val_results, type = "list")
  expect_equal(names(val_results),
               c("level",
                 "OE_ratio", "OE_ratio_lower", "OE_ratio_upper",
                 "CalInt", "CalInt_SE", "CalInt_lower", "CalInt_upper",
                 "CalSlope", "CalSlope_SE", "CalSlope_lower", "CalSlope_upper",
                 "AUC", "AUC_SE", "AUC_lower", "AUC_upper",
                 "R2_CoxSnell", "R2_Nagelkerke",
                 "BrierScore", "Brier_lower", "Brier_upper",
                 "PR_dist", "flex_calibrationplot", "M"))

  expect_no_error(print(val_results))
  expect_no_error(plot(val_results))
  expect_snapshot(summary(val_results))

  #test the error for few unique predicted risks:
  expect_error(pred_validate(x = pred_input_info(model_type = "logistic",
                                                 model_info = data.frame("Intercept" = -3.995,
                                                                         "SexM" = 0.267)),
                             new_data = SYNPM$ValidationData,
                             binary_outcome = "Y"))

})



test_that("output of pred_validate is as expected - multiple models", {

  model2 <- pred_input_info(model_type = "logistic",
                            model_info = SYNPM$Existing_logistic_models)
  val_results <- pred_validate(x = model2,
                               new_data = SYNPM$ValidationData,
                               binary_outcome = "Y",
                               cal_plot = FALSE)

  expect_no_error(print(val_results))
  expect_no_error(summary(val_results))
  expect_no_error(plot(val_results))

  expect_type(val_results, type = "list")
  expect_equal(length(val_results), model2$M + 1)

  expect_s3_class(val_results, c("predvalidate_logistic", "predvalidate"))

  for(m in 1:model2$M) {
    expect_type(val_results[[m]], type = "list")
    expect_equal(names(val_results[[m]]),
                 c("level",
                   "OE_ratio", "OE_ratio_lower", "OE_ratio_upper",
                   "CalInt", "CalInt_SE", "CalInt_lower", "CalInt_upper",
                   "CalSlope", "CalSlope_SE", "CalSlope_lower", "CalSlope_upper",
                   "AUC", "AUC_SE", "AUC_lower", "AUC_upper",
                   "R2_CoxSnell", "R2_Nagelkerke",
                   "BrierScore", "Brier_lower", "Brier_upper",
                   "PR_dist", "flex_calibrationplot"))
  }
})
