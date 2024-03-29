# Internal functions for pred_validate.predinfo_logistic() ---------------------
validate_logistic <- function(ObservedOutcome,
                              Prob,
                              LP,
                              cal_plot,
                              xlab = "Predicted Probability",
                              ylab = "Observed Probability",
                              xlim = c(0,1),
                              ylim = c(0,1),
                              pred_rug = TRUE) {

  # Test for 0 and 1 probabilities
  n_inf <- sum(is.infinite(LP))
  if (n_inf > 0) {
    id <- which(is.infinite(LP))
    ObservedOutcome <- ObservedOutcome[-id]
    LP <- LP[-id]
    Prob <- Prob[-id]
    warning(paste(n_inf,
                  'observations deleted due to predicted risks being 0 and 1'))
  }

  #Estimate observed:expected ratio
  OE_ratio <- mean(ObservedOutcome) / mean(Prob)
  OE_ratio_SE <- sqrt(1 / sum(ObservedOutcome))

  #Estimate calibration intercept
  CITL_mod <- stats::glm(ObservedOutcome ~ 1,
                         family = stats::binomial(link = "logit"),
                         offset = LP)
  CalInt <- as.numeric(stats::coef(CITL_mod)[1])
  CalIntSE <- sqrt(stats::vcov(CITL_mod)[1,1])


  #Estimate calibration slope
  CalSlope_mod <- stats::glm(ObservedOutcome ~ LP,
                             family = stats::binomial(link = "logit"))
  CalSlope <- as.numeric(stats::coef(CalSlope_mod)[2])
  CalSlopeSE <- sqrt(stats::vcov(CalSlope_mod)[2,2])


  #Discrimination
  roc_curve <- pROC::roc(response = ObservedOutcome,
                         predictor = Prob,
                         direction = "<",
                         levels = c(0,1),
                         ci = TRUE)
  AUC <- as.numeric(roc_curve$auc)
  AUCSE <- sqrt(pROC::var(roc_curve))


  #R-squared metrics
  R2_mod <- stats::glm(ObservedOutcome ~ -1,
                       family = stats::binomial(link = "logit"),
                       offset = LP)
  E <- sum(ObservedOutcome) #number of events in the validation data
  N <- length(ObservedOutcome) #number of observations in the validation data
  L_Null <- (E*log(E/N)) + ((N-E)*log(1 - (E/N)))
  LR <- -2 * (L_Null - as.numeric(stats::logLik(R2_mod)))
  MaxR2 <- 1 - exp((2*L_Null) / length(ObservedOutcome))
  R2_coxsnell <- 1 - exp(-LR / length(ObservedOutcome))
  R2_Nagelkerke <- R2_coxsnell / MaxR2


  #Brier Score
  BrierScore <- 1/N * (sum((Prob - ObservedOutcome)^2))

  #Distribution of predicted risks:
  plot_df <- data.frame("Prob" = Prob,
                        "Outcome" = factor(ifelse(ObservedOutcome == 1,
                                                  "Event",
                                                  "No Event")))
  PR_dist <- ggplot2::ggplot(plot_df,
                             ggplot2::aes(x = .data$Outcome,
                                          y = .data$Prob)) +
    ggplot2::geom_violin(position = ggplot2::position_dodge(width = .75),
                         linewidth = 1) +
    ggplot2::geom_boxplot(width = 0.1,
                          outlier.shape = NA) +
    ggplot2::ylab(xlab) +
    ggplot2::theme_bw(base_size = 12)

  # Create flexible calibration plot:
  if (cal_plot == TRUE){
    if(length(unique(Prob)) <= 10) { #allows handling of intercept-only models
      stop("Very low unique predicted risks - calplot not possible; call again with cal_plot = FALSE")
    } else{
      flex_calibrationplot <- flex_calplot(model_type = "logistic",
                                           ObservedOutcome = ObservedOutcome,
                                           Prob = Prob,
                                           LP = LP,
                                           xlim = xlim,
                                           ylim = ylim,
                                           xlab = xlab,
                                           ylab = ylab,
                                           pred_rug = pred_rug)
    }
  } else {
    flex_calibrationplot <- NULL
  }

  #Return results
  out <- list("OE_ratio" = OE_ratio,
              "OE_ratio_SE" = OE_ratio_SE,
              "CalInt" = CalInt,
              "CalInt_SE" = CalIntSE,
              "CalSlope" = CalSlope,
              "CalSlope_SE" = CalSlopeSE,
              "AUC" = AUC,
              "AUC_SE" = AUCSE,
              "R2_CoxSnell" = R2_coxsnell,
              "R2_Nagelkerke" = R2_Nagelkerke,
              "BrierScore" = BrierScore,
              "PR_dist" = PR_dist,
              "flex_calibrationplot" = flex_calibrationplot)
  return(out)
}
