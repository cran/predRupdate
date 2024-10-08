#' Validate Predicted Probabilities
#'
#' This function is included for situations where one has a vector of predicted
#' probabilities from a model and a vector of observed binary outcomes that we
#' wish to validate the predictions against. See \code{\link{pred_validate}} for
#' the main validation function of this package.
#'
#' @param binary_outcome vector of binary outcomes (coded as 1 if outcome
#'   happened, and 0 otherwise). Must be of same length as Prob
#' @param Prob vector of predicted probabilities. Must be of same length of
#'   binary_outcome.
#' @param cal_plot indicate if a flexible calibration plot should be produced
#'   (TRUE) or not (FALSE).
#' @param level the confidence level required for all performance metrics.
#'   Defaults at 95%. Must be a value between 0 and 1.
#' @param ... further plotting arguments for the calibration plot. See Details
#'   below.
#'
#' @details This function takes a vector of observed binary outcomes, and a
#'   corresponding vector of predicted risks (e.g. from a logistic regression
#'   CPM), and calculates measures of predictive performance. The function is
#'   intended as a standalone way of validating predicted risks against binary
#'   outcomes outside of the usual pred_input_info() -> pred_validate() package
#'   workflow. See \code{\link{pred_validate}} for the main validation function
#'   of this package.
#'
#'   Various metrics of calibration (agreement between the observed risk and the
#'   predicted risks, across the full risk range) and discrimination (ability
#'   of the model to distinguish between those who develop the outcome and
#'   those who do not) are calculated. For calibration, the observed-to-expected
#'   ratio, calibration intercept and calibration slopes are estimated. The
#'   calibration intercept is estimated by fitting a logistic regression model
#'   to the observed binary outcomes, with the linear predictor of the model as
#'   an offset. For calibration slope, a logistic regression model is fit to the
#'   observed binary outcome with the linear predictor from the model as the
#'   only covariate. For discrimination, the function estimates the area under
#'   the receiver operating characteristic curve (AUC). Various other metrics
#'   are also calculated to assess overall accuracy (Brier score, Cox-Snell R2).
#'
#'   A flexible calibration plot is produced. Specify parameter
#'   \code{cal_plot} to indicate whether a calibration plot should be produced
#'   (TRUE), or not (FALSE). See \code{\link{pred_validate}} for details on
#'   this plot, and details of optional plotting arguments.
#'
#' @return An object of class "\code{predvalidate}", which is a list containing
#'   relevant calibration and discrimination measures. See
#'   \code{\link{pred_validate}} for details.
#'
#' @export
#'
#' @examples
#' # simulate some data for purposes of example illustration
#' set.seed(1234)
#' x1 <- rnorm(2000)
#' LP <- -2 + (0.5*x1)
#' PR <- 1/(1+exp(-LP))
#' y <- rbinom(2000, 1, PR)
#'
#' #fit hypothetical model to the simulated data
#' mod <- glm(y[1:1000] ~ x1[1:1000], family = binomial(link="logit"))
#'
#' #obtain the predicted risks from the model
#' pred_risk <- predict(mod, type = "response",
#'                       newdata = data.frame("x1" = x1[1001:2000]))
#'
#' #Use pred_val_probs to validate the predicted risks against the
#' #observed outcomes
#' summary(pred_val_probs(binary_outcome = y[1001:2000],
#'                         Prob = pred_risk,
#'                         cal_plot = FALSE))
#' @seealso \code{\link{pred_input_info}}, \code{\link{pred_validate}}
pred_val_probs <- function(binary_outcome,
                           Prob,
                           cal_plot = TRUE,
                           level = 0.95,
                           ...) {

  #check binary_outcome is specified correctly
  if(is.factor(binary_outcome) | is.character(binary_outcome)) {
    stop("binary_outcome should be numeric of 0 and 1s",
         call. = FALSE)
  }
  if (all(unique(binary_outcome) %in% c(0,1)) == FALSE) {
    stop("binary_outcome should only contain 0 and 1s",
         call. = FALSE)
  }

  #Check length of binary_outcome and Prob agree
  if (length(binary_outcome) != length(Prob)) {
    stop("length of binary_outcome and length of Prob are not the same",
         call. = FALSE)
  }

  #ensure level is specified correctly
  if (!is.numeric(level)) {
    stop("level specified incorrectly; must be a value between 0 and 1",
         call. = FALSE)
  }
  if (level > 1 |
      level < 0 |
      is.na(level) |
      is.null(level)) {
    stop("level specified incorrectly; must be a value between 0 and 1",
         call. = FALSE)
  }

  #call internal predictive performance function
  LP <- predRupdate::logit(Prob)
  performance <- validate_logistic(ObservedOutcome = binary_outcome,
                                   Prob = Prob,
                                   LP = LP,
                                   level = level,
                                   cal_plot = cal_plot,
                                   ...)
  performance$M <- 1
  #set class as predvalidate to drawn upon S3 methods of that object class
  class(performance) <- c("predvalidate_logistic", "predvalidate")
  performance
}
