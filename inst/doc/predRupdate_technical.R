## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(predRupdate)

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(data.frame("Phase" = c("(i) Model Development and internal validation", "(ii) External validation", "(iii) Impact Assessment"),
                        "Description" = c("The development of the model uses a cohort of patients, where the predictor variables and outcomes are known, to perform variable selection and estimate the prognostic effect (coefficient) of each predictor on the outcome of interest. This is followed by assessment of predictive performance within that data, adjusting for in-sample optimism", "Aims to assess the predictive performance of the developed model in data that are different to those used to develop the model (e.g., temporally or geographically different). If performance is deemed unsuitable, the model may undergo updating to tailor it more closely to the new data", "Impact assessment investigates the extent to which the model changes clinical practice and improves patient outcomes")), caption = "Phases of clinical prediction model production")

