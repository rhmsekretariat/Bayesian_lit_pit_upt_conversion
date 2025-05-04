#Bayesian model for compositional data
#No need to run the model again, just load the RDS and apply the coefficients.

#1) Install required packages----

library(readxl)
library(tidyr)
library(dplyr)
library(brms)
library(openxlsx)


#2) Prepare your wide format data with LIT or PIT values----
# Your data should contain columns like HC_LIT, DC_LIT, ..., RK_LIT or HC_PIT, DC_PIT, ..., RK_PIT

#3) Load the pre-trained model----
# Change file path accordingly if the model is in another folder
fitUPT_LIT <- readRDS("fitUPT_LIT.rds")
# OR
fitUPT_PIT <- readRDS("fitUPT_PIT.rds")

#4) Define the function builder (do not change)----
categoryFunction <- function(fit, lower, upper) {
  coefs <- fixef(fit)[lower:upper]
  nms <- rownames(summary(fit)$fixed)[lower:upper]
  nms2 <- strsplit(nms, "_")
  var_names <- unlist(lapply(nms2, function(x) x[2]))
  args <- paste(var_names, collapse = ", ")
  rhs <- paste(paste(coefs, var_names, sep = "*"), collapse = " + ")
  eval(parse(text = paste0("function(", args, ") { return(", rhs, ") }")))
}

#5) Build conversion functions (choose based on your input type: LIT or PIT)----
# For LIT:
lit_data <- input_data %>% select(HC_LIT, DC_LIT, DCA_LIT, SP_LIT, SC_LIT, MA_LIT, OT_LIT, R_LIT, S_LIT, SI_LIT, RK_LIT)
lowers_LIT <- seq(1, 110, by = 11)
functions_LIT <- sapply(lowers_LIT, function(x) categoryFunction(fitUPT_LIT, x, x + 10))

# For PIT:
pit_data <- input_data %>% select(HC_PIT, DC_PIT, DCA_PIT, SP_PIT, SC_PIT, MA_PIT, OT_PIT, R_PIT, S_PIT, SI_PIT, RK_PIT)
lowers_PIT <- seq(1, 110, by = 11)
functions_PIT <- sapply(lowers_PIT, function(x) categoryFunction(fitUPT_PIT, x, x + 10))

#6) Define prediction function (do not change)----
apply_functions <- function(row, functions, prefix) {
  names(row) <- gsub(prefix, "", names(row))
  preds <- sapply(functions, function(f) do.call(f, as.list(row)))
  preds <- c(0, preds)
  exp(preds) / sum(exp(preds))
}

#7) Predict UPT from LIT or PIT----
# Choose which one based on your input:
# For LIT:
predicted_upt <- t(apply(lit_data, 1, function(row) apply_functions(row, functions_LIT, "_LIT")))

# For PIT:
#predicted_upt <- t(apply(pit_data, 1, function(row) apply_functions(row, functions_PIT, "_PIT")))

predicted_upt_df <- as.data.frame(predicted_upt)
colnames(predicted_upt_df) <- c("HC_UPT", "DC_UPT", "DCA_UPT", "SP_UPT", "SC_UPT", "MA_UPT", "OT_UPT", "R_UPT", "S_UPT", "SI_UPT", "RK_UPT")

#8) Bind with site or location metadata (optional)----
predicted_upt_df <- cbind(Site = input_data$Site, predicted_upt_df)

#9) Export the results----
write.xlsx(predicted_upt_df, "UPT_predictions.xlsx")


