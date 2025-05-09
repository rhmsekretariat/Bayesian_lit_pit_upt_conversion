#Bayesian model for compositional data
#No need to run the model again, just load the RDS and apply the coefficients.

#1) Install required packages----

library(readxl)
library(tidyr)
library(dplyr)
library(brms)
library(openxlsx)


#2) Prepare your wide format data with LIT or PIT values----
# input_data_LIT <-read_excel ("Create your data to be like this: Site, HC_LIT, DC_LIT,DCA_LIT, SP_LIT, SC_LIT, MA_LIT, OT_LIT, R_LIT, S_LIT, SI_LIT and RK_LIT")
# input_data_PIT <-read_excel ("Create your data to be like this: Site, HC_PIT, DC_PIT,DCA_PIT, SP_PIT, SC_PIT, MA_PIT, OT_PIT, R_PIT, S_PIT, SI_PIT and RK_PIT")

#Convert percent to proportions and normalize the rows to sum = 1 (for LIT)
input_data_LIT2 <- input_data_LIT
input_data_LIT2[, 2:12] <- input_data_LIT2[, 2:12] / 100
input_data_LIT2[, 2:12] <- input_data_LIT2[, 2:12] + 0.000001
input_data_LIT2[, 2:12] <- input_data_LIT2[, 2:12] / rowSums(input_data_LIT2[, 2:12])


# Convert percent to proportions and normalize the rows to sum = 1 (for PIT)
input_data_PIT2 <- input_data_PIT
input_data_PIT2[, 2:12] <- input_data_PIT2[, 2:12] / 100
input_data_PIT2[, 2:12] <- input_data_PIT2[, 2:12] + 0.000001
input_data_PIT2[, 2:12] <- input_data_PIT2[, 2:12] / rowSums(input_data_PIT2[, 2:12])


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
lit_data <- input_data_LIT2 %>% select(HC_LIT, DC_LIT, DCA_LIT, SP_LIT, SC_LIT, MA_LIT, OT_LIT, R_LIT, S_LIT, SI_LIT, RK_LIT)
lowers_LIT <- seq(1, 110, by = 11)
functions_LIT <- sapply(lowers_LIT, function(x) categoryFunction(fitUPT_LIT, x, x + 10))

# For PIT:
pit_data <- input_data_PIT2 %>% select(HC_PIT, DC_PIT, DCA_PIT, SP_PIT, SC_PIT, MA_PIT, OT_PIT, R_PIT, S_PIT, SI_PIT, RK_PIT)
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

# Convert from proportions to percentages
predicted_upt <- predicted_upt * 100

predicted_upt_df <- as.data.frame(predicted_upt)
colnames(predicted_upt_df) <- c("HC_UPT", "DC_UPT", "DCA_UPT", "SP_UPT", "SC_UPT", "MA_UPT", "OT_UPT", "R_UPT", "S_UPT", "SI_UPT", "RK_UPT")

str (predicted_upt_df)

#8) Bind with site or location metadata (optional)----
predicted_upt_df <- cbind(Site = input_data_LIT$Site, predicted_upt_df)

#predicted_upt_df <- cbind(Site = input_data_PIT$Site, predicted_upt_df)

#9) Export the results----
write.xlsx(predicted_upt_df, "UPT_predictions.xlsx")


