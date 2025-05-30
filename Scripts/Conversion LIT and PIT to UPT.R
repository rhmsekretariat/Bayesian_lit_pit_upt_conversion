#Bayesian model for compositional data

#1) instal package----

library (readxl)
library(tidyr)
library(dplyr)
library(bayesplot)
library (ggplot2)
library(brms)
library (broom)
library (compositions)
library (loo)
library(dunn.test)
library (extrafont)
library (gridExtra)

#2. Prepare data set----

#Load the primary data
comparison <- read_excel("LIT UPT PIT.xlsx")

#prepare the data for analysis 
comparison2 <- select (comparison, Location, Year, Site, Longitude, Latitude, Method, HC, DC, DCA, SC, SP, MA, OT, R,S, SI, RK)
str(comparison2)

#make the data set to be 1 in total, so divided by 100
comparison4 <- cbind(Location = comparison$Location, Site = comparison$Site, Longitude = comparison$Longitude,
                     Latitude = comparison$Latitude, Method=comparison$Method, comparison2[,7:17]/100)
head(comparison4)

#prepare data set for Bayesian Model by adding 0.000001
comparison5 <- comparison4
comparison5[,6:16] <- comparison5[,6:16] + 0.000001
comparison5[,6:16] <- comparison5[,6:16]/rowSums(comparison5[,6:16])
str (comparison5)

#make wider version
wide_comparison_3 <- comparison5 %>%
  pivot_wider(names_from = Method, values_from = c(HC:RK)) %>%
  rename_with(~ gsub(" ", "_", .), everything())

#checking the data
head (wide_comparison_3)
str(wide_comparison_3)

#3. Test assumption if the methods have a significant effect on the benthic categories as a composition----
#3.1.Fit delta (with method)----
fit_delta <- brm(
  formula = cbind(HC, DC, DCA, SC, SP, MA, OT, R, S, SI, RK) ~ Method - 1 + 
    (1 | Location/Site), 
  data = comparison5, 
  family = "dirichlet",
  cores = 4,
  control = list(adapt_delta = 0.95) 
)
summary (fit_delta)

#3.1.a Visualization of Trial delta (with method)----
extrafont::loadfonts(device = "win")

# Define the data frame (extract the values from the model fit_delta)
df <- data.frame(
  Method = rep(c("LIT", "PIT", "UPT"), times = 10),
  Category = rep(c("DC", "DCA", "SC", "SP", "MA", "OT", "R", "S", "SI", "RK"), each = 3),
  Estimate = c(-4.81, -4.64, -3.75, -0.07, 0.23, 0.52, -3.75, -3.37, -2.87,
               -3.65, -3.45, -2.86, -4.26, -3.85, -3.58, -3.25, -3.75, -2.48,
               -2.84, -2.59, -1.84, -2.69, -2.17, -1.66, -4.87, -4.71, -4.30,
               -4.88, -4.70, -4.40),
  SE = c(0.36, 0.36, 0.36, 0.27, 0.27, 0.27, 0.66, 0.66, 0.66,
         0.58, 0.58, 0.59, 0.51, 0.52, 0.52, 0.54, 0.54, 0.54,
         1.11, 1.11, 1.11, 0.49, 0.49, 0.49, 0.20, 0.20, 0.20,
         0.22, 0.22, 0.22)
)

# Transformation
df$Percentage <- plogis(df$Estimate) * 100
df$Percentage_Error <- (plogis(df$Estimate + df$SE) - plogis(df$Estimate)) * 100
df$Lower <- pmax(df$Percentage - df$Percentage_Error, 0)
df$Upper <- df$Percentage + df$Percentage_Error
df$Category <- factor(df$Category, levels = c("DC", "DCA", "SC", "SP", "MA", "OT", "R", "S", "SI", "RK"))

# Define shape palette for B/W
shape_palette <- c("LIT" = 16, "PIT" = 17, "UPT" = 15)  # circle, triangle, square

# Final plot
ggplot(df, aes(x = Category, y = Percentage, shape = Method)) +
  geom_point(position = position_dodge(0.6), size = 3.0, color = "black") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(0.6),
                width = 0.4,
                color = "black",
                linewidth = 0.5) +
  scale_shape_manual(values = shape_palette) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 70)) +
  labs(x = "Category", y = "The mean percentage (%)") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Arial"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.line = element_line(size = 0.7, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
  )

#3.2.Fit delta (without method)----
fit_delta.nomethod <- brm(
  formula = cbind(HC, DC, DCA, SC, SP, MA, OT, R, S, SI, RK) ~  1 + 
    (1 | Location/Site), 
  data = comparison5, 
  family = "dirichlet",
  cores = 4,
  control = list(adapt_delta = 0.95) 
)
summary (fit_delta.nomethod)


#3.3. comparison between two models (with method vs no method)----

#the models for comaparison
fit_delta
fit_delta.nomethod

#fit_delta vs fit_delta_nomethod
loo_fit_delta <- loo(fit_delta)
loo_fit_delta_nomethod <- loo(fit_delta.nomethod)
loo_compare(loo_fit_delta, loo_fit_delta_nomethod)

#4. Bayesian model where response and predictor are compositions----
#4.1 Bayesian Model UPT VS LIT (no random)----

fitUPT_LIT <- brm(
  cbind(HC_UPT, DC_UPT, DCA_UPT, SP_UPT, SC_UPT, MA_UPT, OT_UPT, R_UPT, S_UPT, SI_UPT, RK_UPT) ~
    HC_LIT + DC_LIT + DCA_LIT + SP_LIT + SC_LIT + MA_LIT + OT_LIT + R_LIT + S_LIT + SI_LIT + RK_LIT - 1,
  data = wide_comparison_3,
  family = "dirichlet",
  cores = 4,
  iter = 10000
)

summary (fitUPT_LIT)

# Save the LIT-to-UPT model
saveRDS(fitUPT_LIT, file = "fitUPT_LIT.rds")


# Step 1: Select LIT columns 
lit_data <- wide_comparison_3 %>%
  select(HC_LIT, DC_LIT, DCA_LIT, SP_LIT, SC_LIT, MA_LIT, OT_LIT, R_LIT, S_LIT, SI_LIT, RK_LIT)

# Step 2: Build functions from model 
categoryFunction <- function(fit, lower, upper) {
  coefs <- fixef(fit)[lower:upper]
  nms <- rownames(summary(fit)$fixed)[lower:upper]
  nms2 <- strsplit(nms, "_")
  var_names <- unlist(lapply(nms2, function(x) x[2]))
  args <- paste(var_names, collapse = ", ")
  rhs <- paste(paste(coefs, var_names, sep = "*"), collapse = " + ")
  eval(parse(text = paste0("function(", args, ") { return(", rhs, ") }")))
}

lowers_LIT <- seq(1, 110, by = 11)
functions_LIT <- sapply(lowers_LIT, function(x) categoryFunction(fitUPT_LIT, x, x + 10))

# Step 3: Apply the functions to each row 
apply_functions_LIT <- function(row, functions) {
  names(row) <- gsub("_LIT", "", names(row))
  preds <- sapply(functions, function(f) do.call(f, as.list(row)))
  preds <- c(0, preds)
  exp(preds) / sum(exp(preds))
}

predicted_upt_lit <- t(apply(lit_data, 1, function(row) apply_functions_LIT(row, functions_LIT)))
predicted_upt_lit_df <- as.data.frame(predicted_upt_lit)

# Step 4: Add Site column and rename 
predicted_upt_lit_df <- cbind(Site = wide_comparison_3$Site, predicted_upt_lit_df)
colnames(predicted_upt_lit_df)[-1] <- c("HC_UPT_pred", "DC_UPT_pred", "DCA_UPT_pred",
                                        "SP_UPT_pred", "SC_UPT_pred", "MA_UPT_pred",
                                        "OT_UPT_pred", "R_UPT_pred", "S_UPT_pred",
                                        "SI_UPT_pred", "RK_UPT_pred")

# Step 5: Combine with actual UPT + LIT 
actual_upt_lit <- wide_comparison_3 %>%
  select(Site, matches("UPT|LIT")) %>%
  rename_with(~ paste0(., "_actual"), -Site)

comparison_df_lit <- actual_upt_lit %>%
  left_join(predicted_upt_lit_df, by = "Site")

# Step 6: Multiply proportions by 100 
comparison_df_lit <- comparison_df_lit %>%
  mutate(across(ends_with('_actual') | ends_with('_pred'), ~ . * 100))

# Step 7: Rename prediction columns 
colnames(comparison_df_lit) <- sub("UPT_pred", "pred", colnames(comparison_df_lit))

# Step 8: Reshape to long format 
comparison_long_lit <- comparison_df_lit %>%
  pivot_longer(cols = -Site,
               names_to = c("Category", "Method"),
               names_sep = "_",
               values_to = "Value")

# Step 9: Kruskal-Wallis test 
kruskal_test_results_lit <- comparison_long_lit %>%
  group_by(Category) %>%
  summarise(p_value = kruskal.test(Value ~ Method)$p.value)

# Step 10: Dunn post-hoc test
posthoc_results_lit <- comparison_long_lit %>%
  group_by(Category) %>%
  do({
    dunn_test <- dunn.test(.$Value, .$Method, method = "bonferroni")
    data.frame(Category = unique(.$Category),
               Comparison = dunn_test$comparisons,
               P_adjusted = dunn_test$P.adjusted)
  })

# Step 11: Generate R2 plots 
categories <- c("HC", "DC", "DCA", "SC", "SP", "MA", "OT", "R", "S", "SI", "RK")

plot_with_r2 <- function(data, category) {
  actual_col <- paste0(category, "_UPT_actual")
  pred_col <- paste0(category, "_pred")
  
  model <- lm(as.formula(paste(pred_col, "~", actual_col)), data = data)
  r2 <- summary(model)$r.squared
  
  ggplot(data, aes(x = .data[[actual_col]], y = .data[[pred_col]])) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(
      title = paste(category, "(R² =", round(r2, 2), ")"),
      x = "Actual (%)",
      y = "Predicted (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    )
}

plots_lit <- lapply(categories, function(category) plot_with_r2(comparison_df_lit, category))
do.call(grid.arrange, c(plots_lit, ncol = 3))

#4.2 Bayesian Model UPT VS PIT (no random)----

fitUPT_PIT <- brm(
  cbind(HC_UPT, DC_UPT, DCA_UPT, SP_UPT, SC_UPT, MA_UPT, OT_UPT, R_UPT, S_UPT, SI_UPT, RK_UPT) ~
    HC_PIT + DC_PIT + DCA_PIT + SP_PIT + SC_PIT + MA_PIT + OT_PIT + R_PIT + S_PIT + SI_PIT + RK_PIT - 1,
  data = wide_comparison_3,
  family = "dirichlet",
  cores = 4,
  iter = 10000
)

summary (fitUPT_PIT)

# Save the PIT-to-UPT model
saveRDS(fitUPT_PIT, file = "fitUPT_PIT.rds")

# Step 1: Select PIT columns 
pit_data <- wide_comparison_3 %>%
  select(HC_PIT, DC_PIT, DCA_PIT, SP_PIT, SC_PIT, MA_PIT, OT_PIT, R_PIT, S_PIT, SI_PIT, RK_PIT)

# Step 2: Build functions from model
categoryFunction_PIT <- function(fit, lower, upper) {
  coefs <- fixef(fit)[lower:upper]
  nms <- rownames(summary(fit)$fixed)[lower:upper]
  nms2 <- strsplit(nms, "_")
  var_names <- unlist(lapply(nms2, function(x) x[2]))
  args <- paste(var_names, collapse = ", ")
  rhs <- paste(paste(coefs, var_names, sep = "*"), collapse = " + ")
  eval(parse(text = paste0("function(", args, ") { return(", rhs, ") }")))
}

lowers_PIT <- seq(1, 110, by = 11)
functions_PIT <- sapply(lowers_PIT, function(x) categoryFunction_PIT(fitUPT_PIT, x, x + 10))

# Step 3: Apply the functions to each row 
apply_functions_PIT <- function(row, functions) {
  names(row) <- gsub("_PIT", "", names(row))
  preds <- sapply(functions, function(f) do.call(f, as.list(row)))
  preds <- c(0, preds)
  exp(preds) / sum(exp(preds))
}

predicted_upt_pit <- t(apply(pit_data, 1, function(row) apply_functions_PIT(row, functions_PIT)))
predicted_upt_pit_df <- as.data.frame(predicted_upt_pit)

# Step 4: Add Site column and rename 
predicted_upt_pit_df <- cbind(Site = wide_comparison_3$Site, predicted_upt_pit_df)
colnames(predicted_upt_pit_df)[-1] <- c("HC_UPT_pred", "DC_UPT_pred", "DCA_UPT_pred",
                                        "SP_UPT_pred", "SC_UPT_pred", "MA_UPT_pred",
                                        "OT_UPT_pred", "R_UPT_pred", "S_UPT_pred",
                                        "SI_UPT_pred", "RK_UPT_pred")

# Step 5: Combine with actual UPT + PIT 
actual_upt_pit <- wide_comparison_3 %>%
  select(Site, matches("UPT|PIT")) %>%
  rename_with(~ paste0(., "_actual"), -Site)

comparison_df_pit <- actual_upt_pit %>%
  left_join(predicted_upt_pit_df, by = "Site")

# Step 6: Multiply proportions by 100 
comparison_df_pit <- comparison_df_pit %>%
  mutate(across(ends_with('_actual') | ends_with('_pred'), ~ . * 100))

# Step 7: Rename prediction columns
colnames(comparison_df_pit) <- sub("UPT_pred", "pred", colnames(comparison_df_pit))

# Step 8: Reshape to long format 
comparison_long_pit <- comparison_df_pit %>%
  pivot_longer(cols = -Site,
               names_to = c("Category", "Method"),
               names_sep = "_",
               values_to = "Value")

# Step 9: Kruskal-Wallis test
kruskal_test_results_pit <- comparison_long_pit %>%
  group_by(Category) %>%
  summarise(p_value = kruskal.test(Value ~ Method)$p.value)

# Step 10: Dunn post-hoc test 
posthoc_results_pit <- comparison_long_pit %>%
  group_by(Category) %>%
  do({
    dunn_test <- dunn.test(.$Value, .$Method, method = "bonferroni")
    data.frame(Category = unique(.$Category),
               Comparison = dunn_test$comparisons,
               P_adjusted = dunn_test$P.adjusted)
  })

# Step 11: Generate R2 plots 
plots_pit <- lapply(categories, function(category) plot_with_r2(comparison_df_pit, category))
do.call(grid.arrange, c(plots_pit, ncol = 3))





