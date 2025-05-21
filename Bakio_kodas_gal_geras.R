options(max.print = 100000)
# install packages
install.packages(c("readxl", "readr", "dplyr", "ggplot2", "lubridate",
                   "tseries", "corrplot", "psych", "xts", "zoo", "rugarch"))
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tseries)
library(corrplot)
library(psych)
library(xts)
library(zoo)
library(rugarch)

# Load data
prices <- read_excel("C:/Users/marty/Desktop/Prices test run.xlsx")
atc <- read_excel("C:/Users/marty/Desktop/ATC test run.xlsx")
weather <- read_csv("C:/Users/marty/Desktop/Weather test run.csv")
brell <- read_excel("C:/Users/marty/Desktop/Brell_dummy.xlsx")
gas <- read_excel("C:/Users/marty/Desktop/Gas_Hourly_Test.xlsx")

# clean datetime
prices$datetime <- as.POSIXct(as.character(prices$datetime), format = "%Y-%m-%d %H:%M:%S")
weather$datetime <- gsub("T", " ", weather$datetime)
weather$datetime <- as.POSIXct(weather$datetime, format = "%Y-%m-%d %H:%M:%S")
if (is.numeric(atc$datetime)) {
  atc$datetime <- as.POSIXct((atc$datetime - 25569) * 86400, origin = "1970-01-01", tz = "UTC")
} else {
  atc$datetime <- as.POSIXct(as.character(atc$datetime), format = "%Y-%m-%d %H:%M:%S")
}
brell$datetime <- as.POSIXct(as.character(brell$datetime), format = "%Y-%m-%d %H:%M:%S")
gas$datetime <- as.POSIXct(as.character(gas$datetime), format = "%Y-%m-%d %H:%M:%S")  

# merge data
data <- prices %>%
  left_join(atc, by = "datetime") %>%
  left_join(weather, by = "datetime") %>%
  left_join(brell, by = "datetime") %>%
  left_join(gas, by = "datetime")

# define variables
price_col <- "Price (EUR/MWh)"
gas_col <- "lpg_price"
atc_vars <- c("ATC_SE_LT", "ATC_PL_LT", "ATC_LV_LT", "ATC_LT_LV", "ATC_LT_SE", "ATC_LT_PL")
weather_vars <- c("temperature", "wind", "cloud_cover")
numeric_cols <- c(price_col, gas_col, atc_vars, weather_vars)
dummy_var <- "dummy_value"
all_vars <- c(weather_vars, atc_vars, gas_col, dummy_var)

# Clean data
data <- data %>%
  select(datetime, all_of(numeric_cols), dummy_value) %>%
  na.omit()

data <- data %>% filter(.data[[price_col]] > 0)

# log returns
data <- data %>%
  arrange(datetime) %>%
  mutate(price_return = c(NA, diff(log(.data[[price_col]])))) %>%
  na.omit()

# save new dateset
write.csv(data, "merged_cleaned_data_with_gas.csv", row.names = FALSE)

# Convert to xts
price_xts <- xts(data$price_return, order.by = data$datetime)

# Price and returns
ggplot(data, aes(x = datetime, y = .data[[price_col]])) +
  geom_line(color = "blue") +
  labs(title = "Electricity Price Over Time", x = "Date", y = "EUR/MWh") +
  theme_minimal()

brell_split <- as.POSIXct("2025-02-08")

ggplot(data, aes(x = datetime, y = price_return)) +
  geom_line(color = "red") +
  geom_vline(xintercept = as.numeric(brell_split), linetype = "dashed", color = "blue") +
  annotate("text", x = brell_split, y = max(data$price_return, na.rm = TRUE) * 0.9,
           label = "BRELL Disconnection", angle = 90, vjust = -0.5, size = 2, color = "blue") +
  labs(title = "Electricity Price Log Returns", x = "Date", y = "Log Return") +
  theme_minimal()

# Stationarity
adf.test(data$price_return, alternative = "stationary")

# Correlation matrix
cor_matrix <- cor(data %>% select(all_of(numeric_cols)), use = "complete.obs")
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.8)

# PCA
atc_pca <- prcomp(data[atc_vars], scale. = TRUE)
summary(atc_pca)
plot(atc_pca, type = "lines", main = "PCA Scree Plot - ATC Variables")
# === Alternative PCA ==

# Re-create PCA scree plot with percentages and labels
explained_var <- atc_pca$sdev^2 / sum(atc_pca$sdev^2)
cumulative_var <- cumsum(explained_var)

plot(explained_var, type = "b", pch = 19, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     main = "Scree Plot - ATC Variables")
lines(cumulative_var, type = "b", col = "blue", lty = 2)
legend("topright", legend = c("Individual", "Cumulative"),
       col = c("black", "blue"), lty = c(1, 2), bty = "n")


# GARCH(1,1)
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "std"
)

garch_fit <- ugarchfit(spec = garch_spec, data = coredata(price_xts))
# Display residual autocorrelation diagnostics 
par(mfrow = c(2, 2))
acf(residuals(garch_fit), lag.max = 40, main = "ACF of Residuals")
pacf(residuals(garch_fit), lag.max = 40, main = "PACF of Residuals")
acf(residuals(garch_fit)^2, lag.max = 40, main = "ACF of Squared Residuals")
pacf(residuals(garch_fit)^2, lag.max = 40, main = "PACF of Squared Residuals")

par(mfrow = c(1, 1))

volatility_garch_xts <- xts(sigma(garch_fit), order.by = index(price_xts))

# Create data frame for ggplot
vol_df_garch <- data.frame(
  datetime = index(volatility_garch_xts),
  volatility = as.numeric(volatility_garch_xts)
)

# Plot conditional volatility
brell_split <- as.POSIXct("2025-02-08")

ggplot(vol_df_garch, aes(x = datetime, y = volatility)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = as.numeric(brell_split), linetype = "dashed", color = "red") +
  annotate("text", x = brell_split, y = max(vol_df_garch$volatility, na.rm = TRUE) * 0.9,
           label = "BRELL Disconnection", angle = 90, vjust = -0.5, size = 2.5, color = "red") +
  labs(title = "GARCH(1,1) – Conditional Volatility", x = "Date", y = "Volatility") +
  theme_minimal()


# QQ Plot and Summary
plot(garch_fit, which = 9, main = "QQ Plot of GARCH Residuals",sub = "t-distribution fit, highlighting tail deviations" )
show(garch_fit)
# main = "QQ Plot of GARCH Residuals"
# sub = "t-distribution fit, highlighting tail deviations"

infocriteria(garch_fit)

# Define Garch-X
library(purrr)
library(tibble)
library(ggplot2)
library(car)  

run_garchx_model <- function(varname, data_xts, data_df) {
  exog <- as.matrix(data_df[[varname]])
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                          external.regressors = exog),
    mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
    distribution.model = "std"
  )
  fit <- ugarchfit(spec, data = coredata(data_xts), solver = "hybrid")
  ic <- infocriteria(fit)
  tibble(Variable = varname, AIC = ic[1], BIC = ic[2], Fit = list(fit))
}

# Run Garch-X
indiv_results <- map_dfr(all_vars, ~run_garchx_model(.x, price_xts, data))
indiv_top <- indiv_results %>% arrange(AIC) %>% head(5)
write.csv(select(indiv_results, -Fit), "garchx_individual_results_summary.csv", row.names = FALSE)

# Define combinations
combinations <- list(
  c("dummy_value", "ATC_LV_LT"),
  c("dummy_value", "ATC_LT_LV"),
  c("dummy_value", "cloud_cover"),
  c("dummy_value", "ATC_LV_LT", "ATC_LT_LV"),
  c("dummy_value", "ATC_LV_LT", "cloud_cover"),
  c("dummy_value", "ATC_LT_LV", "cloud_cover"),
  c("dummy_value", "ATC_LV_LT", "ATC_LT_LV", "cloud_cover"),
  c("dummy_value", "lpg_price"),
  c("dummy_value", "lpg_price", "cloud_cover")
)

run_combo_model <- function(vars, data_xts, data_df) {
  exog <- as.matrix(data_df[, vars])
  combo_name <- paste(vars, collapse = " + ")
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                          external.regressors = exog),
    mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
    distribution.model = "std"
  )
  fit <- ugarchfit(spec, data = coredata(data_xts), solver = "hybrid")
  ic <- infocriteria(fit)
  tibble(Combination = combo_name, AIC = ic[1], BIC = ic[2], Fit = list(fit))
}

combo_results <- map_dfr(combinations, ~run_combo_model(.x, price_xts, data))
combo_top <- combo_results %>% arrange(AIC) %>% head(5)
write.csv(select(combo_results, -Fit), "garchx_combination_results_summary.csv", row.names = FALSE)

# Pre/post brell comparison
pre_brell <- data %>% filter(dummy_value == 0)
post_brell <- data %>% filter(dummy_value == 1)
pre_xts <- xts(pre_brell$price_return, order.by = pre_brell$datetime)
post_xts <- xts(post_brell$price_return, order.by = post_brell$datetime)

# Use best combo from full data on both pre- and post- BRELL periods
best_combo_vars <- unlist(strsplit(combo_top$Combination[1], " [+] "))

fit_segmented_model <- function(subset_xts, subset_df, vars) {
  exog <- as.matrix(subset_df[, vars])
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                          external.regressors = exog),
    mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
    distribution.model = "std"
  )
  ugarchfit(spec, data = coredata(subset_xts), solver = "hybrid")
}

fit_pre <- fit_segmented_model(pre_xts, pre_brell, best_combo_vars)
fit_post <- fit_segmented_model(post_xts, post_brell, best_combo_vars)

cat("\n=== Info Criteria for Pre-BRELL ===\n")
print(infocriteria(fit_pre))

cat("\n=== Info Criteria for Post-BRELL ===\n")
print(infocriteria(fit_post))

# Save top results
saveRDS(indiv_top, "top_individual_garchx_models.rds")
saveRDS(combo_top, "top_combined_garchx_models.rds")
saveRDS(list(pre=fit_pre, post=fit_post), "brell_segmented_models.rds")

# Plot volatility
best_fit <- combo_top$Fit[[1]]
vol_xts <- xts(sigma(best_fit), order.by = index(price_xts))
vol_df <- data.frame(datetime = index(vol_xts), volatility = as.numeric(vol_xts))

# Calculate pre-/post- BRELL averages
brell_split <- as.POSIXct("2025-02-08 00:00:00")
vol_df$period <- ifelse(vol_df$datetime < brell_split, "Pre-BRELL", "Post-BRELL")
mean_vol <- vol_df %>% group_by(period) %>% summarise(avg_vol = mean(volatility, na.rm = TRUE), sd_vol = sd(volatility, na.rm = TRUE))

# Levene's test for variance equality, paprastam testui per daug kintamuju
cat("\n=== Levene's Test for Variance Equality ===\n")
print(leveneTest(volatility ~ period, data = vol_df))

# Plot
ggplot(vol_df, aes(x = datetime, y = volatility)) +
  geom_line(color = "darkblue") +
  geom_vline(xintercept = as.numeric(brell_split), linetype = "dashed", color = "red") +
  geom_hline(data = mean_vol, aes(yintercept = avg_vol, color = period), linetype = "dotted", linewidth = 1) +
  scale_color_manual(values = c("Pre-BRELL" = "blue", "Post-BRELL" = "green")) +
  labs(title = paste("Volatility - Best GARCH-X Model:", combo_top$Combination[1]),
       subtitle = "Dashed red = BRELL disconnection | Dotted = Average conditional volatility",
       x = "Date", y = "Conditional Volatility", color = "Period") +
  theme_minimal()

# =================================================
# EXPORT INTO CSV FORMAT

# Extract coefficients
garch_coef <- as.data.frame(coef(garch_fit))
colnames(garch_coef) <- "Estimate"
garch_coef$Parameter <- rownames(garch_coef)
garch_coef <- garch_coef[, c("Parameter", "Estimate")]

# Log-likelihood and info criteria
loglik <- likelihood(garch_fit)
aic <- infocriteria(garch_fit)[1]
bic <- infocriteria(garch_fit)[2]

# Create a second data frame with the same structure
info_df <- data.frame(
  Parameter = c("LogLikelihood", "AIC", "BIC"),
  Estimate = c(loglik, aic, bic)
)

# Merge both
garch_summary <- rbind(garch_coef, info_df)

# Export to CSV
write.csv(garch_summary, "garch_11_summary.csv", row.names = FALSE)



# Drop the model object column before exporting
combo_export <- combo_top[, !(names(combo_top) %in% c("Fit"))]

# export
write.csv(combo_export, "top_garchx_models.csv", row.names = FALSE)


# Extract and save pre/post-BRELL model AICs
pre_post_ics <- data.frame(
  Period = c("Pre-BRELL", "Post-BRELL"),
  AIC = c(infocriteria(fit_pre)[1], infocriteria(fit_post)[1]),
  BIC = c(infocriteria(fit_pre)[2], infocriteria(fit_post)[2])
)
write.csv(pre_post_ics, "pre_post_brell_model_comparison.csv", row.names = FALSE)

# Robustness
egarch_spec <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
  distribution.model = "std"
)
egarch_fit <- ugarchfit(spec = egarch_spec, data = coredata(price_xts))
show(egarch_fit)
egarch_ic <- infocriteria(egarch_fit)
garch_ic <- infocriteria(garch_fit)

comparison <- data.frame(
  Model = c("GARCH(1,1)", "EGARCH(1,1)"),
  AIC = c(garch_ic[1], egarch_ic[1]),
  BIC = c(garch_ic[2], egarch_ic[2])
)
print(comparison)

# SENSITIVITY ANALYSIS 

gas_daily <- read_excel("C:/Users/marty/Downloads/Trades_daily_highest_only.xlsx")
colnames(gas_daily)[which(names(gas_daily) == "price")] <- "lpg_price"
gas_daily$datetime <- as.POSIXct(gas_daily$datetime)

# Forward-fill method (step interpolation)
gas_hourly_ffill <- merge(data.frame(datetime = seq(min(gas_daily$datetime),
                                                    max(gas_daily$datetime),
                                                    by = "hour")),
                          gas_daily, by = "datetime", all.x = TRUE)
gas_hourly_ffill$lpg_price <- na.locf(gas_hourly_ffill$lpg_price, fromLast = FALSE)
data_ffill <- data %>%
  select(-lpg_price) %>%
  left_join(gas_hourly_ffill, by = "datetime")

ffill_xts <- xts(data_ffill$price_return, order.by = data_ffill$datetime)
exog_ffill <- as.matrix(data_ffill[, best_combo_vars])

garchx_ffill_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                        external.regressors = exog_ffill),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "std"
)

garchx_ffill_fit <- ugarchfit(spec = garchx_ffill_spec, data = coredata(ffill_xts))
show(garchx_ffill_fit)

# Compare AIC/BIC with the original interpolation
original_ic <- infocriteria(best_fit)
ffill_ic <- infocriteria(garchx_ffill_fit)

gas_comparison <- data.frame(
  Method = c("Original Interpolation", "Forward-fill Interpolation"),
  AIC = c(original_ic[1], ffill_ic[1]),
  BIC = c(original_ic[2], ffill_ic[2])
)

print(gas_comparison)