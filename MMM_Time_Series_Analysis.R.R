# ============================================================================
# APPENDIX 1: COMPLETE R CODE FOR ASSIGNMENT 3
# Student: 6543537
# ============================================================================

# ============================================================================
# STEP 1: SETUP AND DATA LOADING
# ============================================================================
rm(list=ls())

# Install and load required libraries
library(vars)
library(lmtest)
library(tseries)
library(aTSA)

# Load the dataset
df_6543537 <- read.csv("C:/Users/debor/Downloads/6543537_timeseries(3).csv")

# Check structure
str(df_6543537)
head(df_6543537, 10)
nrow(df_6543537)

# ============================================================================
# STEP 2: DATA EXPLORATION
# ============================================================================

# Subset the relevant variables (columns 3-12)
dataset = df_6543537[, c(3:12)]
colnames(dataset) <- c("Week", "LnSales", "LnAdvertising", "LnPrice", 
                       "LnCompAdvertising", "LnCompPrice", "Qrtr1", "Qrtr2", "Qrtr3", "Qrtr4")
summary(dataset)

# Create real-price versions for managerial interpretation
dataset$Price = exp(dataset$LnPrice)
dataset$CompPrice = exp(dataset$LnCompPrice)
dataset$Sales = exp(dataset$LnSales)
dataset$Advertising = exp(dataset$LnAdvertising)
dataset$CompAdvertising = exp(dataset$LnCompAdvertising)

#Price premium calculation
price_premium = mean(dataset$Price) / mean(dataset$CompPrice)
cat("Price Premium (Ixmør vs Competitors):", round(price_premium, 2), "times\n")
# Price Premium (Ixmør vs Competitors): 2.76 times
# (Ixmør is 176 % more expensive than competitors)

# Display summary for key variables
summary(dataset[, 2:6])

# ============================================================================
# STEP 3: CORRELATION ANALYSIS
# ============================================================================

# Correlation matrix for key marketing variables
cor(dataset[c(2:6)])

# Key observations:
# - LnSales & LnAdvertising: moderate positive correlation
# - LnSales & LnCompPrice: positive correlation (price sensitivity)
# - LnAdvertising & LnCompAdvertising: highest correlation (competitive reaction)

# Scatterplot matrix
plot(dataset[, c("LnSales", "LnAdvertising", "LnPrice", 
                 "LnCompAdvertising", "LnCompPrice")],
     main="Scatterplots: LnSales, LnAdvertising, LnPrice, LnCompAdvertising, LnCompPrice")

# ============================================================================
# STEP 4: TIME SERIES PLOTS (Variables over time)
# ============================================================================

# Log Sales over time
plot(dataset[,c(1)], dataset[,c(2)], type="l", col="red", lwd=5,
     xlab="weeks", ylab="Sales", main="Log Sales over time")

# Managerial insight: Variation in real units
exp(max(dataset[,2])) - exp(min(dataset[,2]))
cat("Sales variation (real units):", round(exp(max(dataset[,2])) - exp(min(dataset[,2])), 2), "\n")

# Log Advertising over time
plot(dataset[,c(1)], dataset[,c(3)], type="l", col="red", lwd=5,
     xlab="weeks", ylab="Advertising", main="Log Advertising over time", ylim = c(0, 15))

# Log Price over time
plot(dataset[,c(1)], dataset[,c(4)], type="l", col="red", lwd=5,
     xlab="weeks", ylab="Price", main="Log Price over time")

# Managerial insight: Variation in real units
exp(max(dataset[,4])) - exp(min(dataset[,4]))
cat("Price variation (real units):", round(exp(max(dataset[,4])) - exp(min(dataset[,4])), 4), "\n")

# Log Competitor Advertising over time
plot(dataset[,c(1)], dataset[,c(5)], type="l", col="red", lwd=5,
     xlab="weeks", ylab="Advertising", main="Log Advertising of Competitor over time", ylim = c(0, 15))

# Log Competitor Price over time
plot(dataset[,c(1)], dataset[,c(6)], type="l", col="red", lwd=5,
     xlab="weeks", ylab="Price", main="Log Price of Competitor over time")

# ============================================================================
# STEP 5: COMPARISON LINE PLOTS (Ixmør vs Competitor)
# ============================================================================

#Ixmør Price vs Competitor Price (Log scale)
lnprice_plot <- dataset[, c(1, 4, 6)]
plot(lnprice_plot[, 1], lnprice_plot[, 2], type = "l", col = "red", lwd=2,
     xlab = "weeks", ylab = "Values", main = "Ixmør Ln Price (red) and Competitors Ln Price (black)",
     ylim = c(min(lnprice_plot[, 3]), max(lnprice_plot[, 2])))
lines(lnprice_plot[, 1], lnprice_plot[, 3], col = "black", lwd=2)
legend("bottomright", legend = c("Ixmør Price", "Competitors Price"), lty = 1, col = c("red", "black"))

# Ixmør Price vs Competitor Price (Real scale)
price_plot <- dataset[, c(1, 11, 12)]
plot(price_plot[, 1], price_plot[, 2], type = "l", col = "red", lwd=2,
     xlab = "weeks", ylab = "Values", main = "Ixmør Price (red) and Competitors Price (black)",
     ylim = c(0, max(price_plot[, 2])))
lines(price_plot[, 1], price_plot[, 3], col = "black", lwd=2)
legend("bottomright", legend = c("Ixmør Price", "Competitors Price"), lty = 1, col = c("red", "black"))

# Ixmør Advertising vs Competitor Advertising (Log scale)
lnadvertising_plot <- dataset[, c(1, 3, 5)]
plot(lnadvertising_plot[, 1], lnadvertising_plot[, 2], type = "l", col = "red", lwd=2,
     xlab = "weeks", ylab = "Values", main = "Ixmør Ln Advertising (red) and Ln Competitors Advertising (black)")
lines(lnadvertising_plot[, 1], lnadvertising_plot[, 3], col = "black", lwd=2)
legend("bottomright", legend = c("Ixmør Advertising", "Competitors Advertising"), lty = 1, col = c("red", "black"))
# ============================================================================
# STEP 6: GRANGER CAUSALITY TESTS
# ============================================================================

# Function to test and plot Granger causality p-values
plot_granger_pvals <- function(dv, iv, max_lag = 13, data) {
  pvals <- numeric(max_lag)
  
  for(i in 1:max_lag){
    test_res <- grangertest(as.formula(paste0(dv, " ~ ", iv)), order = i, data = data)
    pvals[i] <- test_res$`Pr(>F)`[2]
  }
  
  # Print significant results
  cat("\nTesting:", dv, "~", iv, "\n")
  for(i in 1:max_lag){
    if(pvals[i] < 0.05) {
      cat("Lag", i, ": p-value =", round(pvals[i], 5), "***\n")
    }
  }
  
  # Plot p-values
  plot(1:max_lag, pvals, type="b", pch=16,
       main=paste("Granger Causality p-values:", dv, "~", iv),
       xlab="Lag Order", ylab="p-value")
  abline(h=0.05, col="red", lty=2, lwd=2)
  
  return(pvals)
}

# Test 6.1: LnSales ~ LnAdvertising
cat("\n========== TEST 1: LnSales ~ LnAdvertising ==========\n")
granger_adv <- plot_granger_pvals("LnSales", "LnAdvertising", max_lag=13, data=dataset)
# Result: No significant lags

# Test 6.2: LnSales ~ LnPrice
cat("\n========== TEST 2: LnSales ~ LnPrice ==========\n")
granger_price <- plot_granger_pvals("LnSales", "LnPrice", max_lag=13, data=dataset)
# Result: No significant lags

# Test 6.3: LnSales ~ LnCompAdvertising
cat("\n========== TEST 3: LnSales ~ LnCompAdvertising ==========\n")
granger_comp_adv <- plot_granger_pvals("LnSales", "LnCompAdvertising", max_lag=13, data=dataset)
# Result: No significant lags

# Test 6.4: LnSales ~ LnCompPrice
cat("\n========== TEST 4: LnSales ~ LnCompPrice ==========\n")
granger_comp_price <- plot_granger_pvals("LnSales", "LnCompPrice", max_lag=13, data=dataset)
# Result: p = 0.05 at lag 6 (SIGNIFICANT!)

# ============================================================================
# STEP 7: PHILLIPS-PERRON UNIT ROOT TEST
# ============================================================================

# Test stationarity: H0 = Unit root present (non-stationary)
cat("\n========== PHILLIPS-PERRON UNIT ROOT TESTS ==========\n")

cat("\n--- LnSales ---\n")
pp.test(dataset$LnSales, output=TRUE)

cat("\n--- LnAdvertising ---\n")
pp.test(dataset$LnAdvertising, output=TRUE)

cat("\n--- LnPrice ---\n")
pp.test(dataset$LnPrice, output=TRUE)

cat("\n--- LnCompAdvertising ---\n")
pp.test(dataset$LnCompAdvertising, output=TRUE)

cat("\n--- LnCompPrice ---\n")
pp.test(dataset$LnCompPrice, output=TRUE)

# ============================================================================
# STEP 8: VAR MODEL ESTIMATION
# ============================================================================

cat("\n========== VAR MODEL ESTIMATION ==========\n")

# Prepare endogenous and exogenous variables
data_endo <- dataset[, c("LnSales", "LnAdvertising", "LnPrice", 
                         "LnCompAdvertising", "LnCompPrice")]
data_exo <- dataset[, c("Qrtr1", "Qrtr2", "Qrtr3")]

# Remove rows with missing values
complete_rows <- complete.cases(data_endo, data_exo)
data_endo <- data_endo[complete_rows, ]
data_exo <- data_exo[complete_rows, ]

# Determine optimal lag length using information criteria
lag_selection <- VARselect(data_endo, lag.max=13, type="both", exogen=data_exo)
cat("\nLag Selection Criteria:\n")
print(lag_selection$selection)

# Estimate VAR model with optimal lag
data_est <- VAR(data_endo, p=1, type="both", exogen=data_exo)

# Print summary for each endogenous variable equation
cat("\n--- Equation 1: LnSales ---\n")
summary(data_est, "LnSales")

cat("\n--- Equation 2: LnAdvertising ---\n")
summary(data_est, "LnAdvertising")

cat("\n--- Equation 3: LnPrice ---\n")
summary(data_est, "LnPrice")

cat("\n--- Equation 4: LnCompAdvertising ---\n")
summary(data_est, "LnCompAdvertising")

cat("\n--- Equation 5: LnCompPrice ---\n")
summary(data_est, "LnCompPrice")

# ============================================================================
# STEP 9: IMPULSE RESPONSE FUNCTIONS (IRF)
# ============================================================================

cat("\n========== IMPULSE RESPONSE FUNCTIONS (IRF) ==========\n")

# 9.1 IRF for LnSales (Individual effects)
cat("\n--- IRF for LnSales (Individual) ---\n")
irf1 <- irf(data_est, impulse=NULL, response="LnSales", n.ahead=13,
            ortho=TRUE, cumulative=FALSE, boot=TRUE, ci=0.68, runs=500)
plot(irf1, main="Orthogonal IRF (Individual): LnSales")

# 9.2 IRF for LnSales (Cumulative effects)
cat("\n--- IRF for LnSales (Cumulative) ---\n")
irf1.cum <- irf(data_est, impulse=NULL, response="LnSales", n.ahead=13,
                ortho=TRUE, cumulative=TRUE, boot=TRUE, ci=0.68, runs=500)
plot(irf1.cum, main="Orthogonal IRF (Cumulative): LnSales")

# 9.3 IRF for LnAdvertising (Individual effects)
cat("\n--- IRF for LnAdvertising (Individual) ---\n")
irf2 <- irf(data_est, impulse=NULL, response="LnAdvertising", n.ahead=13,
            ortho=TRUE, cumulative=FALSE, boot=TRUE, ci=0.68, runs=500)
plot(irf2, main="Orthogonal IRF (Individual): LnAdvertising")

# 9.4 IRF for LnAdvertising (Cumulative effects)
cat("\n--- IRF for LnAdvertising (Cumulative) ---\n")
irf2.cum <- irf(data_est, impulse=NULL, response="LnAdvertising", n.ahead=13,
                ortho=TRUE, cumulative=TRUE, boot=TRUE, ci=0.68, runs=500)
plot(irf2.cum, main="Orthogonal IRF (Cumulative): LnAdvertising")

# 9.5 IRF for LnPrice (Individual effects)
cat("\n--- IRF for LnPrice (Individual) ---\n")
irf3 <- irf(data_est, impulse=NULL, response="LnPrice", n.ahead=13,
            ortho=TRUE, cumulative=FALSE, boot=TRUE, ci=0.68, runs=500)
plot(irf3, main="Orthogonal IRF (Individual): LnPrice")

# 9.6 IRF for LnPrice (Cumulative effects)
cat("\n--- IRF for LnPrice (Cumulative) ---\n")
irf3.cum <- irf(data_est, impulse=NULL, response="LnPrice", n.ahead=13,
                ortho=TRUE, cumulative=TRUE, boot=TRUE, ci=0.68, runs=500)
plot(irf3.cum, main="Orthogonal IRF (Cumulative): LnPrice")

# 9.7 IRF for LnCompAdvertising (Individual effects)
cat("\n--- IRF for LnCompAdvertising (Individual) ---\n")
irf4 <- irf(data_est, impulse=NULL, response="LnCompAdvertising", n.ahead=13,
            ortho=TRUE, cumulative=FALSE, boot=TRUE, ci=0.68, runs=500)
plot(irf4, main="Orthogonal IRF (Individual): LnCompAdvertising")

# 9.8 IRF for LnCompAdvertising (Cumulative effects)
cat("\n--- IRF for LnCompAdvertising (Cumulative) ---\n")
irf4.cum <- irf(data_est, impulse=NULL, response="LnCompAdvertising", n.ahead=13,
                ortho=TRUE, cumulative=TRUE, boot=TRUE, ci=0.68, runs=500)
plot(irf4.cum, main="Orthogonal IRF (Cumulative): LnCompAdvertising")

# 9.9 IRF for LnCompPrice (Individual effects)
cat("\n--- IRF for LnCompPrice (Individual) ---\n")
irf5 <- irf(data_est, impulse=NULL, response="LnCompPrice", n.ahead=13,
            ortho=TRUE, cumulative=FALSE, boot=TRUE, ci=0.68, runs=500)
plot(irf5, main="Orthogonal IRF (Individual): LnCompPrice")

# 9.10 IRF for LnCompPrice (Cumulative effects)
cat("\n--- IRF for LnCompPrice (Cumulative) ---\n")
irf5.cum <- irf(data_est, impulse=NULL, response="LnCompPrice", n.ahead=13,
                ortho=TRUE, cumulative=TRUE, boot=TRUE, ci=0.68, runs=500)
plot(irf5.cum, main="Orthogonal IRF (Cumulative): LnCompPrice")

# ============================================================================
# STEP 10: FORECAST ERROR VARIANCE DECOMPOSITION (FEVD)
# ============================================================================

cat("\n========== FORECAST ERROR VARIANCE DECOMPOSITION (FEVD) ==========\n")

# Estimate FEVD
data_fevd1 <- fevd(data_est, n.ahead=13)

# Extract and format FEVD results (13 periods ahead)
fevd_matrix <- Reduce(rbind, data_fevd1)
fevd_matrix_t <- t(fevd_matrix)
fevd_final <- fevd_matrix_t[, c(13, 26, 39, 52, 65)]

# Create barplot with color scheme
library(RColorBrewer)
barplot(fevd_final,
        col=brewer.pal(5, "YlOrBr"),
        names.arg=c("LnSales", "LnAdvertising", "LnPrice", 
                    "LnCompAdvertising", "LnCompPrice"),
        xlab="Variables",
        ylab="Proportion of Forecast Error Variance",
        main="Forecast Error Variance Decomposition (FEVD) - 13 Periods Ahead")

legend("topright",
       legend=c("LnSales", "LnAdvertising", "LnPrice", 
                "LnCompAdvertising", "LnCompPrice"),
       fill=brewer.pal(5, "YlOrBr"),
       cex=0.85)

# Print detailed FEVD results
cat("\nFEVD Summary (13 periods ahead):\n")
print(data_fevd1)

# ============================================================================
# STEP 11: SUMMARY AND KEY FINDINGS
# ============================================================================

cat("\n========== ANALYSIS SUMMARY ==========\n")
cat("Total observations:", nrow(dataset), "\n")
cat("Time period: Weeks 1 to", max(dataset$Week), "\n")
cat("Price premium (Own vs Competitors):", round(price_premium, 3), "times\n")
cat("\nKey findings from VAR analysis to be detailed in main report.\n")



