#Preliminary

#install.packages("installr")
#library(installr)
#updateR()

#install.packages("dplyr")
#install.packages("quantmod")
#install.packages("PerformanceAnalytics")
#install.packages("imputeTS")
#install.packages("PortfolioAnalytics")
#install.packages("utf8")
#install.packages("tseries")
#install.packages("xml2")
#install.packages("xts")
#install.packages("matlib")
#install.packages("stats")
#install.packages("tidyquant")
#install.packages("quadprog")
#install.packages("ROI")
#install.packages("ROI.plugin.glpk")
#install.packages("ROI.plugin.quadprog")

library(xml2)
library(tseries)
library(utf8)
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(imputeTS)
library(PortfolioAnalytics)
library(xts)
library(stats)
library(tidyquant)
library(quadprog)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(readxl)


#Repeat the next two steps for each streaming index
#Import returns for streaming indicies

#Baseline
Baselineret <- read_excel("C:/Users/soere/OneDrive/Skrivebord/Thesis Index/Master index.xlsx", sheet = "Baseline", 
                          col_types = c("date", "numeric","numeric", "numeric", "numeric", "numeric"))
Baselineret$X <- as.Date(Baselineret$X)

#Convert to xts
Portfolio0ret <- xts(Baselineret[,-1], order.by = Baselineret$X)

#Index 1
Index1 <- read_excel("C:/Users/soere/OneDrive/Skrivebord/Thesis Index/Master index.xlsx", sheet = "Index1", 
                    col_types = c("date", "numeric"))
Index1$X <- as.Date(Index1$X)

#Convert to xts
Index1_ts <- xts(Index1$Index1, Index1$X)

#Index 2
Index2 <- read_excel("C:/Users/soere/OneDrive/Skrivebord/Thesis Index/Master index.xlsx", sheet = "Index2", 
                     col_types = c("date", "numeric"))
Index2$X <- as.Date(Index2$X)

#Convert to xts
Index2_ts <- xts(Index2$Index2, Index2$X)

#Index 3
Index3 <- read_excel("C:/Users/soere/OneDrive/Skrivebord/Thesis Index/Master index.xlsx", sheet = "Index3", 
                     col_types = c("date", "numeric"))
Index3$X <- as.Date(Index3$X)

#Convert to xts
Index3_ts <- xts(Index3$Index3, Index3$X)

#Index 4
Index4 <- read_excel("C:/Users/soere/OneDrive/Skrivebord/Thesis Index/Master index.xlsx", sheet = "Index4", 
                     col_types = c("date", "numeric"))
Index4$X <- as.Date(Index4$X)

#Convert to xts
Index4_ts <- xts(Index4$Index4, Index4$X)

#Index 5
Index5 <- read_excel("C:/Users/soere/OneDrive/Skrivebord/Thesis Index/Master index.xlsx", sheet = "Index5", 
                     col_types = c("date", "numeric"))
Index5$X <- as.Date(Index5$X)

#Convert to xts
Index5_ts <- xts(Index5$Index5, Index5$X)

#Index 6
Index6 <- read_excel("C:/Users/soere/OneDrive/Skrivebord/Thesis Index/Master index.xlsx", sheet = "Index6", 
                     col_types = c("date", "numeric"))
Index6$X <- as.Date(Index6$X)

#Convert to xts
Index6_ts <- xts(Index6$Index6, Index6$X)

#Index 7
Index7 <- read_excel("C:/Users/soere/OneDrive/Skrivebord/Thesis Index/Master index.xlsx", sheet = "Index7", 
                     col_types = c("date", "numeric"))
Index7$X <- as.Date(Index7$X)

#Convert to xts
Index7_ts <- xts(Index7$Index7, Index7$X)

#Index 8
Index8 <- read_excel("C:/Users/soere/OneDrive/Skrivebord/Thesis Index/Master index.xlsx", sheet = "Index8", 
                     col_types = c("date", "numeric"))
Index8$X <- as.Date(Index8$X)

#Convert to xts
Index8_ts <- xts(Index8$Index8, Index8$X)

#Index 9
Index9 <- read_excel("C:/Users/soere/OneDrive/Skrivebord/Thesis Index/Master index.xlsx", sheet = "Index9", 
                     col_types = c("date", "numeric"))
Index9$X <- as.Date(Index9$X)

#Convert to xts
Index9_ts <- xts(Index9$Index9, Index9$X)

#Index 10
Index10 <- read_excel("C:/Users/soere/OneDrive/Skrivebord/Thesis Index/Master index.xlsx", sheet = "Index10", 
                     col_types = c("date", "numeric"))
Index10$X <- as.Date(Index10$X)

#Convert to xts
Index10_ts <- xts(Index10$Index10, Index10$X)

#Index 11
Index11 <- read_excel("C:/Users/soere/OneDrive/Skrivebord/Thesis Index/Master index.xlsx", sheet = "Index11", 
                      col_types = c("date", "numeric"))
Index11$X <- as.Date(Index11$X)

#Convert to xts
Index11_ts <- xts(Index11$Index11, Index11$X)

#Index 12
Index12 <- read_excel("C:/Users/soere/OneDrive/Skrivebord/Thesis Index/Master index.xlsx", sheet = "Index12", 
                      col_types = c("date", "numeric"))
Index12$X <- as.Date(Index12$X)

#Convert to xts
Index12_ts <- xts(Index12$Index12, Index12$X)

#Construct portfolio return data (repeat for each portfolio)
Portfolio1ret <- cbind(Portfolio0ret,Index1_ts)
Portfolio2ret <- cbind(Portfolio0ret,Index2_ts)
Portfolio3ret <- cbind(Portfolio0ret,Index3_ts)
Portfolio4ret <- cbind(Portfolio0ret,Index4_ts)
Portfolio5ret <- cbind(Portfolio0ret,Index5_ts)
Portfolio6ret <- cbind(Portfolio0ret,Index6_ts)
Portfolio7ret <- cbind(Portfolio0ret,Index7_ts)
Portfolio8ret <- cbind(Portfolio0ret,Index8_ts)
Portfolio9ret <- cbind(Portfolio0ret,Index9_ts)
Portfolio10ret <- cbind(Portfolio0ret,Index10_ts)
Portfolio11ret <- cbind(Portfolio0ret,Index11_ts)
Portfolio12ret <- cbind(Portfolio0ret,Index12_ts)

#Specify portfolios (repeat for each)
Baseline <- portfolio.spec(colnames(Portfolio0ret))
Index1_1 <- portfolio.spec(colnames(Portfolio1ret))
Index2_1 <- portfolio.spec(colnames(Portfolio2ret))
Index3_1 <- portfolio.spec(colnames(Portfolio3ret))
Index4_1 <- portfolio.spec(colnames(Portfolio4ret))
Index5_1 <- portfolio.spec(colnames(Portfolio5ret))
Index6_1 <- portfolio.spec(colnames(Portfolio6ret))
Index7_1 <- portfolio.spec(colnames(Portfolio7ret))
Index8_1 <- portfolio.spec(colnames(Portfolio8ret))
Index9_1 <- portfolio.spec(colnames(Portfolio9ret))
Index10_1 <- portfolio.spec(colnames(Portfolio10ret))
Index11_1 <- portfolio.spec(colnames(Portfolio11ret))
Index12_1 <- portfolio.spec(colnames(Portfolio12ret))

#Define objectives and constraints of the portfolio (repeat for each portfolio)
Baseline <- add.constraint(Baseline, type="weight_sum", min_sum=0.9999999, max_sum=1.00000001, indexnum = 1)
Baseline <- add.objective(Baseline, type="return", name="mean")
Baseline <- add.objective(Baseline, type="risk", name="StdDev")


Index1_1 <- add.constraint(Index1_1, type="weight_sum", min_sum=0.9999999, max_sum=1.00000001, indexnum = 1)
Index1_1 <- add.objective(Index1_1, type="return", name="mean")
Index1_1 <- add.objective(Index1_1, type="risk", name="StdDev")


Index2_1 <- add.constraint(Index2_1, type="weight_sum", min_sum=0.9999999, max_sum=1.00000001, indexnum = 1)
Index2_1 <- add.objective(Index2_1, type="return", name="mean")
Index2_1 <- add.objective(Index2_1, type="risk", name="StdDev")


Index3_1 <- add.constraint(Index3_1, type="weight_sum", min_sum=0.9999999, max_sum=1.00000001, indexnum = 1)
Index3_1 <- add.objective(Index3_1, type="return", name="mean")
Index3_1 <- add.objective(Index3_1, type="risk", name="StdDev")


Index4_1 <- add.constraint(Index4_1, type="weight_sum", min_sum=0.9999999, max_sum=1.00000001, indexnum = 1)
Index4_1 <- add.objective(Index4_1, type="return", name="mean")
Index4_1 <- add.objective(Index4_1, type="risk", name="StdDev")


Index5_1 <- add.constraint(Index5_1, type="weight_sum", min_sum=0.9999999, max_sum=1.00000001, indexnum = 1)
Index5_1 <- add.objective(Index5_1, type="return", name="mean")
Index5_1 <- add.objective(Index5_1, type="risk", name="StdDev")


Index6_1 <- add.constraint(Index6_1, type="weight_sum", min_sum=0.9999999, max_sum=1.00000001, indexnum = 1)
Index6_1 <- add.objective(Index6_1, type="return", name="mean")
Index6_1 <- add.objective(Index6_1, type="risk", name="StdDev")


Index7_1 <- add.constraint(Index7_1, type="weight_sum", min_sum=0.9999999, max_sum=1.0000001, indexnum = 1)
Index7_1 <- add.objective(Index7_1, type="return", name="mean")
Index7_1 <- add.objective(Index7_1, type="risk", name="StdDev")


Index8_1 <- add.constraint(Index8_1, type="weight_sum", min_sum=0.9999999, max_sum=1.00000001, indexnum = 1)
Index8_1 <- add.objective(Index8_1, type="return", name="mean")
Index8_1 <- add.objective(Index8_1, type="risk", name="StdDev")


Index9_1 <- add.constraint(Index9_1, type="weight_sum", min_sum=0.9999999, max_sum=1.00000001, indexnum = 1)
Index9_1 <- add.objective(Index9_1, type="return", name="mean")
Index9_1 <- add.objective(Index9_1, type="risk", name="StdDev")


Index10_1 <- add.constraint(Index10_1, type="weight_sum", min_sum=0.9999999, max_sum=1.00000001, indexnum = 1)
Index10_1 <- add.objective(Index10_1, type="return", name="mean")
Index10_1 <- add.objective(Index10_1, type="risk", name="StdDev")


Index11_1 <- add.constraint(Index11_1, type="weight_sum", min_sum=0.9999999, max_sum=1.00000001, indexnum = 1)
Index11_1 <- add.objective(Index11_1, type="return", name="mean")
Index11_1 <- add.objective(Index11_1, type="risk", name="StdDev")


Index12_1 <- add.constraint(Index12_1, type="weight_sum", min_sum=0.9999999, max_sum=1.00000001, indexnum = 1)
Index12_1 <- add.objective(Index12_1, type="return", name="mean")
Index12_1 <- add.objective(Index12_1, type="risk", name="StdDev")


#Construct out-of-sample returns for each portfolio

tp <- 9
rw <- 9

#Rolling rebalancing of portfolio
Baselineport <- optimize.portfolio.rebalancing(Portfolio0ret,
                                            Baseline,
                                            optimize_method="ROI",
                                            rp=rp,
                                            rebalance_on="months",
                                            maxSR=TRUE,
                                            training_period=tp,
                                            rolling_window=rw)

Index1port <- optimize.portfolio.rebalancing(Portfolio1ret,
                                               Index1_1,
                                               optimize_method="ROI",
                                               rp=rp,
                                               rebalance_on="months",
                                               maxSR=TRUE,
                                               training_period=tp,
                                               rolling_window=rw)

Index2port <- optimize.portfolio.rebalancing(Portfolio2ret,
                                             Index2_1,
                                             optimize_method="ROI",
                                             rp=rp,
                                             rebalance_on="months",
                                             maxSR=TRUE,
                                             training_period=tp,
                                             rolling_window=rw)

Index3port <- optimize.portfolio.rebalancing(Portfolio3ret,
                                             Index3_1,
                                             optimize_method="ROI",
                                             rp=rp,
                                             rebalance_on="months",
                                             maxSR=TRUE,
                                             training_period=tp,
                                             rolling_window=rw)

Index4port <- optimize.portfolio.rebalancing(Portfolio4ret,
                                             Index4_1,
                                             optimize_method="ROI",
                                             rp=rp,
                                             rebalance_on="months",
                                             maxSR=TRUE,
                                             training_period=tp,
                                             rolling_window=rw)

Index5port <- optimize.portfolio.rebalancing(Portfolio5ret,
                                             Index5_1,
                                             optimize_method="ROI",
                                             rp=rp,
                                             rebalance_on="months",
                                             maxSR=TRUE,
                                             training_period=tp,
                                             rolling_window=rw)

Index6port <- optimize.portfolio.rebalancing(Portfolio6ret,
                                             Index6_1,
                                             optimize_method="ROI",
                                             rp=rp,
                                             rebalance_on="months",
                                             maxSR=TRUE,
                                             training_period=tp,
                                             rolling_window=rw)

Index7port <- optimize.portfolio.rebalancing(Portfolio7ret,
                                             Index7_1,
                                             optimize_method="ROI",
                                             rp=rp,
                                             rebalance_on="months",
                                             maxSR=TRUE,
                                             training_period=tp,
                                             rolling_window=rw)

Index8port <- optimize.portfolio.rebalancing(Portfolio8ret,
                                             Index8_1,
                                             optimize_method="ROI",
                                             rp=rp,
                                             rebalance_on="months",
                                             maxSR=TRUE,
                                             training_period=tp,
                                             rolling_window=rw)

Index9port <- optimize.portfolio.rebalancing(Portfolio9ret,
                                             Index9_1,
                                             optimize_method="ROI",
                                             rp=rp,
                                             rebalance_on="months",
                                             maxSR=TRUE,
                                             training_period=tp,
                                             rolling_window=rw)

Index10port <- optimize.portfolio.rebalancing(Portfolio10ret,
                                             Index10_1,
                                             optimize_method="ROI",
                                             rp=rp,
                                             rebalance_on="months",
                                             maxSR=TRUE,
                                             training_period=tp,
                                             rolling_window=rw)

Index11port <- optimize.portfolio.rebalancing(Portfolio11ret,
                                              Index11_1,
                                              optimize_method="ROI",
                                              rp=rp,
                                              rebalance_on="months",
                                              maxSR=TRUE,
                                              training_period=tp,
                                              rolling_window=rw)

Index12port <- optimize.portfolio.rebalancing(Portfolio12ret,
                                              Index12_1,
                                              optimize_method="ROI",
                                              rp=rp,
                                              rebalance_on="months",
                                              maxSR=TRUE,
                                              training_period=tp,
                                              rolling_window=rw)

chart.Weights(Baselineport, main="Rebalanced Weights Over Time")
chart.Weights(Index1port, main="Rebalanced Weights Over Time")
chart.Weights(Index2port, main="Rebalanced Weights Over Time")
chart.Weights(Index3port, main="Rebalanced Weights Over Time")
chart.Weights(Index4port, main="Rebalanced Weights Over Time")
chart.Weights(Index5port, main="Rebalanced Weights Over Time")
chart.Weights(Index6port, main="Rebalanced Weights Over Time")
chart.Weights(Index7port, main="Rebalanced Weights Over Time")
chart.Weights(Index8port, main="Rebalanced Weights Over Time")
chart.Weights(Index9port, main="Rebalanced Weights Over Time")
chart.Weights(Index10port, main="Rebalanced Weights Over Time")
chart.Weights(Index11port, main="Rebalanced Weights Over Time")
chart.Weights(Index12port, main="Rebalanced Weights Over Time")

#Define 1/N portfolio
equal_weight <- rep(1 / ncol(Portfolio0ret), ncol(Portfolio0ret))
Naive <- Return.portfolio(Portfolio0ret, weights = equal_weight, geometric = FALSE)
colnames(Naive) <- "1 over N Portfolio"

#Extract weights and returns of portfolio for each portfolio
Baselineport_weights1 <- round(extractWeights(Baselineport),5)
Index1port_weights1 <- round(extractWeights(Index1port),5)
Index2port_weights1 <- round(extractWeights(Index2port),5)
Index3port_weights1 <- round(extractWeights(Index3port),5)
Index4port_weights1 <- round(extractWeights(Index4port),5)
Index5port_weights1 <- round(extractWeights(Index5port),5)
Index6port_weights1 <- round(extractWeights(Index6port),5)
Index7port_weights1 <- round(extractWeights(Index7port),5)
Index8port_weights1 <- round(extractWeights(Index8port),5)
Index9port_weights1 <- round(extractWeights(Index9port),5)
Index10port_weights1 <- round(extractWeights(Index10port),5)
Index11port_weights1 <- round(extractWeights(Index11port),5)
Index12port_weights1 <- round(extractWeights(Index12port),5)

#Export to txt
Exportweights <- cbind(Baselineport_weights1,
                       Index1port_weights1,
                       Index2port_weights1,
                       Index3port_weights1,
                       Index4port_weights1,
                       Index5port_weights1,
                       Index6port_weights1,
                       Index7port_weights1,
                       Index8port_weights1,
                       Index9port_weights1,
                       Index10port_weights1,
                       Index11port_weights1,
                       Index12port_weights1)
write.zoo(Exportweights, file = "Long only weights.txt", col.names = TRUE, row.names = FALSE, sep = "\t")

#Extract weights for further calculation
Baselineport_weights <- extractWeights(Baselineport)
Index1port_weights <- extractWeights(Index1port)
Index2port_weights <- extractWeights(Index2port)
Index3port_weights <- extractWeights(Index3port)
Index4port_weights <- extractWeights(Index4port)
Index5port_weights <- extractWeights(Index5port)
Index6port_weights <- extractWeights(Index6port)
Index7port_weights <- extractWeights(Index7port)
Index8port_weights <- extractWeights(Index8port)
Index9port_weights <- extractWeights(Index9port)
Index10port_weights <- extractWeights(Index10port)
Index11port_weights <- extractWeights(Index11port)
Index12port_weights <- extractWeights(Index12port)

Baselineport_returns <- Return.portfolio(Portfolio0ret, weights=Baselineport_weights, geometric = FALSE)
Index1port_returns <- Return.portfolio(Portfolio1ret, weights=Index1port_weights, geometric = FALSE)
Index2port_returns <- Return.portfolio(Portfolio2ret, weights=Index2port_weights, geometric = FALSE)
Index3port_returns <- Return.portfolio(Portfolio3ret, weights=Index3port_weights, geometric = FALSE)
Index4port_returns <- Return.portfolio(Portfolio4ret, weights=Index4port_weights, geometric = FALSE)
Index5port_returns <- Return.portfolio(Portfolio5ret, weights=Index5port_weights, geometric = FALSE)
Index6port_returns <- Return.portfolio(Portfolio6ret, weights=Index6port_weights, geometric = FALSE)
Index7port_returns <- Return.portfolio(Portfolio7ret, weights=Index7port_weights, geometric = FALSE)
Index8port_returns <- Return.portfolio(Portfolio8ret, weights=Index8port_weights, geometric = FALSE)
Index9port_returns <- Return.portfolio(Portfolio9ret, weights=Index9port_weights, geometric = FALSE)
Index10port_returns <- Return.portfolio(Portfolio10ret, weights=Index10port_weights, geometric = FALSE)
Index11port_returns <- Return.portfolio(Portfolio11ret, weights=Index11port_weights, geometric = FALSE)
Index12port_returns <- Return.portfolio(Portfolio12ret, weights=Index12port_weights, geometric = FALSE)

#Summarize findings in chart
rets_df <- cbind(Baselineport_returns,
                 Index1port_returns,
                 Index2port_returns,
                 Index3port_returns,
                 Index4port_returns,
                 Index5port_returns,
                 Index6port_returns,
                 Index7port_returns,
                 Index8port_returns,
                 Index9port_returns,
                 Index10port_returns,
                 Index11port_returns,
                 Index12port_returns,
                 Naive)

charts.PerformanceSummary(rets_df, main="P/L Over Time")

#Cut off returns to out-of-sample period
rets1_df <- na.omit(cbind(Baselineport_returns,
                          Index1port_returns,
                          Index2port_returns,
                          Index3port_returns,
                          Index4port_returns,
                          Index5port_returns,
                          Index6port_returns,
                          Index7port_returns,
                          Index8port_returns,
                          Index9port_returns,
                          Index10port_returns,
                          Index11port_returns,
                          Index12port_returns,
                          Naive))


#Summarize return, std.dev and SR for each portfolio (monthly)
Returns = table.AnnualizedReturns(rets1_df, scale = 1, geometric = FALSE)

#Export results
write.table(Returns, file = "Long only mean returns.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.table(rets1_df, file = "Unconstrained returns.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
#Summarize return, std.dev and SR for each portfolio (Annualy)

Baseline_return <- (1+Return.annualized(Baselineport_returns, scale = 1, geometric = FALSE))^12-1
Index1_return <- (1+Return.annualized(Index1port_returns, scale = 1, geometric = FALSE))^12-1
Index2_return <- (1+Return.annualized(Index2port_returns, scale = 1, geometric = FALSE))^12-1
Index3_return <- (1+Return.annualized(Index3port_returns, scale = 1, geometric = FALSE))^12-1
Index4_return <- (1+Return.annualized(Index4port_returns, scale = 1, geometric = FALSE))^12-1
Index5_return <- (1+Return.annualized(Index5port_returns, scale = 1, geometric = FALSE))^12-1
Index6_return <- (1+Return.annualized(Index6port_returns, scale = 1, geometric = FALSE))^12-1
Index7_return <- (1+Return.annualized(Index7port_returns, scale = 1, geometric = FALSE))^12-1
Index8_return <- (1+Return.annualized(Index8port_returns, scale = 1, geometric = FALSE))^12-1
Index9_return <- (1+Return.annualized(Index9port_returns, scale = 1, geometric = FALSE))^12-1
Index10_return <- (1+Return.annualized(Index10port_returns, scale = 1, geometric = FALSE))^12-1
Index11_return <- (1+Return.annualized(Index11port_returns, scale = 1, geometric = FALSE))^12-1
Index12_return <- (1+Return.annualized(Index12port_returns, scale = 1, geometric = FALSE))^12-1
Naive_return <- (1+Return.annualized(rets1_df$X1.over.N.Portfolio, scale = 1, geometric = FALSE))^12-1

Returns_Annual = round(cbind(Baseline_return,
                             Index1_return,
                             Index2_return,
                             Index3_return,
                             Index4_return,
                             Index5_return,
                             Index6_return,
                             Index7_return,
                             Index8_return,
                             Index9_return,
                             Index10_return,
                             Index11_return,
                             Index12_return,
                             Naive_return),5)

Baseline_Std. <- StdDev(Baselineport_returns)*sqrt(12)
Index1_Std. <- StdDev(Index1port_returns)*sqrt(12)
Index2_Std. <- StdDev(Index2port_returns)*sqrt(12)
Index3_Std. <- StdDev(Index3port_returns)*sqrt(12)
Index4_Std. <- StdDev(Index4port_returns)*sqrt(12)
Index5_Std. <- StdDev(Index5port_returns)*sqrt(12)
Index6_Std. <- StdDev(Index6port_returns)*sqrt(12)
Index7_Std. <- StdDev(Index7port_returns)*sqrt(12)
Index8_Std. <- StdDev(Index8port_returns)*sqrt(12)
Index9_Std. <- StdDev(Index9port_returns)*sqrt(12)
Index10_Std. <- StdDev(Index10port_returns)*sqrt(12)
Index11_Std. <- StdDev(Index11port_returns)*sqrt(12)
Index12_Std. <- StdDev(Index12port_returns)*sqrt(12)
Naive_Std. <- StdDev(rets1_df$X1.over.N.Portfolio)*sqrt(12)

Std.annual = round(cbind(Baseline_Std.,
                         Index1_Std.,
                         Index2_Std.,
                         Index3_Std.,
                         Index4_Std.,
                         Index5_Std.,
                         Index6_Std.,
                         Index7_Std.,
                         Index8_Std.,
                         Index9_Std.,
                         Index10_Std.,
                         Index11_Std.,
                         Index12_Std.,
                         Naive_Std.),5)

Sharpe_Annual = round(Returns_Annual/Std.annual,5)

#Export Annual ratios

write.table(Returns_Annual, file = "Long only Annual returns.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.table(Std.annual, file = "Long only Annual std..txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.table(Sharpe_Annual, file = "Long only Annual sharpe.txt", col.names = TRUE, row.names = FALSE, sep = "\t")


#Out-of-sample betas
Index1_port <- lm(Index1port_returns ~ Baselineport_returns)
Index2_port <- lm(Index2port_returns ~ Baselineport_returns)
Index3_port <- lm(Index3port_returns ~ Baselineport_returns)
Index4_port <- lm(Index4port_returns ~ Baselineport_returns)
Index5_port <- lm(Index5port_returns ~ Baselineport_returns)
Index6_port <- lm(Index6port_returns ~ Baselineport_returns)
Index7_port <- lm(Index7port_returns ~ Baselineport_returns)
Index8_port <- lm(Index8port_returns ~ Baselineport_returns)
Index9_port <- lm(Index9port_returns ~ Baselineport_returns)
Index10_port <- lm(Index10port_returns ~ Baselineport_returns)
Index11_port <- lm(Index11port_returns ~ Baselineport_returns)
Index12_port <- lm(Index12port_returns ~ Baselineport_returns)
Naive_port <- lm(rets1_df$X1.over.N.Portfolio ~ Baselineport_returns)

beta_Index1 <- Index1_port$coefficients[2]
beta_Index2 <- Index2_port$coefficients[2]
beta_Index3 <- Index3_port$coefficients[2]
beta_Index4 <- Index4_port$coefficients[2]
beta_Index5 <- Index5_port$coefficients[2]
beta_Index6 <- Index6_port$coefficients[2]
beta_Index7 <- Index7_port$coefficients[2]
beta_Index8 <- Index8_port$coefficients[2]
beta_Index9 <- Index9_port$coefficients[2]
beta_Index10 <- Index10_port$coefficients[2]
beta_Index11 <- Index11_port$coefficients[2]
beta_Index12 <- Index12_port$coefficients[2]
beta_Naive <- Naive_port$coefficients[2]

#Risk-free
Rf <- 0.00

Alpha_port1 <- Index1_return-(Rf+beta_Index1*(Baseline_return-Rf))
Alpha_port2 <- Index2_return-(Rf+beta_Index2*(Baseline_return-Rf))
Alpha_port3 <- Index3_return-(Rf+beta_Index3*(Baseline_return-Rf))
Alpha_port4 <- Index4_return-(Rf+beta_Index4*(Baseline_return-Rf))
Alpha_port5 <- Index5_return-(Rf+beta_Index5*(Baseline_return-Rf))
Alpha_port6 <- Index6_return-(Rf+beta_Index6*(Baseline_return-Rf))
Alpha_port7 <- Index7_return-(Rf+beta_Index7*(Baseline_return-Rf))
Alpha_port8 <- Index8_return-(Rf+beta_Index8*(Baseline_return-Rf))
Alpha_port9 <- Index9_return-(Rf+beta_Index9*(Baseline_return-Rf))
Alpha_port10 <- Index10_return-(Rf+beta_Index10*(Baseline_return-Rf))
Alpha_port11 <- Index11_return-(Rf+beta_Index11*(Baseline_return-Rf))
Alpha_port12 <- Index12_return-(Rf+beta_Index12*(Baseline_return-Rf))
Alpha_Naive <- Naive_return-(Rf+beta_Naive*(Baseline_return-Rf))

Alphadf = round(cbind(Alpha_port1,
                      Alpha_port2,
                      Alpha_port3,
                      Alpha_port4,
                      Alpha_port5,
                      Alpha_port6,
                      Alpha_port7,
                      Alpha_port8,
                      Alpha_port9,
                      Alpha_port10,
                      Alpha_port11,
                      Alpha_port12,
                      Alpha_Naive),5)

#Export Alpha
write.zoo(Alphadf, file = "Alpha Long Only.txt", col.names = TRUE, row.names = FALSE, sep = "\t")

#Treynor ratio

Trey_Index1 <- Index1_return/Index1_port$coefficients[2]
Trey_Index2 <- Index2_return/Index2_port$coefficients[2]
Trey_Index3 <- Index3_return/Index3_port$coefficients[2]
Trey_Index4 <- Index4_return/Index4_port$coefficients[2]
Trey_Index5 <- Index5_return/Index5_port$coefficients[2]
Trey_Index6 <- Index6_return/Index6_port$coefficients[2]
Trey_Index7 <- Index7_return/Index7_port$coefficients[2]
Trey_Index8 <- Index8_return/Index8_port$coefficients[2]
Trey_Index9 <- Index9_return/Index9_port$coefficients[2]
Trey_Index10 <- Index10_return/Index10_port$coefficients[2]
Trey_Index11 <- Index11_return/Index11_port$coefficients[2]
Trey_Index12 <- Index12_return/Index12_port$coefficients[2]
Trey_Naive <- Naive_return/Naive_port$coefficients[2]

Treynor_Annual = round(cbind(Trey_Index1,
                             Trey_Index2,
                             Trey_Index3,
                             Trey_Index4,
                             Trey_Index5,
                             Trey_Index6,
                             Trey_Index7,
                             Trey_Index8,
                             Trey_Index9,
                             Trey_Index10,
                             Trey_Index11,
                             Trey_Index12,
                             Trey_Naive),5)

write.zoo(Treynor_Annual, file = "Treynor Long Only.txt", col.names = TRUE, row.names = FALSE, sep = "\t")

#excess returns

rets2_df = cbind(rets1_df$portfolio.returns,
                 rets1_df$portfolio.returns.1,
                 rets1_df$portfolio.returns.2,
                 rets1_df$portfolio.returns.3,
                 rets1_df$portfolio.returns.4,
                 rets1_df$portfolio.returns.5,
                 rets1_df$portfolio.returns.6,
                 rets1_df$portfolio.returns.7,
                 rets1_df$portfolio.returns.8,
                 rets1_df$portfolio.returns.9,
                 rets1_df$portfolio.returns.10,
                 rets1_df$portfolio.returns.11,
                 rets1_df$portfolio.returns.12,
                 rets1_df$X1.over.N.Portfolio)

rets3_df = cbind(rets1_df$portfolio.returns,
                 rets1_df$portfolio.returns,
                 rets1_df$portfolio.returns,
                 rets1_df$portfolio.returns,
                 rets1_df$portfolio.returns,
                 rets1_df$portfolio.returns,
                 rets1_df$portfolio.returns,
                 rets1_df$portfolio.returns,
                 rets1_df$portfolio.returns,
                 rets1_df$portfolio.returns,
                 rets1_df$portfolio.returns,
                 rets1_df$portfolio.returns,
                 rets1_df$portfolio.returns,
                 rets1_df$portfolio.returns)

Returns_excess = table.AnnualizedReturns(rets2_df-rets3_df, scale = 1, geometric = FALSE)

Returns_excess_Annual = round((1+Return.annualized(rets2_df-rets3_df, scale = 1, geometric = FALSE))^12-1,5)

Std.dev_excess_Annual = round(StdDev(rets2_df-rets3_df)*sqrt(12),5)

#Monthly excess returns
write.zoo(Returns_excess, file = "Excess montly Long Only.txt", col.names = TRUE, row.names = FALSE, sep = "\t")

#Export Annualized excess returns
write.zoo(Returns_excess_Annual, file = "Annual Excess Long Only.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.zoo(Std.dev_excess_Annual, file = "Annual Excess Long Only.txt", col.names = TRUE, row.names = FALSE, sep = "\t")

#Maximum returns

maxreturn = round(cbind(max(Baselineport_returns),
                        max(Index1port_returns),
                        max(Index2port_returns),
                        max(Index3port_returns),
                        max(Index4port_returns),
                        max(Index5port_returns),
                        max(Index6port_returns),
                        max(Index7port_returns),
                        max(Index8port_returns),
                        max(Index9port_returns),
                        max(Index10port_returns),
                        max(Index11port_returns),
                        max(Index12port_returns),
                        max(rets1_df$X1.over.N.Portfolio)),5)

#Export max return
write.zoo(maxreturn, file = "Maxreturn Long Only.txt", col.names = TRUE, row.names = FALSE, sep = "\t")

#Minimum returns

minreturn = round(cbind(min(Baselineport_returns),
                        min(Index1port_returns),
                        min(Index2port_returns),
                        min(Index3port_returns),
                        min(Index4port_returns),
                        min(Index5port_returns),
                        min(Index6port_returns),
                        min(Index7port_returns),
                        min(Index8port_returns),
                        min(Index9port_returns),
                        min(Index10port_returns),
                        min(Index11port_returns),
                        min(Index12port_returns),
                        min(rets1_df$X1.over.N.Portfolio)),5)

write.zoo(minreturn, file = "Minreturn Long Only.txt", col.names = TRUE, row.names = FALSE, sep = "\t")

#Maximum drawdown

maxdrawdown = round(maxDrawdown(rets1_df),5)
#Export maximum drawdown
write.zoo(maxdrawdown, file = "MaxDrawdown Long Only.txt", col.names = TRUE, row.names = FALSE, sep = "\t")

#Number of losing months

losing = round(cbind(nrow(rets1_df[rets1_df$portfolio.returns<0]),
                     nrow(rets1_df[rets1_df$portfolio.returns.1<0]),
                     nrow(rets1_df[rets1_df$portfolio.returns.2<0]),
                     nrow(rets1_df[rets1_df$portfolio.returns.3<0]),
                     nrow(rets1_df[rets1_df$portfolio.returns.4<0]),
                     nrow(rets1_df[rets1_df$portfolio.returns.5<0]),
                     nrow(rets1_df[rets1_df$portfolio.returns.6<0]),
                     nrow(rets1_df[rets1_df$portfolio.returns.7<0]),
                     nrow(rets1_df[rets1_df$portfolio.returns.8<0]),
                     nrow(rets1_df[rets1_df$portfolio.returns.9<0]),
                     nrow(rets1_df[rets1_df$portfolio.returns.10<0]),
                     nrow(rets1_df[rets1_df$portfolio.returns.11<0]),
                     nrow(rets1_df[rets1_df$portfolio.returns.12<0]),
                     nrow(rets1_df[rets1_df$X1.over.N.Portfolio<0])),5)

#Export losing months
write.zoo(losing, file = "Losing months Long Only.txt", col.names = TRUE, row.names = FALSE, sep = "\t")

#Export results
write.zoo(Exportweights, file = "Unconstrained weights.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.table(Returns, file = "Unconstrained mean returns.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.table(Returns_Annual, file = "Unconstrained Annual returns.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.table(Std.annual, file = "Unconstrained Annual std..txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.table(Sharpe_Annual, file = "Unconstrained Annual sharpe.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.zoo(Treynor_Annual, file = "Treynor Unconstrained.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.zoo(Alphadf, file = "Alpha Unconstrained.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.zoo(Returns_excess, file = "Excess montly Unconstrained.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.zoo(Returns_excess_Annual, file = "Annual Excess Unconstrained.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.zoo(Std.dev_excess_Annual, file = "Annual Excess std Unconstrained.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.zoo(maxreturn, file = "Maxreturn Unconstrained.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.zoo(minreturn, file = "Minreturn Unconstrained.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.zoo(maxdrawdown, file = "MaxDrawdown Unconstrained.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
write.zoo(losing, file = "Losing months Unconstrained.txt", col.names = TRUE, row.names = FALSE, sep = "\t")
