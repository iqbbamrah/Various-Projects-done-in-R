# Question 5
W <- arima.sim(n=20, list(ar = c(0.8, -0.6), ma = c(0.5)))
acf(W, type = c("correlation"))
W1 <- arima.sim(n=20, list(ar = c(0.8, -0.6), ma = c(0)))
acf(W1, type = c("correlation"))


# Question 6
# i
Et <- rnorm(1000)
summary(Et)

# ii
Y <- arima.sim(n=1000, list(ar = c(0.2021)))

# iii
mean(Y)
0.03072512
var(Y)
1.067843
X <- acf(Y, lag.max = 5, type = c("correlation"))
print(X)
X1 <- acf(Y, lag.max = 5, type = c("covariance"))
print(X1)

# iv
Y1 <- arima.errors(n=1000, list(ar = c(0.2021)))

# Question 7
# i
Z <- arima.sim(n=1000, list(ma = c(0.2021)))

# ii
mean(Z)
0.01246378
var(Z)
0.981958
ZAutoCorr <- acf(Z, lag.max = 5, type = c("correlation"))
ZAutoCov <- acf(Z, lag.max = 5, type = c("covariance"))
print(ZAutoCorr)

# iii


# Question 8
AMDReturns <- 100*(log(AMD$Adj.Close[-1])-log(AMD$Adj.Close)[-nrow(AMD)])
BBBYReturns <- 100*(log(BBBY$Adj.Close[-1])-log(BBBY$Adj.Close)[-nrow(BBBY)])

# i
# AMD
AMD1 <- arima(AMDReturns, order = c(1,0,0))
AMD2 <- arima(AMDReturns, order = c(2,0,0))
AMD3 <- arima(AMDReturns, order = c(3,0,0))
AMD4 <- arima(AMDReturns, order = c(4,0,0))
AMD5 <- arima(AMDReturns, order = c(5,0,0))

AMD1
AMD2
AMD3
AMD4
AMD5

# BBBY
BBBY1 <- arima(BBBYReturns, order = c(1,0,0))
BBBY2 <- arima(BBBYReturns, order = c(2,0,0))
BBBY3 <- arima(BBBYReturns, order = c(3,0,0))
BBBY4 <- arima(BBBYReturns, order = c(4,0,0))
BBBY5 <- arima(BBBYReturns, order = c(5,0,0))

BBBY1
BBBY2
BBBY3
BBBY4
BBBY5

# ii
AMDReturns1 <- ar.yw(AMDReturns, order.max = 5, aic = FALSE)

AMD11 <- accuracy(AMD1)[1:4]
AMD22 <- accuracy(AMD2)[1:4]
AMD33 <- accuracy(AMD3)[1:4]
AMD44 <- accuracy(AMD4)[1:4]
AMD55 <- accuracy(AMD5)[1:4]

Model1 <- matrix(c(AMD11, AMD22, AMD33, AMD44, AMD55), nrow = 5)
rownames(Model1) <- c("AMD11", "AMD22", "AMD33", "AMD44", "AMD55")
Model1 <- as.table(Model1)
Model1

BBBYReturns1 <- ar.yw(BBBYReturns, order.max = 5, aic = FALSE)

BBBY11 <- accuracy(BBBY1)[1:4]
BBBY22 <- accuracy(BBBY2)[1:4]
BBBY33 <- accuracy(BBBY3)[1:4]
BBBY44 <- accuracy(BBBY4)[1:4]
BBBY55 <- accuracy(BBBY5)[1:4]

Model2 <- matrix(c(BBBY11, BBBY22, BBBY33, BBBY44, BBBY55), nrow = 5)
rownames(Model2) <- c("BBBY11", "BBBY22", "BBBY33", "BBBY44", "BBBY55")
Model2 <- as.table(Model2)
Model2

# iii
forecast(AMD1, h = 5)
forecast(AMD2, h = 5)
forecast(AMD3, h = 5)
forecast(AMD4, h = 5)
forecast(AMD5, h = 5)

forecast(BBBY1, h = 5)
forecast(BBBY2, h = 5)
forecast(BBBY3, h = 5)
forecast(BBBY4, h = 5)
forecast(BBBY5, h = 5)

