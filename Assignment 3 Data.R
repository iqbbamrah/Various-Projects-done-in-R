# Question 2 Monte Carlo Simulation
# Part 1
n <- 2000
u <- 0.02
alpha <- 0.05
beta <- 0.8
e <- rnorm(n,0,1)

# Part 2
x <- rep(0,2000)
sigma <- rep(1,2000)
sigma[1] <- u/(1-alpha-beta)

for(i in 2:2000) {
  
  sigma[i] = u + alpha*x[i-1]^2 + beta*sigma[i-1];
  x[i]=sigma[i]^0.5*e[i];
}

# Part 3
arch1 <- garchFit(~garch(1,0), data = x, trace = F)
arch2 <- garchFit(~garch(2,0), data = x, trace = F)
garch11 <- garchFit(~garch(1,1), data = x, trace = F)
garch22 <- garchFit(~garch(2,2), data = x, trace = F)

summary(arch1)
summary(arch2)
summary(garch11)
summary(garch22)

# Part 4 i
x1 <- x[1:1000]
x2 <- x[1001:2000]

# Part ii 
(arch11 <- garchFit(~garch(1,0), data = x1, trace = F))
(garch111 <- garchFit(~garch(1,1), data = x1, trace = F))
(garch222 <- garchFit(~garch(2,2), data = x1, trace = F))
plot(arch1, type = "l")
plot(garch11, type = "l")
plot(garch22, type = "l")

# Part iii
arch11forecast <- forecast(arch11@sigma.t, h = 1000)
garch11forecast <- forecast(garch111@sigma.t, h = 1000)
garch22forecast <- forecast(garch222@sigma.t, h = 1000)
data <- data.frame(arch11forecast$x, garch11forecast$x, garch22forecast$x, x2)
data$numbers <- 1:nrow(data)
plot(data$numbers, data$arch11forecast.x, type = "l", col = "red", xlab = "Observations", ylab = "Forecast")
lines(data$numbers, data$garch11forecast.x, type = "l", col = "purple")
lines(data$numbers, data$garch22forecast.x, type = "l", col = "green")

# Question 3

AMD <- read.csv("/Users/iqbbamrah/Documents/AFM/4th Year/4B/ECON 423/Stock Group/AMD.csv")
BBBY <- read.csv("/Users/iqbbamrah/Documents/AFM/4th Year/4B/ECON 423/Stock Group/BBBY.csv")
EBAY <- read.csv("/Users/iqbbamrah/Documents/AFM/4th Year/4B/ECON 423/Stock Group/EBAY.csv")
GME <- read.csv("/Users/iqbbamrah/Documents/AFM/4th Year/4B/ECON 423/Stock Group/GME.csv")
MSFT <- read.csv("/Users/iqbbamrah/Documents/AFM/4th Year/4B/ECON 423/Stock Group/MSFT.csv")


AMDreturns <- 100*(log(AMD$Adj.Close[-1])-log(AMD$Adj.Close)[-nrow(AMD)])
BBBYreturns <- 100*(log(BBBY$Adj.Close[-1])-log(BBBY$Adj.Close)[-nrow(BBBY)])
EBAYreturns <- 100*(log(EBAY$Adj.Close[-1])-log(EBAY$Adj.Close)[-nrow(EBAY)])
GMEreturns <- 100*(log(GME$Adj.Close[-1])-log(GME$Adj.Close)[-nrow(GME)])
MSFTreturns <- 100*(log(MSFT$Adj.Close[-1])-log(MSFT$Adj.Close)[-nrow(MSFT)])

# Part 1 
# AMD
arch1AMD <- garchFit(~garch(1,0), data = AMDreturns, trace = F)
arch2AMD <- garchFit(~garch(2,0), data = AMDreturns, trace = F)
garch11AMD <- garchFit(~garch(1,1), data = AMDreturns, trace = F)
garch12AMD <- garchFit(~garch(1,2), data = AMDreturns, trace = F)
garch22AMD <- garchFit(~garch(2,2), data = AMDreturns, trace = F)

# BBBY
arch1BBBY <- garchFit(~garch(1,0), data = BBBYreturns, trace = F)
arch2BBBY <- garchFit(~garch(2,0), data = BBBYreturns, trace = F)
garch11BBBY <- garchFit(~garch(1,1), data = BBBYreturns, trace = F)
garch12BBBY <- garchFit(~garch(1,2), data = BBBYreturns, trace = F)
garch22BBBY <- garchFit(~garch(2,2), data = BBBYreturns, trace = F)

# EBAY
arch1EBAY <- garchFit(~garch(1,0), data = EBAYreturns, trace = F)
arch2EBAY <- garchFit(~garch(2,0), data = EBAYreturns, trace = F)
garch11EBAY <- garchFit(~garch(1,1), data = EBAYreturns, trace = F)
garch12EBAY <- garchFit(~garch(1,2), data = EBAYreturns, trace = F)
garch22EBAY <- garchFit(~garch(2,2), data = EBAYreturns, trace = F)

# GME
arch1GME <- garchFit(~garch(1,0), data = GMEreturns, trace = F)
arch2GME <- garchFit(~garch(2,0), data = GMEreturns, trace = F)
garch11GME <- garchFit(~garch(1,1), data = GMEreturns, trace = F)
garch12GME <- garchFit(~garch(1,2), data = GMEreturns, trace = F)
garch22GME <- garchFit(~garch(2,2), data = GMEreturns, trace = F)

# MSFT
arch1MSFT <- garchFit(~garch(1,0), data = MSFTreturns, trace = F)
arch2MSFT <- garchFit(~garch(2,0), data = MSFTreturns, trace = F)
garch11MSFT <- garchFit(~garch(1,1), data = MSFTreturns, trace = F)
garch12MSFT <- garchFit(~garch(1,2), data = MSFTreturns, trace = F)
garch22MSFT <- garchFit(~garch(2,2), data = MSFTreturns, trace = F)

# Part 2
# AMD
summary(arch1AMD)
summary(arch2AMD)
summary(garch11AMD)
summary(garch12AMD)
summary(garch22AMD)

# BBBY
summary(arch1BBBY)
summary(arch2BBBY)
summary(garch11BBBY)
summary(garch12BBBY)
summary(garch22BBBY)

# EBAY
summary(arch1EBAY)
summary(arch2EBAY)
summary(garch11EBAY)
summary(garch12EBAY)
summary(garch22EBAY)

# GME
summary(arch1GME)
summary(arch2GME)
summary(garch11GME)
summary(garch12GME)
summary(garch22GME)

# MSFT
summary(arch1MSFT)
summary(arch2MSFT)
summary(garch11MSFT)
summary(garch12MSFT)
summary(garch22MSFT)

# Part 3
# AMD
plot(arch1AMD@h.t, AMDreturns^2)
plot(arch2AMD@h.t, AMDreturns^2)
plot(garch11AMD@h.t, AMDreturns^2)
plot(garch12AMD@h.t, AMDreturns^2)
plot(garch22AMD@h.t, AMDreturns^2)

# BBBY
plot(arch1BBBY@h.t, BBBYreturns^2)
plot(arch2BBBY@h.t, BBBYreturns^2)
plot(garch11BBBY@h.t, BBBYreturns^2)
plot(garch12BBBY@h.t, BBBYreturns^2)
plot(garch22BBBY@h.t, BBBYreturns^2)

# EBAY
plot(arch1EBAY@h.t, EBAYreturns^2)
plot(arch2EBAY@h.t, EBAYreturns^2)
plot(garch11EBAY@h.t, EBAYreturns^2)
plot(garch12EBAY@h.t, EBAYreturns^2)
plot(garch22EBAY@h.t, EBAYreturns^2)

# GME
plot(arch1GME@h.t, GMEreturns^2)
plot(arch2GME@h.t, GMEreturns^2)
plot(garch11GME@h.t, GMEreturns^2)
plot(garch12GME@h.t, GMEreturns^2)
plot(garch22GME@h.t, GMEreturns^2)

# MSFT
plot(arch1MSFT@h.t, MSFTreturns^2)
plot(arch2MSFT@h.t, MSFTreturns^2)
plot(garch11MSFT@h.t, MSFTreturns^2)
plot(garch12MSFT@h.t, MSFTreturns^2)
plot(garch22MSFT@h.t, MSFTreturns^2)

# Part 4 a
# AMD
arch1AMDforecast <- forecast(arch1AMD@sigma.t, h = 60)
arch2AMDforecast <- forecast(arch2AMD@sigma.t, h = 60)
garch11AMDforecast <- forecast(garch11AMD@sigma.t, h = 60)
garch12AMDforecast <- forecast(garch12AMD@sigma.t, h = 60)
garch22AMDforecast <- forecast(garch22AMD@sigma.t, h = 60)

# BBBY
arch1BBBYforecast <- forecast(arch1BBBY@sigma.t, h = 60)
arch2BBBYforecast <- forecast(arch2BBBY@sigma.t, h = 60)
garch11BBBYforecast <- forecast(garch11BBBY@sigma.t, h = 60)
garch12BBBYforecast <- forecast(garch12BBBY@sigma.t, h = 60)
garch22BBBYforecast <- forecast(garch22BBBY@sigma.t, h = 60)

# EBAY
arch1EBAYforecast <- forecast(arch1EBAY@sigma.t, h = 60)
arch2EBAYforecast <- forecast(arch2EBAY@sigma.t, h = 60)
garch11EBAYforecast <- forecast(garch11EBAY@sigma.t, h = 60)
garch12EBAYforecast <- forecast(garch12EBAY@sigma.t, h = 60)
garch22EBAYforecast <- forecast(garch22EBAY@sigma.t, h = 60)

# GME
arch1GMEforecast <- forecast(arch1GME@sigma.t, h = 60)
arch2GMEforecast <- forecast(arch2GME@sigma.t, h = 60)
garch11GMEforecast <- forecast(garch11GME@sigma.t, h = 60)
garch12GMEforecast <- forecast(garch12GME@sigma.t, h = 60)
garch22GMEforecast <- forecast(garch22GME@sigma.t, h = 60)

# MSFT
arch1MSFTforecast <- forecast(arch1MSFT@sigma.t, h = 60)
arch2MSFTforecast <- forecast(arch2MSFT@sigma.t, h = 60)
garch11MSFTforecast <- forecast(garch11MSFT@sigma.t, h = 60)
garch12MSFTforecast <- forecast(garch12MSFT@sigma.t, h = 60)
garch22MSFTforecast <- forecast(garch22MSFT@sigma.t, h = 60)

# Part 4 b
# AMD
# MSE1 & MSE2 for ARCH1
mean((arch1AMDforecast$residuals - arch1AMD@sigma.t)^2)
mean(((arch1AMDforecast$residuals)^2) - ((arch1AMD@sigma.t)^2))
#MSE1 & MSE2 for ARCH2
mean((arch2AMDforecast$residuals - arch2AMD@sigma.t)^2)
mean(((arch2AMDforecast$residuals)^2) - ((arch2AMD@sigma.t)^2))
#MSE1 & MSE2 for GARCH(1,1)
mean((garch11AMDforecast$residuals - garch11AMD@sigma.t)^2)
mean(((garch11AMDforecast$residuals)^2) - ((garch11AMD@sigma.t)^2))
#MSE1 & MSE2 for GARCH(1,2)
mean((garch12AMDforecast$residuals - garch12AMD@sigma.t)^2)
mean(((garch12AMDforecast$residuals)^2) - ((garch12AMD@sigma.t)^2))
#MSE1 & MSE2 for GARCH(2,2)
mean((garch22AMDforecast$residuals - garch22AMD@sigma.t)^2)
mean(((garch22AMDforecast$residuals)^2) - ((garch22AMD@sigma.t)^2))

# BBBY
#MSE1 & MSE2 for ARCH1
mean((arch1BBBYforecast$residuals - arch1BBBY@sigma.t)^2)
mean(((arch1BBBYforecast$residuals)^2) - ((arch1BBBY@sigma.t)^2))
#MSE1 & MSE2 for ARCH2
mean((arch2BBBYforecast$residuals - arch2BBBY@sigma.t)^2)
mean(((arch2BBBYforecast$residuals)^2) - ((arch2BBBY@sigma.t)^2))
#MSE1 & MSE2 for GARCH(1,1)
mean((garch11BBBYforecast$residuals - garch11BBBY@sigma.t)^2)
mean(((garch11BBBYforecast$residuals)^2) - ((garch11BBBY@sigma.t)^2))
#MSE1 & MSE2 for GARCH(1,2)
mean((garch12BBBYforecast$residuals - garch12BBBY@sigma.t)^2)
mean(((garch12BBBYforecast$residuals)^2) - ((garch12BBBY@sigma.t)^2))
#MSE1 & MSE2 for GARCH(2,2)
mean((garch22BBBYforecast$residuals - garch22BBBY@sigma.t)^2)
mean(((garch22BBBYforecast$residuals)^2) - ((garch22BBBY@sigma.t)^2))

# EBAY
#MSE1 & MSE2 for ARCH1
mean((arch1EBAYforecast$residuals - arch1EBAY@sigma.t)^2)
mean(((arch1EBAYforecast$residuals)^2) - ((arch1EBAY@sigma.t)^2))
#MSE1 & MSE2 for ARCH2
mean((arch2EBAYforecast$residuals - arch2EBAY@sigma.t)^2)
mean(((arch2EBAYforecast$residuals)^2) - ((arch2EBAY@sigma.t)^2))
#MSE1 & MSE2 for GARCH(1,1)
mean((garch11EBAYforecast$residuals - garch11EBAY@sigma.t)^2)
mean(((garch11EBAYforecast$residuals)^2) - ((garch11EBAY@sigma.t)^2))
#MSE1 & MSE2 for GARCH(1,2)
mean((garch12EBAYforecast$residuals - garch12EBAY@sigma.t)^2)
mean(((garch12EBAYforecast$residuals)^2) - ((garch12EBAY@sigma.t)^2))
#MSE1 & MSE2 for GARCH(2,2)
mean((garch22EBAYforecast$residuals - garch22EBAY@sigma.t)^2)
mean(((garch22EBAYforecast$residuals)^2) - ((garch22EBAY@sigma.t)^2))

# GME
#MSE1 & MSE2 for ARCH1
mean((arch1GMEforecast$residuals - arch1GME@sigma.t)^2)
mean(((arch1GMEforecast$residuals)^2) - ((arch1GME@sigma.t)^2))
#MSE1 & MSE2 for ARCH2
mean((arch2GMEforecast$residuals - arch2GME@sigma.t)^2)
mean(((arch2GMEforecast$residuals)^2) - ((arch2GME@sigma.t)^2))
#MSE1 & MSE2 for GARCH(1,1)
mean((garch11GMEforecast$residuals - garch11GME@sigma.t)^2)
mean(((garch11GMEforecast$residuals)^2) - ((garch11GME@sigma.t)^2))
#MSE1 & MSE2 for GARCH(1,2)
mean((garch12GMEforecast$residuals - garch12GME@sigma.t)^2)
mean(((garch12GMEforecast$residuals)^2) - ((garch12GME@sigma.t)^2))
#MSE1 & MSE2 for GARCH(2,2)
mean((garch22GMEforecast$residuals - garch22GME@sigma.t)^2)
mean(((garch22GMEforecast$residuals)^2) - ((garch22GME@sigma.t)^2))

# MSFT
#MSE1 & MSE2 for ARCH1
mean((arch1MSFTforecast$residuals - arch1MSFT@sigma.t)^2)
mean(((arch1MSFTforecast$residuals)^2) - ((arch1MSFT@sigma.t)^2))
#MSE1 & MSE2 for ARCH2
mean((arch2MSFTforecast$residuals - arch2MSFT@sigma.t)^2)
mean(((arch2MSFTforecast$residuals)^2) - ((arch2MSFT@sigma.t)^2))
#MSE1 & MSE2 for GARCH(1,1)
mean((garch11MSFTforecast$residuals - garch11MSFT@sigma.t)^2)
mean(((garch11MSFTforecast$residuals)^2) - ((garch11MSFT@sigma.t)^2))
#MSE1 & MSE2 for GARCH(1,2)
mean((garch12MSFTforecast$residuals - garch12MSFT@sigma.t)^2)
mean(((garch12MSFTforecast$residuals)^2) - ((garch12MSFT@sigma.t)^2))
#MSE1 & MSE2 for GARCH(2,2)
mean((garch22MSFTforecast$residuals - garch22MSFT@sigma.t)^2)
mean(((garch22MSFTforecast$residuals)^2) - ((garch22MSFT@sigma.t)^2))