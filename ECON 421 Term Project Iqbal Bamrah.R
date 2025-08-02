# Task 1
data <- read.csv(file = "/Users/iqbbamrah/Downloads/Empirical_Data/20682484.csv")
attach(data)

# Question 4
# With age LPM
LPM <- glm(lfprt ~ expr + educ + age, data = data)
summary(LPM)
#Without age LPM (To compare the prediction performance with Probit mode without age for consistency)
LPM1 <- glm(lfprt ~ expr + educ, data = data)
summary(LPM)
# Without age Probit model
Probit <- glm(formula = lfprt ~ expr + educ, data = data, family = binomial(link = 'probit'))
summary(Probit)
# With age Probit model (Error when running this Data)
Probit1 <- glm(formula = lfprt ~ expr + educ + age, data = data, family = binomial(link = 'probit'), control = list(maxit = 50))
summary(Probit1)

# Question 5
ProbitMar <- margins(Probit)
summary(ProbitMar)

ProbitMar1 <- margins(Probit1)
summary(ProbitMar1)

# Question 6
fitted <- fitted.values(LPM)
summary(fitted)

fitted1 <- fitted.values(Probit)
summary(fitted1)

stargazer(LPM, Probit, type = "text")
stargazer(LPM1, Probit, type = "text")

# Task 2
# Question 3
BL <- -5
BU <- 5
x <- rnorm(1000, 0, 10)
BO <- 2
e <- rnorm(1000, 0, 6)
y <- x*BO + e
n <- numeric(1000)

for (i in 1:1000) {
  if (y[i] <= BL) {
    n[i] <- BL
  }
  else if(y[i] >= BU){
    n[i] <- BU
  }
  else {n[i] <- y[i]}
}

# Question 4
summary(y)
plot(y)

# Question 5
BL <- -5
BU <- 5
x1 <- rnorm(1000, 0, 10)
BO <- 2
e1 <- rnorm(1000, 0)
y1 <- x1*BO + e1

for (i in 1:1000) {
  if (y1[i] <= BL) {
    n[i] <- BL
  }
  else if(y1[i] >= BU){
    n[i] <- BU
  }
  else {n[i] <- y1[i]}
}
summary(y1)
plot(y1)

cbind(asymp.test(y, parameter = "var"),asymp.test(y1, parameter = "var"))
