# Question 3 Part 2
# AMD
hist(AMD$Adj.Close, main="histogram", xlab=AMD$Adj.Close, probability=TRUE, col="black")
mean(AMD$Adj.Close)
9.41835
var(AMD$Adj.Close)
74.02302
skewness(AMD$Adj.Close)
1.772327
kurtosis(AMD$Adj.Close)
5.654285
acf(AMD$Adj.Close, lag.max = 2767, plot = TRUE)

# BBBY
hist(BBBY$Adj.Close, main="histogram", xlab=BBBY$Adj.Close, probability=TRUE, col="black")
mean(BBBY$Adj.Close)
41.10518
var(BBBY$Adj.Close)
298.5054
skewness(BBBY$Adj.Close)
0.3450989
kurtosis(BBBY$Adj.Close)
1.934834
acf(BBBY$Adj.Close, lag.max = 2767, type = "correlation")

# EBAY
hist(EBAY$Adj.Close, main="histogram", xlab=EBAY$Adj.Close, probability=TRUE, col="black")
mean(EBAY$Adj.Close)
24.05209
var(EBAY$Adj.Close)
81.44503
skewness(EBAY$Adj.Close)
0.08059461
kurtosis(EBAY$Adj.Close)
2.008154
acf(EBAY$Adj.Close, lag.max = 2767, type = "correlation")

# GME
hist(GME$Adj.Close, main="histogram", xlab=GME$Adj.Close, probability=TRUE, col="black")
mean(GME$Adj.Close)
19.40047
var(GME$Adj.Close)
66.05066
skewness(GME$Adj.Close)
0.4879872
kurtosis(GME$Adj.Close)
2.514129
acf(GME$Adj.Close, lag.max = 2767, type = "correlation")

# MSFT
hist(MSFT$Adj.Close, main="histogram", xlab=MSFT$Adj.Close, probability=TRUE, col="black")
mean(MSFT$Adj.Close)
51.89265
var(MSFT$Adj.Close)
1209.567
skewness(MSFT$Adj.Close)
1.20124
kurtosis(MSFT$Adj.Close)
3.387358
acf(MSFT$Adj.Close, lag.max = 2767, type = "correlation")
acf(MSFT$Adj.Close)

# Question 3 Part 3
# AMD
hist(r1, main="histogram", xlab=r1, probability=TRUE, col="black")
r1 <- 100*(log(AMD$Adj.Close[-1])-log(AMD$Adj.Close)[-nrow(AMD)])
mean(r1)
0.06149665
var(r1)
12.71448
skewness(r1)
0.3520103
kurtosis(r1)
12.07636
acf(r1, lag.max = 2767, plot = TRUE)

# BBBY
hist(r2, main="histogram", xlab=r2, probability=TRUE, col="black")
r2 <- 100*(log(BBBY$Adj.Close[-1])-log(BBBY$Adj.Close)[-nrow(BBBY)])
mean(r2)
0.02755223
var(r2)
4.862941
skewness(r2)
0.801231
kurtosis(r2)
20.9829
acf(r2, lag.max = 2767, type = "correlation")

# EBAY
hist(r3, main="histogram", xlab=r3, probability=TRUE, col="black")
r3 <- 100*(log(EBAY$Adj.Close[-1])-log(EBAY$Adj.Close)[-nrow(EBAY)])
mean(r3)
0.05109304
var(r3)
3.174115
skewness(r3)
0.1020515
kurtosis(r3)
8.240902
acf(r3, lag.max = 2767, type = "correlation")

# GME
hist(r4, main="histogram", xlab=r4, probability=TRUE, col="black")
r4 <- 100*(log(GME$Adj.Close[-1])-log(GME$Adj.Close)[-nrow(GME)])
mean(r4)
0.03668966
var(r4)
7.686753
skewness(r4)
2.75527
kurtosis(r4)
36.65672
acf(r4, lag.max = 2767, type = "correlation")

# MSFT
hist(r5, main="histogram", xlab=r5, probability=TRUE, col="black")
r5 <- 100*(log(MSFT$Adj.Close[-1])-log(MSFT$Adj.Close)[-nrow(MSFT)])
mean(r5)
0.0742651
var(r5)
2.047234
skewness(r5)
0.1083984
kurtosis(r5)
6.440139
acf(r5, lag.max = 2767, type = "correlation")

# Question 3 Part 4
# AMD
r11 <- r1^2
hist(r11, main="histogram", xlab=r11, probability=TRUE, col="black")
acf(r11, lag.max = 2767, type = "correlation")

# BBBY
r22 <- r2^2
hist(r22, main="histogram", xlab=r22, probability=TRUE, col="black")
acf(r22, lag.max = 2767, type = "correlation")

# EBAY
r33 <- r3^2
hist(r33, main="histogram", xlab=r33, probability=TRUE, col="black")
acf(r33, lag.max = 2767, type = "correlation")

# GME
r44 <- r4^2
hist(r44, main="histogram", xlab=r44, probability=TRUE, col="black")
acf(r44, lag.max = 2767, type = "correlation")

#MSFT
r55 <- r5^2
hist(r55, main="histogram", xlab=r55, probability=TRUE, col="black")
acf(r55, lag.max = 2767, type = "correlation")

# Question 4 Part 5
# AMD
cor.test(r1,AMD$Volume[-1])

#BBBY
cor.test(r2,BBBY$Volume[-1])

#EBAY
cor.test(r3,EBAY$Volume[-1])

#GME
cor.test(r4,GME$Volume[-1])

#MSFT
cor.test(r5,MSFT$Volume[-1])

# Question 3 Part 6
# AMD
AMDClose <- AMD$Adj.Close[-c(1,2)]
AMDprevClose <- AMD$Adj.Close[-c(1,nrow(AMD))]
AMDprevClose1 <- AMD$Adj.Close[-c(nrow(AMD)-1,nrow(AMD))]

AMDprevincrease <- ifelse(AMDprevClose > AMDprevClose1, 1, 0)

AMDprevdecrease <- ifelse(AMDprevClose < AMDprevClose1, 1, 0)

AMDcurrentincrease <- ifelse(AMDClose > AMDprevClose, 1, 0)

AMDcurrentdecrease <- ifelse(AMDClose < AMDprevClose, 1, 0)

AMDprevdaysincrease <- which(AMDprevincrease%in%1)

AMDprevdaysdecrease <- which(AMDprevdecrease%in%0)

# i
mean(AMDcurrentincrease[AMDprevdaysincrease])
0.4955823
# ii
mean(AMDcurrentincrease[AMDprevdaysdecrease])
0.4887218
# iii
mean(AMDcurrentdecrease[AMDprevdaysincrease])
0.4682731
# iv
mean(AMDcurrentdecrease[AMDprevdaysdecrease])
0.4736842

# BBBY
BBBYClose <- BBBY$Adj.Close[-c(1,2)]
BBBYprevClose <- BBBY$Adj.Close[-c(1,nrow(BBBY))]
BBBYprevClose1 <- BBBY$Adj.Close[-c(nrow(BBBY)-1,nrow(BBBY))]

BBBYprevincrease <- ifelse(BBBYprevClose > BBBYprevClose1, 1, 0)

BBBYprevdecrease <- ifelse(BBBYprevClose < BBBYprevClose1, 1, 0)

BBBYcurrentincrease <- ifelse(BBBYClose > BBBYprevClose, 1, 0)

BBBYcurrentdecrease <- ifelse(BBBYClose < BBBYprevClose, 1, 0)

BBBYprevdaysincrease <- which(BBBYprevincrease%in%1)

BBBYprevdaysdecrease <- which(BBBYprevdecrease%in%0)

# i
mean(BBBYcurrentincrease[BBBYprevdaysincrease])
0.4619696
# ii
mean(BBBYcurrentincrease[BBBYprevdaysdecrease])
0.4622642
# iii
mean(BBBYcurrentdecrease[BBBYprevdaysincrease])
0.5292234
# iv
mean(BBBYcurrentdecrease[BBBYprevdaysdecrease])
0.5290881

# EBAY
EBAYClose <- EBAY$Adj.Close[-c(1,2)]
EBAYprevClose <- EBAY$Adj.Close[-c(1,nrow(EBAY))]
EBAYprevClose1 <- EBAY$Adj.Close[-c(nrow(EBAY)-1,nrow(EBAY))]

EBAYprevincrease <- ifelse(EBAYprevClose > EBAYprevClose1, 1, 0)

EBAYprevdecrease <- ifelse(EBAYprevClose < EBAYprevClose1, 1, 0)

EBAYcurrentincrease <- ifelse(EBAYClose > EBAYprevClose, 1, 0)

EBAYcurrentdecrease <- ifelse(EBAYClose < EBAYprevClose, 1, 0)

EBAYprevdaysincrease <- which(EBAYprevincrease%in%1)

EBAYprevdaysdecrease <- which(EBAYprevdecrease%in%0)

# i
mean(EBAYcurrentincrease[EBAYprevdaysincrease])
0.5092736
# ii
mean(EBAYcurrentincrease[EBAYprevdaysdecrease])
0.510329
# iii
mean(EBAYcurrentdecrease[EBAYprevdaysincrease])
0.4860896
# iv
mean(EBAYcurrentdecrease[EBAYprevdaysdecrease])
0.4850803

# GME
GMEClose <- GME$Adj.Close[-c(1,2)]
GMEprevClose <- GME$Adj.Close[-c(1,nrow(GME))]
GMEprevClose1 <- GME$Adj.Close[-c(nrow(GME)-1,nrow(GME))]

GMEprevincrease <- ifelse(GMEprevClose > GMEprevClose1, 1, 0)

GMEprevdecrease <- ifelse(GMEprevClose < GMEprevClose1, 1, 0)

GMEcurrentincrease <- ifelse(GMEClose > GMEprevClose, 1, 0)

GMEcurrentdecrease <- ifelse(GMEClose < GMEprevClose, 1, 0)

GMEprevdaysincrease <- which(GMEprevincrease%in%1)

GMEprevdaysdecrease <- which(GMEprevdecrease%in%0)

# i
mean(GMEcurrentincrease[GMEprevdaysincrease])
0.4980665
# ii
mean(GMEcurrentincrease[GMEprevdaysdecrease])
0.5049057
# iii
mean(GMEcurrentdecrease[GMEprevdaysincrease])
0.4880124
# iv
mean(GMEcurrentdecrease[GMEprevdaysdecrease])
0.4807547

# MSFT
MSFTClose <- MSFT$Adj.Close[-c(1,2)]
MSFTprevClose <- MSFT$Adj.Close[-c(1,nrow(MSFT))]
MSFTprevClose1 <- MSFT$Adj.Close[-c(nrow(MSFT)-1,nrow(MSFT))]

MSFTprevincrease <- ifelse(MSFTprevClose > MSFTprevClose1, 1, 0)

MSFTprevdecrease <- ifelse(MSFTprevClose < MSFTprevClose1, 1, 0)

MSFTcurrentincrease <- ifelse(MSFTClose > MSFTprevClose, 1, 0)

MSFTcurrentdecrease <- ifelse(MSFTClose < MSFTprevClose, 1, 0)

MSFTprevdaysincrease <- which(MSFTprevincrease%in%1)

MSFTprevdaysdecrease <- which(MSFTprevdecrease%in%0)

# i
mean(MSFTcurrentincrease[MSFTprevdaysincrease])
0.5030534
# ii
mean(MSFTcurrentincrease[MSFTprevdaysdecrease])
0.5071161
# iii
mean(MSFTcurrentdecrease[MSFTprevdaysincrease])
0.4885496
# iv
mean(MSFTcurrentdecrease[MSFTprevdaysdecrease])
0.4846442

# Question 3 Part 7
# AMD
AMDClose <- AMD$Adj.Close[-c(1,2)]
AMDprevvolume <- AMD$Volume[-c(1,nrow(AMD))]
AMDprevvolume1 <- AMD$Volume[-c(nrow(AMD)-1,nrow(AMD))]
AMDprevvolincrease <- ifelse(AMDprevvolume > AMDprevvolume1, 1, 0)
AMDprevvoldecrease <- ifelse(AMDprevvolume < AMDprevvolume1, 1, 0)
AMDcurrentvolincrease <- ifelse(AMDClose > AMDprevvolume, 1, 0)
AMDcurrentvoldecrease <- ifelse(AMDClose < AMDprevvolume, 1, 0)
AMDprevdaysvolincrease <- which(AMDprevvolincrease%in%1)
AMDprevdaysvoldecrease <- which(AMDprevvoldecrease%in%1)

# i
mean(AMDcurrentincrease[AMDprevdaysvolincrease])
0.5121951
# ii
mean(AMDcurrentincrease[AMDprevdaysvoldecrease])
0.4796073
# iii
mean(AMDcurrentdecrease[AMDprevdaysvolincrease])
0.4575273
# iv
mean(AMDcurrentdecrease[AMDprevdaysvoldecrease])
0.4833837

# BBBY
BBBYClose <- BBBY$Adj.Close[-c(1,2)]
BBBYprevvolume <- BBBY$Volume[-c(1,nrow(BBBY))]
BBBYprevvolume1 <- BBBY$Volume[-c(nrow(BBBY)-1,nrow(BBBY))]
BBBYprevvolincrease <- ifelse(BBBYprevvolume > BBBYprevvolume1, 1, 0)
BBBYprevvoldecrease <- ifelse(BBBYprevvolume < BBBYprevvolume1, 1, 0)
BBBYcurrentvolincrease <- ifelse(BBBYClose > BBBYprevvolume, 1, 0)
BBBYcurrentvoldecrease <- ifelse(BBBYClose < BBBYprevvolume, 1, 0)
BBBYprevdaysvolincrease <- which(BBBYprevvolincrease%in%1)
BBBYprevdaysvoldecrease <- which(BBBYprevvoldecrease%in%1)

# i
mean(BBBYcurrentincrease[BBBYprevdaysvolincrease])
0.497105
# ii
mean(BBBYcurrentincrease[BBBYprevdaysvoldecrease])
0.4976994
# iii
mean(BBBYcurrentdecrease[BBBYprevdaysvolincrease])
0.4913151
# iv
mean(BBBYcurrentdecrease[BBBYprevdaysvoldecrease])
0.4953988

# EBAY
EBAYClose <- EBAY$Adj.Close[-c(1,2)]
EBAYprevvolume <- EBAY$Volume[-c(1,nrow(EBAY))]
EBAYprevvolume1 <- EBAY$Volume[-c(nrow(EBAY)-1,nrow(EBAY))]
EBAYprevvolincrease <- ifelse(EBAYprevvolume > EBAYprevvolume1, 1, 0)
EBAYprevvoldecrease <- ifelse(EBAYprevvolume < EBAYprevvolume1, 1, 0)
EBAYcurrentvolincrease <- ifelse(EBAYClose > EBAYprevvolume, 1, 0)
EBAYcurrentvoldecrease <- ifelse(EBAYClose < EBAYprevvolume, 1, 0)
EBAYprevdaysvolincrease <- which(EBAYprevvolincrease%in%1)
EBAYprevdaysvoldecrease <- which(EBAYprevvoldecrease%in%1)

# i
mean(EBAYcurrentincrease[EBAYprevdaysvolincrease])
0.5103563
# ii
mean(EBAYcurrentincrease[EBAYprevdaysvoldecrease])
0.5191424
# iii
mean(EBAYcurrentdecrease[EBAYprevdaysvolincrease])
0.4871582
# iv
mean(EBAYcurrentdecrease[EBAYprevdaysvoldecrease])
0.4732006

# GME 
GMEClose <- GME$Adj.Close[-c(1,2)]
GMEprevvolume <- GME$Volume[-c(1,nrow(GME))]
GMEprevvolume1 <- GME$Volume[-c(nrow(GME)-1,nrow(GME))]
GMEprevvolincrease <- ifelse(GMEprevvolume > GMEprevvolume1, 1, 0)
GMEprevvoldecrease <- ifelse(GMEprevvolume < GMEprevvolume1, 1, 0)
GMEcurrentvolincrease <- ifelse(GMEClose > GMEprevvolume, 1, 0)
GMEcurrentvoldecrease <- ifelse(GMEClose < GMEprevvolume, 1, 0)
GMEprevdaysvolincrease <- which(GMEprevvolincrease%in%1)
GMEprevdaysvoldecrease <- which(GMEprevvoldecrease%in%1)

# i
mean(GMEcurrentincrease[GMEprevdaysvolincrease])
0.4966443
# ii
mean(GMEcurrentincrease[GMEprevdaysvoldecrease])
0.5306586
# iii
mean(GMEcurrentdecrease[GMEprevdaysvolincrease])
0.4865772
# iv
mean(GMEcurrentdecrease[GMEprevdaysvoldecrease])
0.4602574

# MSFT
MSFTClose <- MSFT$Adj.Close[-c(1,2)]
MSFTprevvolume <- MSFT$Volume[-c(1,nrow(MSFT))]
MSFTprevvolume1 <- MSFT$Volume[-c(nrow(MSFT)-1,nrow(MSFT))]
MSFTprevvolincrease <- ifelse(MSFTprevvolume > MSFTprevvolume1, 1, 0)
MSFTprevvoldecrease <- ifelse(MSFTprevvolume < MSFTprevvolume1, 1, 0)
MSFTcurrentvolincrease <- ifelse(MSFTClose > MSFTprevvolume, 1, 0)
MSFTcurrentvoldecrease <- ifelse(MSFTClose < MSFTprevvolume, 1, 0)
MSFTprevdaysvolincrease <- which(MSFTprevvolincrease%in%1)
MSFTprevdaysvoldecrease <- which(MSFTprevvoldecrease%in%1)

# i
mean(MSFTcurrentincrease[MSFTprevdaysvolincrease])
0.509434
# ii
mean(MSFTcurrentincrease[MSFTprevdaysvoldecrease])
0.5316847
# iii
mean(MSFTcurrentdecrease[MSFTprevdaysvolincrease])
0.4774405
# iv
mean(MSFTcurrentdecrease[MSFTprevdaysvoldecrease])
0.4613601

# Question 4
st <- rnorm(length(r2), 2, 2)
regression <- lm(r2~st)
summary(regression)

regression

set.seed(20682484)
pv <- matrix(0,1000,2)

for(i in 1:1000){
  stm <- rnorm(length(r2), 2, 2)
  regressionm <- lm(r2~stm)
  pv[i,1] <- ifelse(summary(regressionm)$coefficients[1,4] < 0.05, 1, 0)
  pv[i,2] <- ifelse(summary(regressionm)$coefficients[2,4] < 0.05, 1, 0)
}

int <- sum(pv[,1])
slo <- sum(pv[,2])

summary(int)
summary(slo)






















