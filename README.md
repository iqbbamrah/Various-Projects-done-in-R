# Various Projects Done in R

R projects from my economics coursework at the University of Waterloo: a term project on **binary-outcome and censored-data models** (ECON 421) and three assignments on **financial time series** — return statistics, ARMA models and ARCH/GARCH volatility models (ECON 423).

| Project | Course | Topic | Main tools |
|---|---|---|---|
| [ECON 421 Term Project](#econ-421-term-project--binary-and-censored-outcome-models) | ECON 421 | Linear probability vs. probit models of labour-force participation; censored-regression (Tobit-type) theory and simulation | `glm`, `margins`, `stargazer`, `asympTest` |
| [Assignment 1](#assignment-1--descriptive-statistics-of-stock-prices-and-returns) | ECON 423 | Distribution, autocorrelation and volatility of five stocks' prices and returns | base R |
| [Assignment 2](#assignment-2--arma-simulation-and-ar-model-fitting) | ECON 423 | Simulating AR/MA processes; fitting and forecasting AR models of stock returns | `arima.sim`, `arima`, `ar.yw`, `forecast` |
| [Assignment 3](#assignment-3--archgarch-volatility-modelling) | ECON 423 | Simulating a GARCH process; fitting ARCH/GARCH models to stock returns and comparing volatility forecasts | `fGarch` (`garchFit`), `forecast` |

The ECON 423 final project — forecasting stock returns and comparing them with Twitter sentiment — is in its own repo: [Time-Series-Stock-Price-Forecasting-Project](https://github.com/iqbbamrah/Time-Series-Stock-Price-Forecasting-Project).

---

## ECON 421 Term Project — binary and censored outcome models

Files: [`ECON 421 Term Project Iqbal Bamrah.R`](<ECON 421 Term Project Iqbal Bamrah.R>) (code) and [`ECON 421 Term Project Iqbal Bamrah.pdf`](<ECON 421 Term Project Iqbal Bamrah.pdf>) (write-up: handwritten derivations plus screenshots of the R output; it's a scanned document, so its text isn't searchable).

### Task 1 — Labour-force participation: linear probability model vs. probit

**Data:** 1,000 individuals. The outcome `lfprt` is a 0/1 indicator of labour-force participation; the regressors are years of work experience (`expr`), years of education (`educ`) and `age`.

**What it covers:**
- Interpreting the linear probability model (LPM) and why it's problematic for a binary outcome (the relationship is rarely linear, and fitted "probabilities" can fall outside 0–1).
- Deriving the probit model: the latent-variable set-up, the log-likelihood, the first-order conditions of the ML estimator, its asymptotic distribution, and the marginal effects.
- Estimating both models in R and comparing them.

**Results:**
- **LPM** (experience, education, age): experience +0.078, education +0.051, age −0.023, all significant at the 1% level.
- **The LPM produces impossible probabilities:** its fitted values range from −0.84 to 1.29, whereas the probit's fitted probabilities stay within (0, 1).
- **Probit** (experience and education): coefficients 0.374 and 0.239. Average marginal effects: each extra year of experience raises the probability of participation by about **7.6 percentage points** (SE 0.3) and each extra year of education by about **4.9 points** (SE 0.3).
- **Model comparison on the same regressors** (experience and education only): the probit has the lower AIC (743.1 vs. 809.1), so it fits the data better than the LPM.
- **Probit with age included did not converge.** The estimates blew up (standard errors in the tens of thousands or more) and the residual deviance was essentially zero — the signature of a sample where the regressors perfectly predict the outcome. The write-up flags this as an "error within the data" and compares the models without age for that reason.

### Task 2 — Censored regression

**Theory:** the latent model y\* = xβ + ε with ε ~ N(0, σ²), where the observed y is censored at a lower bound, an upper bound, or both. The write-up derives the likelihood, the first-order conditions of the ML estimators of β and σ², and the asymptotic distribution for the **left-censored, right-censored and interval-censored** cases.

**Simulation:** the script simulates 1,000 observations of y = 2x + ε with x ~ N(0, 10²), first with errors of standard deviation 6 and then with standard deviation 1, and tests the variance of the outcome with a one-sample asymptotic variance test (`asympTest::asymp.test`). The estimated variances are about **444** and **403** respectively. (The censoring bounds of −5 and +5 are set up in the script, but the reported summaries and variance tests are on the simulated outcome before censoring.)

---

## Assignment 1 — descriptive statistics of stock prices and returns

File: [`Assignment 1 Stock Data.R`](<Assignment 1 Stock Data.R>)

**Data:** daily adjusted-close prices and trading volume for five stocks: AMD, BBBY (Bed Bath & Beyond), EBAY, GME (GameStop) and MSFT.

**What it does:**
- **Prices:** histogram, mean, variance, skewness and kurtosis of each stock's adjusted close, plus its autocorrelation function (ACF).
- **Returns:** the same statistics for daily returns, computed as 100 × the log change in adjusted close. Return kurtosis ranges from about **6.4 (MSFT) to 36.7 (GME)** — AMD 12.1, BBBY 21.0, EBAY 8.2 — well above the value of 3 for a normal distribution, i.e. heavy-tailed returns.
- **Squared returns:** histograms and ACFs, to look for volatility clustering.
- **Returns and volume:** a correlation test between each stock's return and its trading volume.
- **Up/down-day frequencies:** how often a stock rises or falls conditional on the previous day's price move or volume move.
- **Monte Carlo:** a 1,000-replication simulation (fixed seed) that regresses BBBY's returns on pure random noise and counts how often the intercept and slope come out "significant" at the 5% level.

The numeric results from this script are pasted into it, right beneath the code that produced them.

## Assignment 2 — ARMA simulation and AR model fitting

File: [`Assignment 2 Data.R`](<Assignment 2 Data.R>)

- **Simulated ARMA processes (Q5):** an ARMA process with AR coefficients 0.8 and −0.6 (n = 20), with and without an MA(1) term, and the sample ACF of each.
- **Simulated AR(1) and MA(1) (Q6–Q7):** 1,000 draws each with coefficient 0.2021. Sample mean and variance were about 0.031 and 1.068 for the AR(1), and 0.012 and 0.982 for the MA(1), and the script computes the sample autocorrelations and autocovariances out to lag 5.
- **Real data (Q8):** AMD and BBBY daily returns. AR(1) through AR(5) models are fit with `arima`, a Yule–Walker AR(5) fit (`ar.yw`) is also run, the in-sample accuracy measures of the five `arima` models are tabulated for comparison, and each of those models produces a 5-step-ahead forecast.

## Assignment 3 — ARCH/GARCH volatility modelling

File: [`Assignment 3 Data.R`](<Assignment 3 Data.R>)

- **Monte Carlo (Q2):** simulates 2,000 observations from a GARCH(1,1) process (ω = 0.02, α = 0.05, β = 0.8, standard-normal shocks) and fits ARCH(1), ARCH(2), GARCH(1,1) and GARCH(2,2) models to it with `fGarch::garchFit`. It then splits the sample in half, refits ARCH(1), GARCH(1,1) and GARCH(2,2) on the first 1,000 observations, and produces 1,000-step volatility forecasts from each to set against the second half.
- **Real data (Q3):** for each of the five stocks (AMD, BBBY, EBAY, GME, MSFT), fits ARCH(1), ARCH(2), GARCH(1,1), GARCH(1,2) and GARCH(2,2) to daily returns; plots each model's fitted conditional variance against squared returns; produces 60-day-ahead volatility forecasts; and computes two mean-squared-error measures (MSE1 and MSE2) per model to compare forecast accuracy.

---

## Running the code

- **The data isn't in this repository.** Assignments 1 and 2 assume the five stock data frames (`AMD`, `BBBY`, `EBAY`, `GME`, `MSFT`, with `Adj.Close` and `Volume` columns) are already loaded in the R session. Assignment 3 and the ECON 421 project read CSVs from absolute paths on my own machine (the ECON 421 file is an individually assigned dataset with columns `lfprt`, `expr`, `educ` and `age`). To run any of these you'll need to supply equivalent data and update the paths.
- **R packages used:** `forecast` (for `arima` accuracy and `forecast()`), `fGarch`, `margins`, `stargazer` and `asympTest`, plus a package that provides `skewness()` and `kurtosis()` (e.g. `moments`).
