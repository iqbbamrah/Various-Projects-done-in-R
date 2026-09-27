# Various Projects Done in R

| Project | Course | Topic |
|---|---|---|
| [ECON 421 Term Project](#econ-421-term-project-binary-and-censored-outcome-models) | ECON 421 | Linear probability vs. probit models of labour-force participation, plus censored-regression theory and simulation |
| [Assignment 1](#assignment-1-descriptive-statistics-of-stock-prices-and-returns) | ECON 423 | Distribution, autocorrelation and volatility of five stocks' prices and returns |
| [Assignment 2](#assignment-2-arma-simulation-and-ar-model-fitting) | ECON 423 | Simulating AR/MA processes, and fitting and forecasting AR models of stock returns |
| [Assignment 3](#assignment-3-archgarch-volatility-modelling) | ECON 423 | Simulating a GARCH process, then fitting ARCH/GARCH models and comparing volatility forecasts |

---

## ECON 421 Term Project: binary and censored outcome models

### Problem
How should a binary outcome (labour-force participation) be modeled, and why is a linear probability model problematic for it? And how are regression models estimated when the outcome is censored at a lower bound, an upper bound, or both?

### Data
- **Task 1:** 1,000 individuals (an individually assigned dataset). Outcome `lfprt` is a 0/1 indicator of labour-force participation, and the regressors are years of work experience (`expr`), years of education (`educ`) and `age`.
- **Task 2:** 1,000 simulated observations of y = 2x + ε with x ~ N(0, 10²), with error standard deviation 6 and then 1.

### Methodology
- **Linear probability model (LPM):** estimated and interpreted, including why it's problematic for a binary outcome (the relationship is rarely linear, and fitted "probabilities" can fall outside 0–1).
- **Probit:** derived the latent-variable set-up, log-likelihood, ML first-order conditions, asymptotic distribution, and marginal effects, then estimated the model in R and compared it against the LPM (fitted values, AIC).
- **Censored regression:** derived the likelihood, ML first-order conditions for β and σ², and asymptotic distribution for the left-, right- and interval-censored cases of y\* = xβ + ε, ε ~ N(0, σ²). Simulated data and tested the outcome variance with a one-sample asymptotic variance test (`asympTest::asymp.test`).
- **Tools:** `glm`, `margins`, `stargazer`, `asympTest`.

### Results
- **LPM** (experience, education, age): experience +0.078, education +0.051, age −0.023, all significant at the 1% level. Its fitted values range from **−0.84 to 1.29**, i.e. impossible probabilities. The probit's stay within (0, 1).
- **Probit** (experience and education): coefficients 0.374 and 0.239. Average marginal effects: each extra year of experience raises the probability of participation by about **7.6 percentage points** (SE 0.3), and each extra year of education by about **4.9 points** (SE 0.3).
- **Model fit:** on the same regressors, the probit has the lower AIC (743.1 vs. 809.1).
- **Probit with age included did not converge:** standard errors blew up and the residual deviance was essentially zero, the signature of regressors that perfectly predict the outcome in this sample.
- **Censored-regression simulation:** estimated outcome variances of about **444** (error SD 6) and **403** (error SD 1). The censoring bounds (−5, +5) are set up in the script, but these summaries are on the outcome before censoring.

### Key takeaways
- The LPM's out-of-range fitted values make its weakness concrete: for binary outcomes, the probit is the better-specified model and fits better (lower AIC).
- Marginal effects, not raw probit coefficients, are the interpretable quantities.
- Non-convergence with near-zero deviance is a symptom of perfect separation in the data, not a model to report, which is why the comparison is made without age.

---

## Assignment 1: descriptive statistics of stock prices and returns

### Problem
What do the distributions, autocorrelation and volatility of real stock prices and returns look like, and do they match the textbook assumption of normally distributed, independent returns?

### Data
Daily adjusted-close prices and trading volume for five stocks: AMD, BBBY (Bed Bath & Beyond), EBAY, GME (GameStop) and MSFT.

### Methodology
- **Prices and returns:** histogram, mean, variance, skewness, kurtosis and ACF of adjusted close and of daily returns (100 × log change in adjusted close).
- **Squared returns:** histograms and ACFs, to look for volatility clustering.
- **Returns and volume:** correlation test between each stock's return and its trading volume.
- **Up/down-day frequencies:** how often a stock rises or falls conditional on the previous day's price or volume move.
- **Monte Carlo:** 1,000 replications regressing BBBY's returns on pure random noise, counting how often the coefficients come out "significant" at 5%.

### Results
Return kurtosis ranges from about **6.4 (MSFT) to 36.7 (GME)**: AMD 12.1, BBBY 21.0, EBAY 8.2. All are well above the value of 3 for a normal distribution. The full numeric output is pasted into the script beneath the code that produced it.

### Key takeaways
- Stock returns are heavy-tailed, far more so for the "meme" stocks (GME, BBBY) than for MSFT, so normal-distribution assumptions understate extreme moves.
- The Monte Carlo shows how often pure noise produces "significant" regression coefficients, a caution against over-reading single significant results.

---

## Assignment 2: ARMA simulation and AR model fitting

### Problem
How do AR and MA processes behave, and how well do simple AR models describe and forecast real stock returns?

### Data
Simulated ARMA, AR(1) and MA(1) series, plus AMD and BBBY daily returns.

### Methodology
- **Simulated ARMA processes:** AR coefficients 0.8 and −0.6 (n = 20), with and without an MA(1) term, and the sample ACF of each.
- **Simulated AR(1) and MA(1):** 1,000 draws each with coefficient 0.2021, with sample moments, autocorrelations and autocovariances out to lag 5.
- **Real data:** AR(1) through AR(5) fit to AMD and BBBY returns with `arima`, plus a Yule–Walker AR(5) (`ar.yw`). In-sample accuracy measures are compared across models, with a 5-step-ahead forecast from each.
- **Tools:** `arima.sim`, `arima`, `ar.yw`, `forecast`.

### Results
- Simulated AR(1): sample mean ≈ 0.031, variance ≈ 1.068. Simulated MA(1): mean ≈ 0.012, variance ≈ 0.982, both close to their theoretical values.
- The five AR specifications' in-sample accuracy measures are tabulated side by side for each stock, with 5-step-ahead forecasts.

### Key takeaways
- Simulation makes the theoretical ACF signatures of AR vs. MA processes concrete, which is what model identification relies on.
- Comparing AR orders on accuracy measures, rather than picking one order up front, is the basis for choosing a forecasting model.

---

## Assignment 3: ARCH/GARCH volatility modelling

### Problem
Can ARCH/GARCH models capture the volatility clustering in stock returns, and which specification forecasts volatility best?

### Data
2,000 observations simulated from a GARCH(1,1) process, plus daily returns for AMD, BBBY, EBAY, GME and MSFT.

### Methodology
- **Monte Carlo:** simulated GARCH(1,1) (ω = 0.02, α = 0.05, β = 0.8, standard-normal shocks), fit ARCH(1), ARCH(2), GARCH(1,1) and GARCH(2,2) with `fGarch::garchFit`, then refit on the first half and produced 1,000-step volatility forecasts to compare against the second half.
- **Real data:** for each stock, fit ARCH(1), ARCH(2), GARCH(1,1), GARCH(1,2) and GARCH(2,2), plotted fitted conditional variance against squared returns, produced 60-day-ahead volatility forecasts, and compared models on two mean-squared-error measures (MSE1 and MSE2).
- **Tools:** `fGarch`, `forecast`.

### Results
For each of the five stocks, the script produces fitted-variance plots against squared returns, 60-day volatility forecasts, and MSE1/MSE2 for every ARCH/GARCH specification, giving a per-stock ranking of forecast accuracy.

### Key takeaways
- Fitting models to data simulated from a *known* GARCH process shows whether the estimation recovers the true structure, before trusting it on real returns.
- Volatility is forecastable even when returns themselves aren't, which is why GARCH-family models are the standard tool for risk.

---

## How to run

1. Install the R packages: `forecast`, `fGarch`, `margins`, `stargazer`, `asympTest`, and `moments` (for `skewness()` / `kurtosis()`).
2. Supply the data, which isn't in this repo. Assignments 1 and 2 expect the five stock data frames (`AMD`, `BBBY`, `EBAY`, `GME`, `MSFT`, with `Adj.Close` and `Volume` columns) already loaded in the session. Assignment 3 and the ECON 421 project read CSVs from absolute paths, so update those to your own files.
3. Run each script top to bottom.

## Repo structure

```
├── ECON 421 Term Project Iqbal Bamrah.R     # ECON 421 code
├── ECON 421 Term Project Iqbal Bamrah.pdf   # ECON 421 write-up (scanned handwritten derivations + R output)
├── Assignment 1 Stock Data.R                # ECON 423: descriptive statistics (results pasted beneath the code)
├── Assignment 2 Data.R                      # ECON 423: ARMA simulation and AR fitting
├── Assignment 3 Data.R                      # ECON 423: ARCH/GARCH volatility modelling
└── README.md
```

