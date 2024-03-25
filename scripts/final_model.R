

# 0) Packs --------------------------------------------------------------

source("scripts/packs.R")
library("strucchange")
library("vars")
library("tseries")
library("forecast")
library("performance")
library("urca")


# 1) Data ----------------------------------------------------------------
data <- read_excel("inputs/data.xlsx",
                    sheet = "dados") %>%
  clean_names() %>%
  mutate(time = c(seq(
    as.Date("1998-01-01"), as.Date("2022-06-01"), by = "month"
  ))) %>%
  filter(time >= "2010-01-01") %>%
  na.omit() %>%
  dplyr::select(-2) %>%
  mutate(
    ipca = log(ipca),
    exchange_rate = log(cambio_medio_venda),
    oil = log(petroleo_brent_futuros),
    pim = log(pim_dessaz)
  ) %>%
  dplyr::select(time, ipca, exchange_rate, oil, pim)



ipca <- data %>% dplyr::select(ipca) %>%
  ts()

exchange_rate <- data %>% dplyr::select(exchange_rate) %>%
  ts()

oil <- data %>% dplyr::select(oil) %>%
  ts()

pim <- data %>% dplyr::select(pim) %>%
  ts()

ps <- cbind(ipca, exchange_rate, oil, pim)





# 2) Pre modeling Analysis---------------------------------------------------------------


# 2.1) Stationarity ---------------------------------------------------

# IPCA
adf.test(ipca)
# As P > 0.05, we do not reject H0 and the series does not present stationarity

sta_ipca <- diff(ipca, differences = 1)
adf.test(sta_ipca)
pacf(sta_ipca)
acf(sta_ipca)

# exchange_rate
adf.test(exchange_rate)
# As P > 0.05, we do not reject H0 and the series does not present stationarity

sta_exchange_rate <- diff(exchange_rate, differences = 1)
adf.test(sta_exchange_rate)
pacf(sta_exchange_rate)
acf(sta_exchange_rate)

# oil
adf.test(oil)

# As P > 0.05, we do not reject H0 and the series does not present stationarity

sta_oil <- diff(oil, differences = 1)
adf.test(sta_oil)
pacf(sta_oil)
acf(sta_oil)

# PIM
adf.test(pim)

# As P > 0.05, we do not reject H0 and the series does not present stationarity
sta_pim <- diff(pim, differences = 1)
adf.test(sta_pim)
acf(sta_pim)
pacf(pim)


pt <- cbind(sta_ipca,sta_exchange_rate,sta_oil,sta_pim)

# 2.2) Lag selection----------------------------------------------------------

lag_sec <- VARselect(pt, lag.max = 12, type = "const")
lag_sec$selection



# 3) Model ---------------------------------------------------------------

model <- VAR(pt,
             p = 1,
             season = NULL,
             exogen = NULL )



summary(model)




# 4) Diagnosis ---------------------------------------------------------


# 4.1) Serial Correlation --------------------------------------------------

serial <-  serial.test(model, lags.pt = 12, type = "PT.asymptotic") %>%
  print()

# As p> 0.05 there is no evidence of serial correlation


# 4.2) Heteroscedasticity ------------------------------------------------

arch <-  arch.test(model, lags.multi = 12, multivariate.only = TRUE) %>%
  print()

# As p> 0.05 there is no evidence of heteroscedasticity


# 4.2) Residual Normality --------------------------------------------

normal <- normality.test(model, multivariate.only = TRUE) %>%
  print()

# As p< 0.05 in the JB test, there is evidence that the distribution of residuals is not normal

# 4.3) Structural Break --------------------------------------------------

structural <- stability(model, type = "OLS-CUSUM") 

par(mar = c(0.5, 0.5, 0.5, 0.5))

plot(structural)



# 4.4) Granger causality test---------------------------------------

granger_ipca <- causality(model,cause = "sta_ipca") %>% print()
granger_exchange_rate <- causality(model,cause = "sta_exchange_rate") %>% print()
granger_oil <- causality(model,cause = "sta_oil") %>% print()
granger_pim <- causality(model,cause = "sta_pim") %>% print()




# 4.41) IPCA x exchange_rate -----------------------------------------------------


caus_ipca_exchange_rate <-
  grangertest(sta_ipca ~ sta_exchange_rate, order = 12, data = pt) %>%
  print()

# As p > 0.05, we do not reject H0,
# therefore, there is no evidence that the IPCA causes exchange rate in the Granger sense


caus_sta_exchange_rate_sta_ipca <-
  grangertest(sta_exchange_rate ~ sta_ipca, order = 12, data = pt) %>%
  print()

# As p < 0.05, we reject H0,
# therefore, there is evidence that the exchange rate causes sta_ipca in the Granger sense

# 4.42) sta_ipca x sta_oil -----------------------------------------------------

caus_sta_ipca_petro <-
  grangertest(sta_ipca ~ sta_oil, order = 1, data = pt) %>%
  print()

# As p < 0.05, we reject H0,
# therefore, there is evidence that sta_ipca causes sta_oil in the Granger sense

caus_petro_sta_ipca <-
  grangertest(sta_oil ~ sta_ipca, order = 1, data = pt) %>%
  print()

# As p > 0.05, we do not reject H0,
# therefore, there is no evidence that sta_oil causes sta_ipca in the Granger sense

# 4.43) sta_ipca x sta_pim -----------------------------------------------------

caus_sta_ipca_sta_pim <- grangertest(sta_ipca ~ sta_pim, order = 1, data = pt) %>%
  print()

# As p > 0.05, we do not reject H0,
# therefore, there is no evidence that sta_ipca causes sta_pim in the Granger sense

caus_sta_pim_sta_ipca <- grangertest(sta_pim ~ sta_ipca, order = 1, data = pt) %>%
  print()

# As p < 0.05, we reject H0,
# therefore, there is evidence that sta_pim causes sta_ipca in the Granger sense


# 4.44) sta_exchange_rate x sta_oil -------------------------------------------------

caus_sta_exchange_rate_sta_oil <-
  grangertest(sta_exchange_rate ~ sta_oil, order = 1, data = pt) %>%
  print()

# As p < 0.05, we reject H0,
# therefore, there is evidence that sta_exchange_rate causes sta_oil in the Granger sense

caus_sta_oil_sta_exchange_rate <-
  grangertest(sta_oil ~ sta_exchange_rate, order = 1, data = pt) %>%
  print()

# As p > 0.05, we do not reject H0,
# therefore, there is no evidence that sta_oil causes sta_exchange_rate in the Granger sense


# 4.45) sta_exchange_rate x sta_pim -------------------------------------------------

caus_sta_exchange_rate_sta_pim <-
  grangertest(sta_exchange_rate ~ sta_pim, order = 1, data = pt) %>%
  print()

# As p > 0.05, we do not reject H0,
# therefore, there is no evidence that sta_exchange_rate causes sta_pim in the Granger sense

caus_sta_pim_sta_exchange_rate <-
  grangertest(sta_pim ~ sta_exchange_rate, order = 1, data = pt) %>%
  print()

# As p < 0.05, we reject H0,
# therefore, there is evidence that sta_pim causes sta_exchange_rate in the granger sense

# 4.45) sta_oil x sta_pim -------------------------------------------------

caus_sta_oil_sta_pim <-
  grangertest(sta_oil ~ sta_pim, order = 1, data = pt) %>%
  print()

# As p > 0.05, we do not reject H0,
# therefore, there is no evidence that sta_oil causes sta_pim in the Granger sense


caus_sta_pim_petrole <-
  grangertest(sta_pim ~ sta_oil, order = 1, data = pt) %>%
  print()

# As p < 0.05, we reject H0,
# therefore, there is evidence that sta_pim causes sta_oil in the Granger sense



# 4.5) Impulse-Response ---------------------------------------------------


# 4.51) IPCA x exchange_rate -----------------------------------------------------


ip_ipca_exchange_rate <- irf(
  model,
  impulse = "sta_ipca",
  response = "sta_exchange_rate",
  n.ahead = 12,
  boot = TRUE) %>%
  plot()

# shock on ipca, response on exchange_rate

# Ipca generates a positive effect on exchange_rate


ip_exchange_rate_ipca <- irf(
  model,
  impulse = "sta_exchange_rate",
  response = "sta_ipca",
  n.ahead = 12,
  boot = TRUE) %>% plot()


# shock on ipca, response on exchange_rate


# 4.52) IPCA x oil -----------------------------------------------------


ip_ipca_oil <-
  irf(
    model,
    impulse = "sta_ipca",
    response = "sta_oil",
    n.ahead = 12,
    boot = TRUE
  ) %>%
  plot()

ip_oil_ipca <-
  irf(
    model,
    impulse = "sta_oil",
    response = "sta_ipca",
    n.ahead = 12,
    boot = TRUE
  ) %>%
  plot()

# 4.53) IPCA x PIM -----------------------------------------------------


ip_ipca_pim <-
  irf(
    model,
    impulse = "sta_ipca",
    response = "sta_pim",
    n.ahead = 12,
    boot = TRUE
  ) %>%
  plot()

ip_pim_ipca <-
  irf(
    model,
    impulse = "sta_pim",
    response = "sta_ipca",
    n.ahead = 12,
    boot = TRUE
  ) %>%
  plot()

# 4.53) exchange_rate x oil -----------------------------------------------------


ip_exchange_rate_oil <-
  irf(
    model,
    impulse = "sta_exchange_rate",
    response = "sta_oil",
    n.ahead = 12,
    boot = TRUE
  ) %>%
  plot()

ip_oil_exchange_rate <-
  irf(
    model,
    impulse = "sta_oil",
    response = "sta_exchange_rate",
    n.ahead = 12,
    boot = TRUE
  ) %>%
  plot()

# 4.54) exchange_rate x PIM -----------------------------------------------------


ip_exchange_rate_pim <-
  irf(
    model,
    impulse = "sta_exchange_rate",
    response = "sta_pim",
    n.ahead = 12,
    boot = TRUE
  ) %>%
  plot()

ip_pim_exchange_rate <-
  irf(
    model,
    impulse = "sta_pim",
    response = "sta_exchange_rate",
    n.ahead = 12,
    boot = TRUE
  ) %>%
  plot()

# 4.54) oil x PIM -----------------------------------------------------


ip_oil_pim <-
  irf(
    model,
    impulse = "sta_oil",
    response = "sta_pim",
    n.ahead = 12,
    boot = TRUE
  ) %>%
  plot()

ip_pim_oil <-
  irf(
    model,
    impulse = "sta_pim",
    response = "sta_oil",
    n.ahead = 12,
    boot = TRUE
  ) %>%
  plot()


#4.6) Variance Decomposition
# Impact of each variable on the fluctuation of the others ------------------

par(mar = c(0.5, 0.5, 0.5, 0.5))


dec_var <- fevd(model, n.ahead = 12)

