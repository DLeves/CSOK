CSOK TDK
================

- [Kezdő beállítások](#kezdő-beállítások)
  - [Libraryk](#libraryk)
  - [Adatok beolvasása, átalakítása](#adatok-beolvasása-átalakítása)
    - [CSOK dummy meghatározása](#csok-dummy-meghatározása)
    - [Adatok pótlása GAM-al](#adatok-pótlása-gam-al)
- [Területi autokorreláció](#területi-autokorreláció)
  - [Szomszédsági mátrixok](#szomszédsági-mátrixok)
    - [Adatok felbontása Moranhoz és
      Gearyhez](#adatok-felbontása-moranhoz-és-gearyhez)
    - [Szomszédsági mátrixok
      létrehozása](#szomszédsági-mátrixok-létrehozása)
  - [Területi autokorreláció évente](#területi-autokorreláció-évente)
- [Leíró stat](#leíró-stat)
  - [Korrelációk](#korrelációk)
- [Panelmodell](#panelmodell)
  - [Tesztek](#tesztek)
  - [Nemtudom mi legyen a cím itt](#nemtudom-mi-legyen-a-cím-itt)
  - [Modellek](#modellek)
    - [Summary-k](#summary-k)
    - [AIC, BIC](#aic-bic)
- [SEM](#sem)

# Kezdő beállítások

## Libraryk

``` r
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(plm)
library(splm)
library(sp)
library(spdep)
library(mgcv)
library(lmtest)
library(psych)
library(ppcor)
library(lavaan)
```

## Adatok beolvasása, átalakítása

``` r
df = read.csv("Data/ExtendedDataset.csv")

df$EV = as.factor(df$EV)
df$JARAS_NEV = as.factor(df$JARAS_NEV)
```

### CSOK dummy meghatározása

Ez a medián a CSOK-os években

``` r
median(df$CSOK[df$EV > "2015"])
```

    ## Warning in Ops.factor(df$EV, "2015"): '>' not meaningful for factors

    ## [1] NA

A mediánnál elválasztva:

``` r
cuts = c(-Inf, 1, 75, Inf)
labs = c("nincs", "alacsony", "magas")

df$CSOKTREND = cut(df$CSOK, breaks = cuts, labels = labs, include.lowest = T)

rm(cuts, labs)
```

### Adatok pótlása GAM-al

``` r
gammodel = gam(LAKAS ~ s(X,Y, by = EV, k = 8), family = "gaussian", data = df, method = "REML")

predicted = predict(gammodel, df)
df$LAKAS_PRED = df$LAKAS

for (i in 1:nrow(df)) {
  if(is.na(df$LAKAS[i])){
    if(predicted[i] > 0){
      df$LAKAS_PRED[i] = predicted[i]
    }else{
      df$LAKAS_PRED[i] = 0
    }
  }
}

rm(i, predicted, gammodel)
```

# Területi autokorreláció

## Szomszédsági mátrixok

### Adatok felbontása Moranhoz és Gearyhez

``` r
df2012 = filter(df, EV == 2012)
df2013 = filter(df, EV == 2013)
df2014 = filter(df, EV == 2014)
df2015 = filter(df, EV == 2015)
df2016 = filter(df, EV == 2016)
df2017 = filter(df, EV == 2017)
df2018 = filter(df, EV == 2018)
df2019 = filter(df, EV == 2019)
df2020 = filter(df, EV == 2020)
df2021 = filter(df, EV == 2021)
```

### Szomszédsági mátrixok létrehozása

``` r
listw32 = nb2listw(dnearneigh(df2016[,c(9,10)], 0, 32, longlat = T))
listw36 = nb2listw(dnearneigh(df2016[,c(9,10)], 0, 36, longlat = T))
listw40 = nb2listw(dnearneigh(df2016[,c(9,10)], 0, 40, longlat = T))
```

## Területi autokorreláció évente

``` r
# Moran, Geary 2016
moran(df2016$LAKAS_PRED, listw32, 175, 174, zero.policy = NULL, NAOK = F)
moran(df2016$LAKAS_PRED, listw36, 175, 174, zero.policy = NULL, NAOK = F)
moran(df2016$LAKAS_PRED, listw40, 175, 174, zero.policy = NULL, NAOK = F)
geary(df2016$LAKAS_PRED, listw32, 175, 174, Szero(listw32))
geary(df2016$LAKAS_PRED, listw36, 175, 174, Szero(listw36))
geary(df2016$LAKAS_PRED, listw40, 175, 174, Szero(listw40))

# Moran, Geary 2017
moran(df2017$LAKAS_PRED, listw32, 175, 174, zero.policy = NULL, NAOK = F)
moran(df2017$LAKAS_PRED, listw36, 175, 174, zero.policy = NULL, NAOK = F)
moran(df2017$LAKAS_PRED, listw40, 175, 174, zero.policy = NULL, NAOK = F)
geary(df2017$LAKAS_PRED, listw32, 175, 174, Szero(listw32))
geary(df2017$LAKAS_PRED, listw36, 175, 174, Szero(listw36))
geary(df2017$LAKAS_PRED, listw40, 175, 174, Szero(listw40))

# Moran, Geary 2018
moran(df2018$LAKAS_PRED, listw32, 175, 174, zero.policy = NULL, NAOK = F)
moran(df2018$LAKAS_PRED, listw36, 175, 174, zero.policy = NULL, NAOK = F)
moran(df2018$LAKAS_PRED, listw40, 175, 174, zero.policy = NULL, NAOK = F)
geary(df2018$LAKAS_PRED, listw32, 175, 174, Szero(listw32))
geary(df2018$LAKAS_PRED, listw36, 175, 174, Szero(listw36))
geary(df2018$LAKAS_PRED, listw40, 175, 174, Szero(listw40))

# Moran, Geary 2019
moran(df2019$LAKAS_PRED, listw32, 175, 174, zero.policy = NULL, NAOK = F)
moran(df2019$LAKAS_PRED, listw36, 175, 174, zero.policy = NULL, NAOK = F)
moran(df2019$LAKAS_PRED, listw40, 175, 174, zero.policy = NULL, NAOK = F)
geary(df2019$LAKAS_PRED, listw32, 175, 174, Szero(listw32))
geary(df2019$LAKAS_PRED, listw36, 175, 174, Szero(listw36))
geary(df2019$LAKAS_PRED, listw40, 175, 174, Szero(listw40))

# Moran, Geary 2020
moran(df2020$LAKAS_PRED, listw32, 175, 174, zero.policy = NULL, NAOK = F)
moran(df2020$LAKAS_PRED, listw36, 175, 174, zero.policy = NULL, NAOK = F)
moran(df2020$LAKAS_PRED, listw40, 175, 174, zero.policy = NULL, NAOK = F)
geary(df2020$LAKAS_PRED, listw32, 175, 174, Szero(listw32))
geary(df2020$LAKAS_PRED, listw36, 175, 174, Szero(listw36))
geary(df2020$LAKAS_PRED, listw40, 175, 174, Szero(listw40))
```

# Leíró stat

``` r
describe(df2012$LAKAS)
describe(df2013$LAKAS)
describe(df2014$LAKAS)
describe(df2015$LAKAS)
describe(df2016$LAKAS)
describe(df2017$LAKAS)
describe(df2018$LAKAS)
describe(df2019$LAKAS)
describe(df2020$LAKAS)
describe(df2021$LAKAS)
```

## Korrelációk

``` r
cor(df[,c(4:8,14:18)], use = "complete.obs")
```

    ##                 LAKAS       SZJA       MUNKA   BERUHAZAS        CSOK
    ## LAKAS      1.00000000  0.4524761  0.37681306  0.21970624  0.02798068
    ## SZJA       0.45247607  1.0000000  0.76176321  0.65480917  0.15495230
    ## MUNKA      0.37681306  0.7617632  1.00000000  0.55171493  0.19346516
    ## BERUHAZAS  0.21970624  0.6548092  0.55171493  1.00000000  0.09673088
    ## CSOK       0.02798068  0.1549523  0.19346516  0.09673088  1.00000000
    ## BUNOZES   -0.05715594 -0.2520321 -0.25982415 -0.21406135 -0.08827811
    ## SERTETT   -0.14519939 -0.4786846 -0.51769037 -0.31142199 -0.19991632
    ## PEDAGOGUS  0.23637629  0.1830092  0.07755174  0.16020173 -0.07238756
    ## ORVOS     -0.33984858 -0.3451780 -0.30159493 -0.10513064  0.17688275
    ## GYORVOS   -0.08994859 -0.1652809 -0.12108496 -0.05896689  0.02580778
    ##               BUNOZES    SERTETT   PEDAGOGUS       ORVOS     GYORVOS
    ## LAKAS     -0.05715594 -0.1451994  0.23637629 -0.33984858 -0.08994859
    ## SZJA      -0.25203211 -0.4786846  0.18300918 -0.34517798 -0.16528087
    ## MUNKA     -0.25982415 -0.5176904  0.07755174 -0.30159493 -0.12108496
    ## BERUHAZAS -0.21406135 -0.3114220  0.16020173 -0.10513064 -0.05896689
    ## CSOK      -0.08827811 -0.1999163 -0.07238756  0.17688275  0.02580778
    ## BUNOZES    1.00000000  0.5331466  0.03582450  0.05974800  0.06137758
    ## SERTETT    0.53314655  1.0000000  0.10062095  0.17288650  0.14297398
    ## PEDAGOGUS  0.03582450  0.1006210  1.00000000  0.06395328  0.07404506
    ## ORVOS      0.05974800  0.1728865  0.06395328  1.00000000  0.14647215
    ## GYORVOS    0.06137758  0.1429740  0.07404506  0.14647215  1.00000000

``` r
pcor(na.omit(df[,c(4:8,14:18)]))$estimate
```

    ##                 LAKAS        SZJA       MUNKA   BERUHAZAS        CSOK
    ## LAKAS      1.00000000  0.22025976  0.08802404 -0.09499849  0.03305034
    ## SZJA       0.22025976  1.00000000  0.47049588  0.45316455  0.08470265
    ## MUNKA      0.08802404  0.47049588  1.00000000  0.14035111  0.10061307
    ## BERUHAZAS -0.09499849  0.45316455  0.14035111  1.00000000 -0.05821834
    ## CSOK       0.03305034  0.08470265  0.10061307 -0.05821834  1.00000000
    ## BUNOZES    0.01022799  0.01193103  0.02967921 -0.07237939  0.02450377
    ## SERTETT    0.06318258 -0.16331058 -0.23568456  0.05667894 -0.09959544
    ## PEDAGOGUS  0.20774436  0.16307579 -0.04414352  0.04345751 -0.11772744
    ## ORVOS     -0.22511388 -0.20471854 -0.08253315  0.15015208  0.27510267
    ## GYORVOS   -0.01993593 -0.10523180  0.02824298  0.04381977  0.05169498
    ##               BUNOZES     SERTETT   PEDAGOGUS       ORVOS     GYORVOS
    ## LAKAS      0.01022799  0.06318258  0.20774436 -0.22511388 -0.01993593
    ## SZJA       0.01193103 -0.16331058  0.16307579 -0.20471854 -0.10523180
    ## MUNKA      0.02967921 -0.23568456 -0.04414352 -0.08253315  0.02824298
    ## BERUHAZAS -0.07237939  0.05667894  0.04345751  0.15015208  0.04381977
    ## CSOK       0.02450377 -0.09959544 -0.11772744  0.27510267  0.05169498
    ## BUNOZES    1.00000000  0.47190644 -0.01245317 -0.02383703 -0.01089151
    ## SERTETT    0.47190644  1.00000000  0.14876944  0.01314383  0.06371884
    ## PEDAGOGUS -0.01245317  0.14876944  1.00000000  0.18663071  0.08870950
    ## ORVOS     -0.02383703  0.01314383  0.18663071  1.00000000  0.05429053
    ## GYORVOS   -0.01089151  0.06371884  0.08870950  0.05429053  1.00000000

# Panelmodell

## Tesztek

``` r
phtest(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + MEGYE_SZH, df, index = c("JARAS_NEV", "EV"))
```

    ## 
    ##  Hausman Test
    ## 
    ## data:  LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + MEGYE_SZH
    ## chisq = 69.489, df = 5, p-value = 1.309e-13
    ## alternative hypothesis: one model is inconsistent

``` r
phtest(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND, df, index = c("JARAS_NEV", "EV"))
```

    ## 
    ##  Hausman Test
    ## 
    ## data:  LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND
    ## chisq = 87.633, df = 5, p-value < 2.2e-16
    ## alternative hypothesis: one model is inconsistent

``` r
pFtest(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND, df, index = c("JARAS_NEV", "EV"))
```

    ## 
    ##  F test for individual effects
    ## 
    ## data:  LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND
    ## F = 12.544, df1 = 174, df2 = 1570, p-value < 2.2e-16
    ## alternative hypothesis: significant effects

## Nemtudom mi legyen a cím itt

``` r
df2 = df %>% arrange(EV) %>% filter(BALATON == 0)
listw36 = nb2listw(dnearneigh(df2[df2$EV == "2016",c(9,10)], 0, 36, longlat = T), style = "W")
```

## Modellek

Ezeket külön kellene vegyem majd amikor csinálom őket

``` r
# regi code
cuts = c(-Inf, 1, 70, 86, Inf)
labs = c("nincs", "alacsony","közepes", "magas")
df2$CSOKTREND = cut(df2$CSOK, breaks = cuts, labels = labs, include.lowest = T)

model1 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

cuts = c(-Inf, 1, 75, Inf)
labs = c("nincs", "alacsony", "magas")
df2$CSOKTREND = cut(df2$CSOK, breaks = cuts, labels = labs, include.lowest = T)

model2 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

model3 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOK, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

model4 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

rm(cuts, labs)
```

### Summary-k

``` r
summary(model1)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND, 
    ##     data = df2, index = c("JARAS_NEV", "EV"), listw = listw36, 
    ##     model = "within", effect = "individual", lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -29.57079  -2.47829  -0.13098   2.07284  45.13993 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.425039   0.030721  13.835 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t-value  Pr(>|t|)    
    ## SZJA               0.0128184  0.0016543  7.7485 9.298e-15 ***
    ## MUNKA             -0.3501837  0.0749741 -4.6707 3.001e-06 ***
    ## BERUHAZAS         -0.0052288  0.0011940 -4.3793 1.191e-05 ***
    ## CSOKTRENDalacsony  1.7720718  0.8653858  2.0477   0.04059 *  
    ## CSOKTRENDközepes   1.5546326  0.6874166  2.2616   0.02372 *  
    ## CSOKTRENDmagas     0.0507620  0.6571413  0.0772   0.93843    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(model2)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND, 
    ##     data = df2, index = c("JARAS_NEV", "EV"), listw = listw36, 
    ##     model = "within", effect = "individual", lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -29.56834  -2.42710  -0.17835   2.02348  45.27320 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.427861   0.030651  13.959 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t-value  Pr(>|t|)    
    ## SZJA               0.0132399  0.0016241  8.1523 3.572e-16 ***
    ## MUNKA             -0.3614834  0.0752768 -4.8021 1.570e-06 ***
    ## BERUHAZAS         -0.0053522  0.0011953 -4.4779 7.540e-06 ***
    ## CSOKTRENDalacsony  1.5803465  0.7645856  2.0669   0.03874 *  
    ## CSOKTRENDmagas     0.5120191  0.6097326  0.8397   0.40105    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(model3)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOK, 
    ##     data = df2, index = c("JARAS_NEV", "EV"), listw = listw36, 
    ##     model = "within", effect = "individual", lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -29.68337  -2.42192  -0.14743   1.95331  45.42790 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.436999   0.030331  14.408 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t-value  Pr(>|t|)    
    ## SZJA       0.0146362  0.0014832  9.8682 < 2.2e-16 ***
    ## MUNKA     -0.3623438  0.0753657 -4.8078 1.526e-06 ***
    ## BERUHAZAS -0.0054907  0.0011955 -4.5928 4.372e-06 ***
    ## CSOK       0.0018729  0.0066855  0.2801    0.7794    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(model4)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS, data = df2, 
    ##     index = c("JARAS_NEV", "EV"), listw = listw36, model = "within", 
    ##     effect = "individual", lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -29.68360  -2.40912  -0.12467   1.93846  45.40176 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.437079   0.030328  14.412 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t-value  Pr(>|t|)    
    ## SZJA       0.0146471  0.0014828  9.8783 < 2.2e-16 ***
    ## MUNKA     -0.3510459  0.0635404 -5.5248 3.299e-08 ***
    ## BERUHAZAS -0.0055641  0.0011668 -4.7686 1.855e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### AIC, BIC

``` r
AIC1 = 2 * (length(model1$coefficients) + 1) - 2 * model1$logLik
AIC2 = 2 * (length(model2$coefficients) + 1) - 2 * model2$logLik
AIC3 = 2 * (length(model3$coefficients) + 1) - 2 * model3$logLik
AIC4 = 2 * (length(model4$coefficients) + 1) - 2 * model4$logLik

BIC1 = -2 * model1$logLik + (length(model1$coefficients)+1)*log(nrow(df))
BIC2 = -2 * model2$logLik + (length(model2$coefficients)+1)*log(nrow(df))
BIC3 = -2 * model3$logLik + (length(model3$coefficients)+1)*log(nrow(df))
BIC4 = -2 * model4$logLik + (length(model4$coefficients)+1)*log(nrow(df))

data.frame(AIC = c(AIC1, AIC2, AIC3, AIC4),
           BIC = c(BIC1, BIC2, BIC3, BIC4))
```

    ##        AIC      BIC
    ## 1 11046.93 11090.67
    ## 2 11048.75 11087.02
    ## 3 11051.44 11084.24
    ## 4 11049.52 11076.85

# SEM

``` r
sem = "
  # measurement model
    
  # regressions
  
  # residual correlations
"

fit = sem(sem, data = df2012)
summary(fit, standardized = T)
```
