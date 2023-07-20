CSOK TDK kód
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
  - [Nem tudom mi legyen a cím itt](#nem-tudom-mi-legyen-a-cím-itt)
  - [Modellek](#modellek)
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
library(corrplot)
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
cor(df[,c(4:8,14:19)], use = "complete.obs")
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
    ## ATLAGAR    0.69680891  0.7430468  0.55728901  0.38713991  0.01480768
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
    ## ATLAGAR   -0.10765804 -0.2622687  0.27325799 -0.45604068 -0.15125215
    ##               ATLAGAR
    ## LAKAS      0.69680891
    ## SZJA       0.74304678
    ## MUNKA      0.55728901
    ## BERUHAZAS  0.38713991
    ## CSOK       0.01480768
    ## BUNOZES   -0.10765804
    ## SERTETT   -0.26226871
    ## PEDAGOGUS  0.27325799
    ## ORVOS     -0.45604068
    ## GYORVOS   -0.15125215
    ## ATLAGAR    1.00000000

``` r
corrplot(cor(df[,c(4:8,14:19)], use = "complete.obs"), method = "color", addCoef.col = "darkolivegreen")
```

![](tdk_files/figure-gfm/corrplot-1.png)<!-- -->

``` r
pcor(na.omit(df[,c(4:8,14:19)]))$estimate
```

    ##                  LAKAS         SZJA       MUNKA    BERUHAZAS        CSOK
    ## LAKAS      1.000000000 -0.141305719  0.08054721 -0.002347232  0.06527680
    ## SZJA      -0.141305719  1.000000000  0.40263633  0.452815592  0.10827029
    ## MUNKA      0.080547215  0.402636328  1.00000000  0.137211519  0.09949717
    ## BERUHAZAS -0.002347232  0.452815592  0.13721152  1.000000000 -0.06709262
    ## CSOK       0.065276796  0.108270293  0.09949717 -0.067092623  1.00000000
    ## BUNOZES   -0.011578544 -0.009818031  0.03012111 -0.066512071  0.02695475
    ## SERTETT    0.015567480 -0.173721241 -0.23426654  0.065437692 -0.09446884
    ## PEDAGOGUS  0.083556225  0.048688336 -0.04153938  0.064821125 -0.10489165
    ## ORVOS     -0.061201382 -0.047247105 -0.08326690  0.113857941  0.25217900
    ## GYORVOS    0.001476325 -0.070593660  0.02780678  0.038759870  0.04926800
    ## ATLAGAR    0.548133710  0.540551842 -0.01264402 -0.139262319 -0.06877748
    ##                BUNOZES     SERTETT   PEDAGOGUS       ORVOS      GYORVOS
    ## LAKAS     -0.011578544  0.01556748  0.08355623 -0.06120138  0.001476325
    ## SZJA      -0.009818031 -0.17372124  0.04868834 -0.04724711 -0.070593660
    ## MUNKA      0.030121108 -0.23426654 -0.04153938 -0.08326690  0.027806780
    ## BERUHAZAS -0.066512071  0.06543769  0.06482112  0.11385794  0.038759870
    ## CSOK       0.026954752 -0.09446884 -0.10489165  0.25217900  0.049268004
    ## BUNOZES    1.000000000  0.46801433 -0.01817523 -0.01502497 -0.009662893
    ## SERTETT    0.468014330  1.00000000  0.13562781  0.02791154  0.065780969
    ## PEDAGOGUS -0.018175229  0.13562781  1.00000000  0.21537565  0.092822572
    ## ORVOS     -0.015024972  0.02791154  0.21537565  1.00000000  0.045505643
    ## GYORVOS   -0.009662893  0.06578097  0.09282257  0.04550564  1.000000000
    ## ATLAGAR    0.036719814  0.06778690  0.16044999 -0.22318048 -0.033096695
    ##               ATLAGAR
    ## LAKAS      0.54813371
    ## SZJA       0.54055184
    ## MUNKA     -0.01264402
    ## BERUHAZAS -0.13926232
    ## CSOK      -0.06877748
    ## BUNOZES    0.03671981
    ## SERTETT    0.06778690
    ## PEDAGOGUS  0.16044999
    ## ORVOS     -0.22318048
    ## GYORVOS   -0.03309669
    ## ATLAGAR    1.00000000

``` r
corrplot(pcor(na.omit(df[,c(4:8,14:19)]))$estimate, method = "color", addCoef.col = "darkolivegreen")
```

![](tdk_files/figure-gfm/parcialis%20corrplot-1.png)<!-- -->

# Panelmodell

## Tesztek

``` r
phtest(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + MEGYE_SZH + BALATON + BUNOZES + SERTETT + PEDAGOGUS + ORVOS + GYORVOS + ATLAGAR, df, index = c("JARAS_NEV", "EV"))
```

    ## 
    ##  Hausman Test
    ## 
    ## data:  LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + MEGYE_SZH +  ...
    ## chisq = 21.873, df = 11, p-value = 0.02538
    ## alternative hypothesis: one model is inconsistent

``` r
phtest(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES + SERTETT + PEDAGOGUS + ORVOS + GYORVOS + ATLAGAR, df, index = c("JARAS_NEV", "EV"))
```

    ## 
    ##  Hausman Test
    ## 
    ## data:  LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES +  ...
    ## chisq = 33.222, df = 11, p-value = 0.0004842
    ## alternative hypothesis: one model is inconsistent

``` r
pFtest(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES + SERTETT + PEDAGOGUS + ORVOS + GYORVOS + ATLAGAR, df, index = c("JARAS_NEV", "EV"))
```

    ## 
    ##  F test for individual effects
    ## 
    ## data:  LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES +  ...
    ## F = 5.2639, df1 = 174, df2 = 1023, p-value < 2.2e-16
    ## alternative hypothesis: significant effects

## Nem tudom mi legyen a cím itt

``` r
df2 = df %>% arrange(EV)
listw36 = nb2listw(dnearneigh(df2[df2$EV == "2016",c(9,10)], 0, 36, longlat = T), style = "W")
```

## Modellek

Az ORVOS és GYORVOS változókat nem tudom beletenni, mert csak 2015-től
vannak és azok néhol hiányosan.

#### Modell 1

CSOK dummy: “nincs”, “alacsony”,“közepes”, “magas”

``` r
cuts = c(-Inf, 1, 70, 86, Inf)
labs = c("nincs", "alacsony","közepes", "magas")
df2$CSOKTREND = cut(df2$CSOK, breaks = cuts, labels = labs, include.lowest = T)

model1 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model1)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + 
    ##     BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR, data = df2, index = c("JARAS_NEV", 
    ##     "EV"), listw = listw36, model = "within", effect = "individual", 
    ##     lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -42.03430  -2.26730  -0.28721   1.84919 115.25958 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.350435   0.031668  11.066 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA              -4.6545e-03  2.2944e-03 -2.0287 0.0424931 *  
    ## MUNKA             -2.5353e-02  1.0019e-01 -0.2530 0.8002302    
    ## BERUHAZAS         -4.4823e-03  1.3487e-03 -3.3233 0.0008896 ***
    ## CSOKTRENDalacsony  2.5702e+00  9.6050e-01  2.6759 0.0074527 ** 
    ## CSOKTRENDközepes   3.0838e+00  7.8813e-01  3.9128 9.124e-05 ***
    ## CSOKTRENDmagas     8.0434e-01  7.4258e-01  1.0832 0.2787350    
    ## BUNOZES           -7.4323e-05  1.7104e-04 -0.4345 0.6639042    
    ## SERTETT           -6.6471e-04  3.9130e-04 -1.6987 0.0893734 .  
    ## PEDAGOGUS          4.8527e-01  4.6228e-01  1.0497 0.2938443    
    ## ATLAGAR            1.0684e+00  8.6477e-02 12.3551 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Modell 2

CSOK dummy: “nincs”, “alacsony”, “magas”

``` r
cuts = c(-Inf, 1, 75, Inf)
labs = c("nincs", "alacsony", "magas")
df2$CSOKTREND = cut(df2$CSOK, breaks = cuts, labels = labs, include.lowest = T)
rm(cuts, labs)

model2 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model2)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + 
    ##     BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR, data = df2, index = c("JARAS_NEV", 
    ##     "EV"), listw = listw36, model = "within", effect = "individual", 
    ##     lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -41.83609  -2.21652  -0.32581   1.73821 115.18953 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.356005   0.031582  11.273 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA              -3.7543e-03  2.2732e-03 -1.6516 0.0986210 .  
    ## MUNKA             -6.4351e-02  1.0013e-01 -0.6427 0.5204302    
    ## BERUHAZAS         -4.5852e-03  1.3524e-03 -3.3903 0.0006982 ***
    ## CSOKTRENDalacsony  2.6641e+00  8.6434e-01  3.0823 0.0020543 ** 
    ## CSOKTRENDmagas     1.5761e+00  6.9588e-01  2.2649 0.0235194 *  
    ## BUNOZES           -6.9834e-05  1.7141e-04 -0.4074 0.6837026    
    ## SERTETT           -7.2464e-04  3.9121e-04 -1.8523 0.0639846 .  
    ## PEDAGOGUS          3.8896e-01  4.6280e-01  0.8404 0.4006587    
    ## ATLAGAR            1.0385e+00  8.5643e-02 12.1263 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Modell 3

CSOK számként

``` r
model3 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOK + BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model3)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOK + 
    ##     BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR, data = df2, index = c("JARAS_NEV", 
    ##     "EV"), listw = listw36, model = "within", effect = "individual", 
    ##     lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -42.57811  -2.16945  -0.27426   1.72630 115.31856 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda  0.36908    0.03115  11.848 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA      -2.0915e-03  2.1490e-03 -0.9733 0.3304234    
    ## MUNKA     -9.2495e-02  9.9275e-02 -0.9317 0.3514890    
    ## BERUHAZAS -4.6189e-03  1.3534e-03 -3.4129 0.0006429 ***
    ## CSOK       1.5048e-02  7.6525e-03  1.9663 0.0492593 *  
    ## BUNOZES   -6.9934e-05  1.7150e-04 -0.4078 0.6834375    
    ## SERTETT   -7.4940e-04  3.9128e-04 -1.9153 0.0554592 .  
    ## PEDAGOGUS  2.3010e-01  4.5840e-01  0.5020 0.6156893    
    ## ATLAGAR    1.0503e+00  8.5953e-02 12.2197 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Modell 4

CSOK nélkül

``` r
model4 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOK + BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model4)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOK + 
    ##     BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR, data = df2, index = c("JARAS_NEV", 
    ##     "EV"), listw = listw36, model = "within", effect = "individual", 
    ##     lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -42.57811  -2.16945  -0.27426   1.72630 115.31856 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda  0.36908    0.03115  11.848 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA      -2.0915e-03  2.1490e-03 -0.9733 0.3304234    
    ## MUNKA     -9.2495e-02  9.9275e-02 -0.9317 0.3514890    
    ## BERUHAZAS -4.6189e-03  1.3534e-03 -3.4129 0.0006429 ***
    ## CSOK       1.5048e-02  7.6525e-03  1.9663 0.0492593 *  
    ## BUNOZES   -6.9934e-05  1.7150e-04 -0.4078 0.6834375    
    ## SERTETT   -7.4940e-04  3.9128e-04 -1.9153 0.0554592 .  
    ## PEDAGOGUS  2.3010e-01  4.5840e-01  0.5020 0.6156893    
    ## ATLAGAR    1.0503e+00  8.5953e-02 12.2197 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### AIC, BIC

``` r
paic = function(model){
  return(2 * (length(model$coefficients) + 1) - 2 * model$logLik)
}

pbic = function(model){
 return(-2 * model$logLik + (length(model$coefficients)+1)*log(nrow(model$model)))
}

#valamiert nem mukodik a lapply, sapply... -val :/
data.frame(AIC = c(paic(model1), paic(model2), paic(model3), paic(model4)) ,
           BIC = c(pbic(model1), pbic(model2), pbic(model3), pbic(model4)))
```

    ##        AIC      BIC
    ## 1 11938.25 12003.86
    ## 2 11944.55 12004.70
    ## 3 11948.18 12002.85
    ## 4 11948.18 12002.85

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
