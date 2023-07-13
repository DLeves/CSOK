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
corrplot(cor(df[,c(4:8,14:18)], use = "complete.obs"), method = "color", addCoef.col = "darkolivegreen")
```

![](tdk_files/figure-gfm/corrplot-1.png)<!-- -->

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

``` r
corrplot(pcor(na.omit(df[,c(4:8,14:18)]))$estimate, method = "color", addCoef.col = "darkolivegreen")
```

![](tdk_files/figure-gfm/parcialis%20corrplot-1.png)<!-- -->

# Panelmodell

## Tesztek

``` r
phtest(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + MEGYE_SZH + BALATON + BUNOZES + SERTETT + PEDAGOGUS + ORVOS + GYORVOS, df, index = c("JARAS_NEV", "EV"))
```

    ## 
    ##  Hausman Test
    ## 
    ## data:  LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + MEGYE_SZH +  ...
    ## chisq = 52.766, df = 10, p-value = 8.23e-08
    ## alternative hypothesis: one model is inconsistent

``` r
phtest(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES + SERTETT + PEDAGOGUS + ORVOS + GYORVOS, df, index = c("JARAS_NEV", "EV"))
```

    ## 
    ##  Hausman Test
    ## 
    ## data:  LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES +  ...
    ## chisq = 94.73, df = 10, p-value = 6.147e-16
    ## alternative hypothesis: one model is inconsistent

``` r
pFtest(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES + SERTETT + PEDAGOGUS + ORVOS + GYORVOS, df, index = c("JARAS_NEV", "EV"))
```

    ## 
    ##  F test for individual effects
    ## 
    ## data:  LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES +  ...
    ## F = 8.638, df1 = 174, df2 = 1024, p-value < 2.2e-16
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

model1 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES + SERTETT + PEDAGOGUS, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model1)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + 
    ##     BUNOZES + SERTETT + PEDAGOGUS, data = df2, index = c("JARAS_NEV", 
    ##     "EV"), listw = listw36, model = "within", effect = "individual", 
    ##     lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -33.67945  -2.80473  -0.13815   2.20214 119.47749 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda  0.40056    0.03144  12.741 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA               1.2279e-02  1.9711e-03  6.2295 4.679e-10 ***
    ## MUNKA             -3.5610e-01  1.0107e-01 -3.5232 0.0004264 ***
    ## BERUHAZAS         -5.5450e-03  1.4025e-03 -3.9536 7.699e-05 ***
    ## CSOKTRENDalacsony  2.9971e+00  9.9983e-01  2.9976 0.0027214 ** 
    ## CSOKTRENDközepes   2.0416e+00  8.1476e-01  2.5058 0.0122177 *  
    ## CSOKTRENDmagas     4.1088e-01  7.7186e-01  0.5323 0.5945057    
    ## BUNOZES           -7.8009e-05  1.7802e-04 -0.4382 0.6612458    
    ## SERTETT           -7.8183e-04  4.0711e-04 -1.9204 0.0548012 .  
    ## PEDAGOGUS          8.6453e-01  4.8008e-01  1.8008 0.0717343 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Modell 2

CSOK dummy: “nincs”, “alacsony”, “magas”

``` r
cuts = c(-Inf, 1, 75, Inf)
labs = c("nincs", "alacsony", "magas")
df2$CSOKTREND = cut(df2$CSOK, breaks = cuts, labels = labs, include.lowest = T)
rm(cuts, labs)

model2 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES + SERTETT + PEDAGOGUS, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model2)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + 
    ##     BUNOZES + SERTETT + PEDAGOGUS, data = df2, index = c("JARAS_NEV", 
    ##     "EV"), listw = listw36, model = "within", effect = "individual", 
    ##     lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -33.17314  -2.81092  -0.17748   2.24657 119.74721 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.402221   0.031405  12.807 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA               1.2961e-02  1.9455e-03  6.6619 2.703e-11 ***
    ## MUNKA             -3.6683e-01  1.0144e-01 -3.6161 0.0002991 ***
    ## BERUHAZAS         -5.7781e-03  1.4042e-03 -4.1148 3.875e-05 ***
    ## CSOKTRENDalacsony  2.4520e+00  8.9815e-01  2.7300 0.0063332 ** 
    ## CSOKTRENDmagas     8.0819e-01  7.1966e-01  1.1230 0.2614268    
    ## BUNOZES           -7.1944e-05  1.7821e-04 -0.4037 0.6864224    
    ## SERTETT           -7.8790e-04  4.0666e-04 -1.9375 0.0526850 .  
    ## PEDAGOGUS          8.5219e-01  4.7951e-01  1.7772 0.0755326 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Modell 3

CSOK számként

``` r
model3 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOK + BUNOZES + SERTETT + PEDAGOGUS, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model3)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOK + 
    ##     BUNOZES + SERTETT + PEDAGOGUS, data = df2, index = c("JARAS_NEV", 
    ##     "EV"), listw = listw36, model = "within", effect = "individual", 
    ##     lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -31.82627  -2.71257  -0.15857   2.13002 119.87841 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.417891   0.030918  13.516 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA       1.5241e-02  1.7603e-03  8.6580 < 2.2e-16 ***
    ## MUNKA     -3.9105e-01  1.0057e-01 -3.8885 0.0001009 ***
    ## BERUHAZAS -5.9406e-03  1.4050e-03 -4.2283 2.355e-05 ***
    ## CSOK       3.2185e-03  7.8850e-03  0.4082 0.6831414    
    ## BUNOZES   -6.9596e-05  1.7836e-04 -0.3902 0.6963881    
    ## SERTETT   -8.2918e-04  4.0683e-04 -2.0381 0.0415370 *  
    ## PEDAGOGUS  6.3947e-01  4.7552e-01  1.3448 0.1786962    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Modell 4

CSOK nélkül

``` r
model4 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOK + BUNOZES + SERTETT + PEDAGOGUS, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model4)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOK + 
    ##     BUNOZES + SERTETT + PEDAGOGUS, data = df2, index = c("JARAS_NEV", 
    ##     "EV"), listw = listw36, model = "within", effect = "individual", 
    ##     lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -31.82627  -2.71257  -0.15857   2.13002 119.87841 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.417891   0.030918  13.516 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA       1.5241e-02  1.7603e-03  8.6580 < 2.2e-16 ***
    ## MUNKA     -3.9105e-01  1.0057e-01 -3.8885 0.0001009 ***
    ## BERUHAZAS -5.9406e-03  1.4050e-03 -4.2283 2.355e-05 ***
    ## CSOK       3.2185e-03  7.8850e-03  0.4082 0.6831414    
    ## BUNOZES   -6.9596e-05  1.7836e-04 -0.3902 0.6963881    
    ## SERTETT   -8.2918e-04  4.0683e-04 -2.0381 0.0415370 *  
    ## PEDAGOGUS  6.3947e-01  4.7552e-01  1.3448 0.1786962    
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
    ## 1 12089.80 12149.94
    ## 2 12091.34 12146.01
    ## 3 12097.47 12146.68
    ## 4 12097.47 12146.68

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
