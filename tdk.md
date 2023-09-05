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
  - [Eü főkomponensek](#eü-főkomponensek)
    - [Főkomponensek létehozása](#főkomponensek-létehozása)
    - [Kiválasztás és df-hez adás](#kiválasztás-és-df-hez-adás)
    - [Főkomponensek elnevezése](#főkomponensek-elnevezése)
  - [Tesztek](#tesztek)
  - [Nem tudom mi legyen a cím itt](#nem-tudom-mi-legyen-a-cím-itt)
  - [Modellek](#modellek)
    - [AIC, BIC](#aic-bic)
- [SEM](#sem)
  - [Normalizált változókkal](#normalizált-változókkal)

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
library(car)
```

## Adatok beolvasása, átalakítása

``` r
df = read.csv("Data/ExtendedDataset.csv")
```

### CSOK dummy meghatározása

Ez a medián a CSOK-os években

``` r
median(df$CSOK[df$EV > "2015"])
```

    ## [1] 74

``` r
df$JARAS_NEV = as.factor(df$JARAS_NEV)
df$EV = as.factor(df$EV)
```

A mediánnál elválasztva:

``` r
cuts = c(-Inf, 1, 75, Inf)
labs = c("nincs", "alacsony", "magas")

df$CSOKTREND = cut(df$CSOK, breaks = cuts, labels = labs, include.lowest = T)

rm(cuts, labs)
```

### Adatok pótlása GAM-al

#### Lakás GAM modell

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
```

#### Eü adatok GAM modelljei

``` r
sapply(df[,20:37], function(x) sum(is.na(x)))
```

    ##              HO_SZAM            HGYO_SZAM        HO_APOLO_SZAM 
    ##                    0                   49                    0 
    ##        HO_SZOLG_SZAM      HGYO_SZOLG_SZAM       FHO_SZOLG_SZAM 
    ##                    0                   41                   40 
    ##     FGYHO_SZOLG_SZAM   HO_HELY_SZOLG_SZAM HGYO_HELY_SZOLG_SZAM 
    ##                   51                  446                 1159 
    ##         HO_TMED_SZAM      HGYO_TMED_SZAMA         HO_FORG_OSSZ 
    ##                 1575                 1582                    0 
    ##       HGYO_FORG_OSSZ         HGYO_FORG_RB         HGYO_FORG_RK 
    ##                   41                   41                   57 
    ##           HO_FORG_RB           HO_FORG_RK               VEDONO 
    ##                    0                    0                    0

##### FGYHO_SZOLG_SZAM

``` r
gammodel = gam(FGYHO_SZOLG_SZAM  ~ s(X,Y, by = EV, k = 8), family = "gaussian", data = df, method = "REML")

predicted = predict(gammodel, df)

for (i in 1:nrow(df)) {
  if(is.na(df$FGYHO_SZOLG_SZAM[i])){
    if(predicted[i] > 0){
      df$FGYHO_SZOLG_SZAM[i] = predicted[i]
    }else{
      df$FGYHO_SZOLG_SZAM[i] = 0
    }
  }
}
```

##### HGYO_SZAM

``` r
gammodel = gam(HGYO_SZAM ~ s(X,Y, by = EV, k = 8), family = "gaussian", data = df, method = "REML")

predicted = predict(gammodel, df)

for (i in 1:nrow(df)) {
  if(is.na(df$HGYO_SZAM[i])){
    if(predicted[i] > 0){
      df$HGYO_SZAM[i] = predicted[i]
    }else{
      df$HGYO_SZAM[i] = 0
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
corrplot(cor(df[,c(4:8,14:37)], use = "complete.obs"), method = "color")
```

![](tdk_files/figure-gfm/corrplot-1.png)<!-- -->

``` r
corrplot(cor(df[,c(4:8,14:37)], use = "complete.obs"), method = "color", addCoef.col = "darkolivegreen")
```

![](tdk_files/figure-gfm/corrplot-2.png)<!-- -->

``` r
pcor(na.omit(df[,c(4:8,14:37)]))$estimate
```

    ## Error in solve.default(cvx): system is computationally singular: reciprocal condition number = 2.44954e-26

``` r
corrplot(pcor(na.omit(df[,c(4:8,14:16,19:37)]))$estimate, method = "color")
```

    ## Warning in pcor(na.omit(df[, c(4:8, 14:16, 19:37)])): The inverse of
    ## variance-covariance matrix is calculated using Moore-Penrose generalized matrix
    ## invers due to its determinant of zero.

![](tdk_files/figure-gfm/parcialis%20corrplot-1.png)<!-- -->

Nem tudom miert dob error, elozoleg mukodott rendesen. Azert le van
mentve a kep:

``` r
knitr::include_graphics("tdk_files/figure-gfm/parcialis corrplot-1.png")
```

<img src="tdk_files/figure-gfm/parcialis corrplot-1.png" width="1440" />

# Panelmodell

## Eü főkomponensek

### Főkomponensek létehozása

``` r
fokompok = prcomp(df[,c("VEDONO", "HO_FORG_RB", "HO_FORG_OSSZ", "FGYHO_SZOLG_SZAM", "HO_SZOLG_SZAM", "HO_APOLO_SZAM", "HGYO_SZAM")], center = TRUE, scale=TRUE)

summary(fokompok)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3    PC4     PC5     PC6     PC7
    ## Standard deviation     2.0970 1.0020 0.83167 0.6787 0.50551 0.38691 0.20237
    ## Proportion of Variance 0.6282 0.1434 0.09881 0.0658 0.03651 0.02139 0.00585
    ## Cumulative Proportion  0.6282 0.7716 0.87045 0.9363 0.97276 0.99415 1.00000

### Kiválasztás és df-hez adás

``` r
df = cbind(df, fokompok$x[,1:3])
```

``` r
corrplot(cor(df[,c("VEDONO", "HO_FORG_RB", "HO_FORG_OSSZ", "FGYHO_SZOLG_SZAM", "HO_SZOLG_SZAM", "HO_APOLO_SZAM", "HGYO_SZAM", "PC1", "PC2", "PC3")]), method = "color", addCoef.col = "black")
```

![](tdk_files/figure-gfm/fokom%20corrplot-1.png)<!-- -->

Ez így nem jó, nem lehet meghatározni a PC2 és PC3-at, mert mind a
VEDONO, mind a HGYO_SZAM korrelál vele.

``` r
fokompok = prcomp(df[,c("HO_FORG_RB", "HO_FORG_OSSZ", "FGYHO_SZOLG_SZAM", "HO_SZOLG_SZAM", "HO_APOLO_SZAM", "HGYO_SZAM")], center = TRUE, scale=TRUE)

summary(fokompok)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3     PC4    PC5     PC6
    ## Standard deviation     2.0516 0.9336 0.68701 0.50628 0.3873 0.20237
    ## Proportion of Variance 0.7015 0.1453 0.07866 0.04272 0.0250 0.00683
    ## Cumulative Proportion  0.7015 0.8468 0.92545 0.96817 0.9932 1.00000

``` r
df[,c("PC1", "PC2")] = fokompok$x[,1:2]
```

Akkor a VEDONO és elhagyásával járok jobban(source: próbálgatás).

``` r
corrplot(cor(df[,c("VEDONO", "HO_FORG_RB", "HO_FORG_OSSZ", "FGYHO_SZOLG_SZAM", "HO_SZOLG_SZAM", "HO_APOLO_SZAM", "HGYO_SZAM", "PC1", "PC2")]), method = "color", addCoef.col = "black")
```

![](tdk_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Főkomponensek elnevezése

Mik lehetnek az egyes főkomponensek?

- PC1: rendelők nagysága/leterheltsége
- PC2: gyermekorvosok száma

## Tesztek

``` r
phtest(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR + VEDONO + PC1 + PC2, df, index = c("JARAS_NEV", "EV"))
```

    ## 
    ##  Hausman Test
    ## 
    ## data:  LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES +  ...
    ## chisq = 102.16, df = 12, p-value < 2.2e-16
    ## alternative hypothesis: one model is inconsistent

``` r
pFtest(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR + VEDONO + PC1 + PC2, df, index = c("JARAS_NEV", "EV"))
```

    ## 
    ##  F test for individual effects
    ## 
    ## data:  LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES +  ...
    ## F = 6.0579, df1 = 174, df2 = 1563, p-value < 2.2e-16
    ## alternative hypothesis: significant effects

Mindkét teszt p-értéke 2.2 $\times$ 10<sup>-16</sup>, vagyis szinte 0.
Ezt azt jelenti, hogy mindkét esetben el lehet vetni a nullhipotézist.

A Hausmann teszt nullhipotzise az, hogy a random modell a megfelelő a
fix hatásúval szemben. Ezt elvethetjük.

A globális F próba nullhipotézise szerint a Pooled OLS modell a
megfelelő, csakugyan a fix hatásúval szemben. Ezt is elvethetjük.

Tehát a panelmodell fix hatású lesz.

## Nem tudom mi legyen a cím itt

``` r
df2 = df %>% arrange(EV)
listw36 = nb2listw(dnearneigh(df2[df2$EV == "2016",c(9,10)], 0, 36, longlat = T), style = "W")
```

## Modellek

``` r
paic = function(model){
  return(2 * (length(model$coefficients) + 1) - 2 * model$logLik)
}

pbic = function(model){
 return(-2 * model$logLik + (length(model$coefficients)+1)*log(nrow(model$model)))
}
```

#### Modell 1

CSOK dummy: “nincs”, “alacsony”,“közepes”, “magas”

``` r
cuts = c(-Inf, 1, 70, 86, Inf)
labs = c("nincs", "alacsony","közepes", "magas")
df2$CSOKTREND = cut(df2$CSOK, breaks = cuts, labels = labs, include.lowest = T)

model1_0 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR + VEDONO + PC1 + PC2, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model1_0)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + 
    ##     BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR + VEDONO + PC1 + 
    ##     PC2, data = df2, index = c("JARAS_NEV", "EV"), listw = listw36, 
    ##     model = "within", effect = "individual", lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -41.16936  -2.35093  -0.26746   1.85967 115.02179 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.334465   0.031949  10.469 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA              -4.1749e-03  2.3311e-03 -1.7910 0.0732978 .  
    ## MUNKA              1.8481e-02  1.0173e-01  0.1817 0.8558504    
    ## BERUHAZAS         -4.3045e-03  1.3503e-03 -3.1877 0.0014339 ** 
    ## CSOKTRENDalacsony  2.4397e+00  9.6247e-01  2.5348 0.0112502 *  
    ## CSOKTRENDközepes   3.0051e+00  7.8888e-01  3.8093 0.0001394 ***
    ## CSOKTRENDmagas     7.3648e-01  7.4179e-01  0.9928 0.3207843    
    ## BUNOZES           -8.1145e-05  1.7088e-04 -0.4749 0.6348862    
    ## SERTETT           -6.0537e-04  3.9226e-04 -1.5433 0.1227604    
    ## PEDAGOGUS          4.3912e-01  4.6188e-01  0.9507 0.3417377    
    ## ATLAGAR            1.0233e+00  8.7992e-02 11.6290 < 2.2e-16 ***
    ## VEDONO            -3.7488e-01  5.3680e-01 -0.6984 0.4849452    
    ## PC1               -1.5222e+00  5.9662e-01 -2.5514 0.0107295 *  
    ## PC2               -9.2588e-01  5.5001e-01 -1.6834 0.0923023 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model1_1 = spml(LAKAS_PRED ~ SZJA  + BERUHAZAS + CSOKTREND  + SERTETT  + ATLAGAR + PC1 + PC2, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model1_1)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + BERUHAZAS + CSOKTREND + SERTETT + 
    ##     ATLAGAR + PC1 + PC2, data = df2, index = c("JARAS_NEV", "EV"), 
    ##     listw = listw36, model = "within", effect = "individual", 
    ##     lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##     Min.  1st Qu.   Median  3rd Qu.     Max. 
    ## -41.4239  -2.2894  -0.2844   1.8586 115.0847 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.336772   0.031794  10.592 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA              -0.00425377  0.00179515 -2.3696 0.0178079 *  
    ## BERUHAZAS         -0.00426167  0.00134446 -3.1698 0.0015254 ** 
    ## CSOKTRENDalacsony  2.31263803  0.93010636  2.4864 0.0129034 *  
    ## CSOKTRENDközepes   2.84225552  0.74755859  3.8021 0.0001435 ***
    ## CSOKTRENDmagas     0.59852118  0.65895894  0.9083 0.3637287    
    ## SERTETT           -0.00069041  0.00033112 -2.0851 0.0370634 *  
    ## ATLAGAR            1.03972200  0.08411001 12.3615 < 2.2e-16 ***
    ## PC1               -1.55503557  0.58933697 -2.6386 0.0083245 ** 
    ## PC2               -0.95478725  0.54560891 -1.7499 0.0801273 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
paic(model1_0)
```

    ## [1] 11934.01

``` r
paic(model1_1)
```

    ## [1] 11927.62

``` r
pbic(model1_0)
```

    ## [1] 12016.02

``` r
pbic(model1_1)
```

    ## [1] 11987.76

AIC, BIC preferálja a szűkített modellt.

``` r
model1 = model1_1
```

#### Modell 2

CSOK dummy: “nincs”, “alacsony”, “magas”

``` r
cuts = c(-Inf, 1, 75, Inf)
labs = c("nincs", "alacsony", "magas")
df2$CSOKTREND = cut(df2$CSOK, breaks = cuts, labels = labs, include.lowest = T)
rm(cuts, labs)

model2_0 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR+ VEDONO + PC1 + PC2, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model2_0)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + 
    ##     BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR + VEDONO + PC1 + 
    ##     PC2, data = df2, index = c("JARAS_NEV", "EV"), listw = listw36, 
    ##     model = "within", effect = "individual", lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -40.93310  -2.31098  -0.29695   1.82221 114.95407 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.339461   0.031887  10.646 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA              -3.2944e-03  2.3058e-03 -1.4287  0.153078    
    ## MUNKA             -1.9731e-02  1.0168e-01 -0.1940  0.846141    
    ## BERUHAZAS         -4.3958e-03  1.3543e-03 -3.2457  0.001172 ** 
    ## CSOKTRENDalacsony  2.6056e+00  8.6586e-01  3.0093  0.002619 ** 
    ## CSOKTRENDmagas     1.4993e+00  6.9522e-01  2.1566  0.031034 *  
    ## BUNOZES           -7.6554e-05  1.7124e-04 -0.4471  0.654835    
    ## SERTETT           -6.6491e-04  3.9215e-04 -1.6956  0.089970 .  
    ## PEDAGOGUS          3.4458e-01  4.6233e-01  0.7453  0.456091    
    ## ATLAGAR            9.9054e-01  8.7231e-02 11.3554 < 2.2e-16 ***
    ## VEDONO            -4.4166e-01  5.3613e-01 -0.8238  0.410054    
    ## PC1               -1.5278e+00  5.9779e-01 -2.5558  0.010595 *  
    ## PC2               -9.2257e-01  5.5183e-01 -1.6718  0.094556 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model2_1 = spml(LAKAS_PRED ~ SZJA + BERUHAZAS + CSOKTREND + SERTETT + ATLAGAR + PC1 + PC2, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model2_1)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + BERUHAZAS + CSOKTREND + SERTETT + 
    ##     ATLAGAR + PC1 + PC2, data = df2, index = c("JARAS_NEV", "EV"), 
    ##     listw = listw36, model = "within", effect = "individual", 
    ##     lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -41.18165  -2.26755  -0.32048   1.80800 114.93781 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.342387   0.031719  10.794 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA              -0.00391371  0.00177554 -2.2042  0.027507 *  
    ## BERUHAZAS         -0.00438735  0.00134755 -3.2558  0.001131 ** 
    ## CSOKTRENDalacsony  2.42113995  0.83234196  2.9088  0.003628 ** 
    ## CSOKTRENDmagas     1.27833311  0.61956739  2.0633  0.039087 *  
    ## SERTETT           -0.00072349  0.00033115 -2.1848  0.028906 *  
    ## ATLAGAR            1.01539173  0.08365404 12.1380 < 2.2e-16 ***
    ## PC1               -1.58840188  0.59045580 -2.6901  0.007142 ** 
    ## PC2               -0.97213636  0.54700891 -1.7772  0.075538 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
paic(model2_0)
```

    ## [1] 11940.1

``` r
paic(model2_1)
```

    ## [1] 11933.74

``` r
pbic(model2_0)
```

    ## [1] 12016.64

``` r
pbic(model2_1)
```

    ## [1] 11988.41

AIC, BIC preferálja a szűkített modellt.

``` r
model2 = model2_1
```

#### Modell 3

CSOK számként

``` r
model3_0 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOK + BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR + VEDONO + PC1 + PC2, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model3_0)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOK + 
    ##     BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR + VEDONO + PC1 + 
    ##     PC2, data = df2, index = c("JARAS_NEV", "EV"), listw = listw36, 
    ##     model = "within", effect = "individual", lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -41.68965  -2.28237  -0.29391   1.79334 115.08427 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.352225   0.031469  11.193 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA      -1.5797e-03  2.1730e-03 -0.7270  0.467246    
    ## MUNKA     -5.0121e-02  1.0080e-01 -0.4972  0.619038    
    ## BERUHAZAS -4.4124e-03  1.3557e-03 -3.2546  0.001135 ** 
    ## CSOK       1.4477e-02  7.6444e-03  1.8938  0.058245 .  
    ## BUNOZES   -7.6084e-05  1.7134e-04 -0.4440  0.657009    
    ## SERTETT   -6.9099e-04  3.9224e-04 -1.7617  0.078127 .  
    ## PEDAGOGUS  1.8803e-01  4.5788e-01  0.4106  0.681330    
    ## ATLAGAR    1.0011e+00  8.7568e-02 11.4318 < 2.2e-16 ***
    ## VEDONO    -5.0964e-01  5.3570e-01 -0.9514  0.341427    
    ## PC1       -1.5722e+00  5.9727e-01 -2.6323  0.008480 ** 
    ## PC2       -8.5015e-01  5.5112e-01 -1.5426  0.122933    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model3_1 = spml(LAKAS_PRED ~ SZJA  + BERUHAZAS + CSOK + SERTETT + ATLAGAR + PC1 + PC2, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model3_1)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + BERUHAZAS + CSOK + SERTETT + 
    ##     ATLAGAR + PC1 + PC2, data = df2, index = c("JARAS_NEV", "EV"), 
    ##     listw = listw36, model = "within", effect = "individual", 
    ##     lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##     Min.  1st Qu.   Median  3rd Qu.     Max. 
    ## -41.8629  -2.2233  -0.3199   1.7481 114.9608 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.355354   0.031325  11.344 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA      -0.00257700  0.00167474 -1.5387 0.1238661    
    ## BERUHAZAS -0.00445650  0.00134833 -3.3052 0.0009491 ***
    ## CSOK       0.01171637  0.00678153  1.7277 0.0840440 .  
    ## SERTETT   -0.00074205  0.00033131 -2.2397 0.0251086 *  
    ## ATLAGAR    1.02811862  0.08416086 12.2161 < 2.2e-16 ***
    ## PC1       -1.64544396  0.58951316 -2.7912 0.0052514 ** 
    ## PC2       -0.90554422  0.54682958 -1.6560 0.0977239 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model3_2 = spml(LAKAS_PRED ~ BERUHAZAS + CSOK + SERTETT + ATLAGAR + PC1 + PC2, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model3_2)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ BERUHAZAS + CSOK + SERTETT + ATLAGAR + 
    ##     PC1 + PC2, data = df2, index = c("JARAS_NEV", "EV"), listw = listw36, 
    ##     model = "within", effect = "individual", lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -40.79970  -2.28129  -0.25005   1.77701 115.29025 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.347068   0.031386  11.058 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## BERUHAZAS -0.00566403  0.00108974 -5.1976 2.019e-07 ***
    ## CSOK       0.00623925  0.00573560  1.0878   0.27668    
    ## SERTETT   -0.00062998  0.00032326 -1.9489   0.05131 .  
    ## ATLAGAR    0.94973092  0.06603832 14.3815 < 2.2e-16 ***
    ## PC1       -1.83090936  0.58016963 -3.1558   0.00160 ** 
    ## PC2       -0.91763770  0.54747765 -1.6761   0.09371 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
paic(model3_0)
```

    ## [1] 11943.54

``` r
paic(model3_1)
```

    ## [1] 11937.2

``` r
paic(model3_2)
```

    ## [1] 11937.51

``` r
pbic(model3_0)
```

    ## [1] 12014.62

``` r
pbic(model3_1)
```

    ## [1] 11986.4

``` r
pbic(model3_2)
```

    ## [1] 11981.24

AIC modell3_1-et preferálja, BIC modell3_2-t. BIC mellett döntök.

``` r
model3 = model3_2
```

#### Modell 4

CSOK nélkül

``` r
model4_0 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + BUNOZES + SERTETT + PEDAGOGUS + ATLAGAR + VEDONO + PC1 + PC2, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model4_0)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + BUNOZES + 
    ##     SERTETT + PEDAGOGUS + ATLAGAR + VEDONO + PC1 + PC2, data = df2, 
    ##     index = c("JARAS_NEV", "EV"), listw = listw36, model = "within", 
    ##     effect = "individual", lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -41.54154  -2.24867  -0.27788   1.74321 115.04944 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.354034   0.031468  11.251 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA      -1.1122e-03  2.1606e-03 -0.5147 0.6067309    
    ## MUNKA      1.8616e-02  9.4067e-02  0.1979 0.8431241    
    ## BERUHAZAS -4.9742e-03  1.3242e-03 -3.7563 0.0001724 ***
    ## BUNOZES   -5.9768e-05  1.7128e-04 -0.3490 0.7271183    
    ## SERTETT   -7.4981e-04  3.9133e-04 -1.9160 0.0553604 .  
    ## PEDAGOGUS  8.9962e-02  4.5531e-01  0.1976 0.8433712    
    ## ATLAGAR    9.8124e-01  8.6948e-02 11.2854 < 2.2e-16 ***
    ## VEDONO    -4.9154e-01  5.3609e-01 -0.9169 0.3592027    
    ## PC1       -1.6061e+00  5.9745e-01 -2.6882 0.0071836 ** 
    ## PC2       -8.5975e-01  5.5158e-01 -1.5587 0.1190691    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model4_1 = spml(LAKAS_PRED ~ SZJA + BERUHAZAS + SERTETT + ATLAGAR + PC1 + PC2, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model4_1)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ SZJA + BERUHAZAS + SERTETT + ATLAGAR + 
    ##     PC1 + PC2, data = df2, index = c("JARAS_NEV", "EV"), listw = listw36, 
    ##     model = "within", effect = "individual", lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -41.56431  -2.19050  -0.29791   1.74012 114.99265 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.355043   0.031365   11.32 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA      -0.00101883  0.00141572 -0.7197 0.4717404    
    ## BERUHAZAS -0.00504408  0.00130583 -3.8627 0.0001121 ***
    ## SERTETT   -0.00084647  0.00032605 -2.5961 0.0094280 ** 
    ## ATLAGAR    0.98741472  0.08078999 12.2220 < 2.2e-16 ***
    ## PC1       -1.60051201  0.58947899 -2.7151 0.0066250 ** 
    ## PC2       -0.86230741  0.54672195 -1.5772 0.1147421    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model4_2 = spml(LAKAS_PRED ~ BERUHAZAS + SERTETT + ATLAGAR + PC1 + PC2, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model4_2)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ BERUHAZAS + SERTETT + ATLAGAR + PC1 + 
    ##     PC2, data = df2, index = c("JARAS_NEV", "EV"), listw = listw36, 
    ##     model = "within", effect = "individual", lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -41.05520  -2.22611  -0.27883   1.74199 115.16601 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.350561   0.031256  11.216 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## BERUHAZAS -0.00555821  0.00108471 -5.1241 2.989e-07 ***
    ## SERTETT   -0.00075780  0.00030051 -2.5218  0.011677 *  
    ## ATLAGAR    0.95469555  0.06592222 14.4822 < 2.2e-16 ***
    ## PC1       -1.71429181  0.57149834 -2.9996  0.002703 ** 
    ## PC2       -0.88010852  0.54652397 -1.6104  0.107316    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
model4_3 = spml(LAKAS_PRED ~ BERUHAZAS + SERTETT + ATLAGAR + PC1, df2,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

summary(model4_3)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = LAKAS_PRED ~ BERUHAZAS + SERTETT + ATLAGAR + PC1, 
    ##     data = df2, index = c("JARAS_NEV", "EV"), listw = listw36, 
    ##     model = "within", effect = "individual", lag = T, spatial.error = "none")
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -41.43348  -2.17797  -0.28075   1.79870 115.13108 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.352327   0.031262   11.27 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## BERUHAZAS -0.00566695  0.00108341 -5.2307 1.689e-07 ***
    ## SERTETT   -0.00077183  0.00030058 -2.5678  0.010234 *  
    ## ATLAGAR    0.96881351  0.06533304 14.8288 < 2.2e-16 ***
    ## PC1       -1.78677147  0.56980191 -3.1358  0.001714 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
paic(model4_0)
```

    ## [1] 11945.14

``` r
paic(model4_1)
```

    ## [1] 11938.18

``` r
paic(model4_2)
```

    ## [1] 11936.68

``` r
paic(model4_3)
```

    ## [1] 11937.27

``` r
pbic(model4_0)
```

    ## [1] 12010.75

``` r
pbic(model4_1)
```

    ## [1] 11981.92

``` r
pbic(model4_2)
```

    ## [1] 11974.96

``` r
pbic(model4_3)
```

    ## [1] 11970.08

AIC modell4_2-t preferalja, BIC modell4_3/t, BIC szerint dontok.

``` r
model4 = model4_3
```

### AIC, BIC

``` r
#valamiert nem mukodik a lapply/sapply... -val :/
data.frame(AIC = c(paic(model1), paic(model2), paic(model3), paic(model4)) ,
           BIC = c(pbic(model1), pbic(model2), pbic(model3), pbic(model4)))
```

    ##        AIC      BIC
    ## 1 11927.62 11987.76
    ## 2 11933.74 11988.41
    ## 3 11937.51 11981.24
    ## 4 11937.27 11970.08

# SEM

``` r
sem = "
  # measurement model
    EU =~ VEDONO + HO_FORG_RB + HO_FORG_OSSZ + FGYHO_SZOLG_SZAM + HO_SZOLG_SZAM + HO_APOLO_SZAM + HGYO_SZAM
  # regressions
    LAKAS_PRED ~ SZJA + MUNKA + ATLAGAR + EU
  # residual correlations
    ATLAGAR ~~ SZJA + EU
    SZJA ~~ MUNKA + BERUHAZAS + EU
    VEDONO ~~ HO_APOLO_SZAM + HO_SZAM + HO_FORG_OSSZ + HO_FORG_RB
    HO_FORG_RB ~~ HGYO_SZAM + HO_APOLO_SZAM + HO_SZOLG_SZAM + FGYHO_SZOLG_SZAM + HO_FORG_OSSZ 
    HO_FORG_OSSZ ~~ HGYO_SZAM + HO_APOLO_SZAM + HO_SZOLG_SZAM + FGYHO_SZOLG_SZAM
    FGYHO_SZOLG_SZAM ~~ HGYO_SZAM + HO_APOLO_SZAM + HO_SZOLG_SZAM
    HO_SZOLG_SZAM ~~ HO_APOLO_SZAM + HGYO_SZAM
    HO_APOLO_SZAM ~~ HGYO_SZAM
"

fit = sem(sem, data = df2012)
```

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, :
    ## lavaan WARNING: some observed variances are (at least) a factor 1000 times
    ## larger than others; use varTable(fit) to investigate

    ## Warning in lav_data_full(data = data, group = group, cluster = cluster, : lavaan WARNING: some observed variances are larger than 1000000
    ##   lavaan NOTE: use varTable(fit) to investigate

    ## Warning in lavaan::lavaan(model = sem, data = df2012, model.type = "sem", : lavaan WARNING:
    ##     the optimizer warns that a solution has NOT been found!

``` r
summary(fit, standardized = T)
```

    ## lavaan 0.6.16 did NOT end normally after 10000 iterations
    ## ** WARNING ** Estimates below are most likely unreliable
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        48
    ## 
    ##   Number of observations                           175
    ## 
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Latent Variables:
    ##                    Estimate    Std.Err  z-value  P(>|z|)   Std.lv    Std.all
    ##   EU =~                                                                     
    ##     VEDONO              1.000                                 5.269    0.051
    ##     HO_FORG_RB       4687.095       NA                    24694.382    1.000
    ##     HO_FORG_OSSZ     4739.434       NA                    24970.137    1.000
    ##     FGYHO_SZOLG_SZ      1.305       NA                        6.878    0.137
    ##     HO_SZOLG_SZAM       2.330       NA                       12.277    0.099
    ##     HO_APOLO_SZAM       4.276       NA                       22.529    0.101
    ##     HGYO_SZAM           1.885       NA                        9.931    0.089
    ## 
    ## Regressions:
    ##                    Estimate    Std.Err  z-value  P(>|z|)   Std.lv    Std.all
    ##   LAKAS_PRED ~                                                              
    ##     SZJA               -0.009       NA                       -0.009   -0.169
    ##     MUNKA               0.079       NA                        0.079    0.034
    ##     ATLAGAR             2.049       NA                        2.049    0.889
    ##     EU                 -0.089       NA                       -0.466   -0.046
    ## 
    ## Covariances:
    ##                       Estimate    Std.Err  z-value  P(>|z|)   Std.lv    Std.all
    ##   SZJA ~~                                                                      
    ##     ATLAGAR              681.817       NA                      681.817    0.809
    ##   EU ~~                                                                        
    ##     ATLAGAR              -14.867       NA                       -2.822   -0.642
    ##   SZJA ~~                                                                      
    ##     MUNKA                 60.359       NA                       60.359    0.071
    ##     BERUHAZAS          11136.156       NA                    11136.156    0.255
    ##   EU ~~                                                                        
    ##     SZJA                -557.028       NA                     -105.726   -0.551
    ##  .VEDONO ~~                                                                    
    ##    .HO_APOLO_SZAM          2.088       NA                        2.088    0.000
    ##     HO_SZAM             2790.507       NA                     2790.507    0.995
    ##    .HO_FORG_OSSZ       95692.671       NA                    95692.671    2.676
    ##    .HO_FORG_RB         94683.816       NA                    94683.816    8.827
    ##  .HO_FORG_RB ~~                                                                
    ##    .HGYO_SZAM          11328.306       NA                    11328.306    0.976
    ##    .HO_APOLO_SZAM      -7965.894       NA                    -7965.894   -0.344
    ##    .HO_SZOLG_SZAM       6815.563       NA                     6815.563    0.532
    ##    .FGYHO_SZOLG_SZ    -51580.397       NA                   -51580.397   -9.992
    ##    .HO_FORG_OSSZ      -67869.418       NA                   -67869.418   -1.886
    ##  .HO_FORG_OSSZ ~~                                                              
    ##    .HGYO_SZAM           4048.203       NA                     4048.203    0.105
    ##    .HO_APOLO_SZAM     -23341.861       NA                   -23341.861   -0.302
    ##    .HO_SZOLG_SZAM      -1263.969       NA                    -1263.969   -0.030
    ##    .FGYHO_SZOLG_SZ    -56441.312       NA                   -56441.312   -3.279
    ##  .FGYHO_SZOLG_SZAM ~~                                                          
    ##    .HGYO_SZAM           5122.807       NA                     5122.807    0.923
    ##    .HO_APOLO_SZAM      10365.301       NA                    10365.301    0.936
    ##    .HO_SZOLG_SZAM       5643.468       NA                     5643.468    0.921
    ##  .HO_SZOLG_SZAM ~~                                                             
    ##    .HO_APOLO_SZAM      27480.170       NA                    27480.170    0.999
    ##    .HGYO_SZAM          13782.132       NA                    13782.132    1.000
    ##  .HO_APOLO_SZAM ~~                                                             
    ##    .HGYO_SZAM          24906.468       NA                    24906.468    0.999
    ## 
    ## Variances:
    ##                    Estimate    Std.Err  z-value  P(>|z|)   Std.lv    Std.all
    ##    .VEDONO          10659.057       NA                    10659.057    0.997
    ##    .HO_FORG_RB     -10794.351       NA                   -10794.351   -0.000
    ##    .HO_FORG_OSSZ   120002.014       NA                   120002.014    0.000
    ##    .FGYHO_SZOLG_SZ   2468.499       NA                     2468.499    0.981
    ##    .HO_SZOLG_SZAM   15208.771       NA                    15208.771    0.990
    ##    .HO_APOLO_SZAM   49729.406       NA                    49729.406    0.990
    ##    .HGYO_SZAM       12489.419       NA                    12489.419    0.992
    ##    .LAKAS_PRED         38.945       NA                       38.945    0.378
    ##     SZJA            36753.102       NA                    36753.102    1.000
    ##     MUNKA              19.506       NA                       19.506    1.000
    ##     ATLAGAR            19.346       NA                       19.346    1.000
    ##     BERUHAZAS       51785.183       NA                    51785.183    1.000
    ##     HO_SZAM           737.190       NA                      737.190    1.000
    ##     EU                 27.758       NA                        1.000    1.000

``` r
vartable(fit)
```

    ##                name idx nobs    type exo user      mean          var nlev lnam
    ## 1            VEDONO  37  175 numeric   0    0     5.141 5.820000e-01    0     
    ## 2        HO_FORG_RB  35  175 numeric   0    0 63569.589 1.498855e+08    0     
    ## 3      HO_FORG_OSSZ  31  175 numeric   0    0 66120.469 1.525984e+08    0     
    ## 4  FGYHO_SZOLG_SZAM  26  175 numeric   0    0     2.585 2.264000e+00    0     
    ## 5     HO_SZOLG_SZAM  23  175 numeric   0    0     5.354 7.320000e-01    0     
    ## 6     HO_APOLO_SZAM  22  175 numeric   0    0     5.996 1.312000e+00    0     
    ## 7         HGYO_SZAM  21  175 numeric   0    0     1.310 1.600000e-01    0     
    ## 8        LAKAS_PRED  38  175 numeric   0    0     8.176 8.976800e+01    0     
    ## 9              SZJA   5  175 numeric   0    0   692.955 2.686650e+04    0     
    ## 10            MUNKA   6  175 numeric   0    0    54.795 1.960900e+01    0     
    ## 11          ATLAGAR  19  175 numeric   0    0     6.446 1.461600e+01    0     
    ## 12        BERUHAZAS   7  175 numeric   0    0   339.526 2.575309e+04    0     
    ## 13          HO_SZAM  20  175 numeric   0    0     5.012 5.700000e-01    0

## Normalizált változókkal

``` r
df2012$n_LAKAS_PRED = scale(df2012$LAKAS_PRED)
df2012$n_HO_FORG_RB = scale(df2012$HO_FORG_RB)
df2012$n_HO_FORG_OSSZ = scale(df2012$HO_FORG_OSSZ)
df2012$n_SZJA = scale(df2012$SZJA)
df2012$n_BERUHAZAS = scale(df2012$BERUHAZAS)

sem = "
  # measurement model
    EU =~ VEDONO + n_HO_FORG_RB + n_HO_FORG_OSSZ + FGYHO_SZOLG_SZAM + HO_SZOLG_SZAM + HO_APOLO_SZAM + HGYO_SZAM
  # regressions
    n_LAKAS_PRED ~ n_SZJA + n_BERUHAZAS + MUNKA + ATLAGAR + EU
  # residual correlations
    ATLAGAR ~~ n_SZJA + EU
    n_SZJA ~~ MUNKA + n_BERUHAZAS + EU
    VEDONO ~~ HO_APOLO_SZAM + HO_SZAM + n_HO_FORG_OSSZ + n_HO_FORG_RB
    n_HO_FORG_RB ~~ HGYO_SZAM + HO_APOLO_SZAM + HO_SZOLG_SZAM + FGYHO_SZOLG_SZAM + n_HO_FORG_OSSZ 
    n_HO_FORG_OSSZ ~~ HGYO_SZAM + HO_APOLO_SZAM + HO_SZOLG_SZAM + FGYHO_SZOLG_SZAM
    FGYHO_SZOLG_SZAM ~~ HGYO_SZAM + HO_APOLO_SZAM + HO_SZOLG_SZAM
    HO_SZOLG_SZAM ~~ HO_APOLO_SZAM + HGYO_SZAM
    HO_APOLO_SZAM ~~ HGYO_SZAM
"

fit = sem(sem, data = df2012)
summary(fit, standardized = T)
```

    ## lavaan 0.6.16 ended normally after 147 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        49
    ## 
    ##   Number of observations                           175
    ## 
    ## Model Test User Model:
    ##                                                       
    ##   Test statistic                               489.097
    ##   Degrees of freedom                                42
    ##   P-value (Chi-square)                           0.000
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   EU =~                                                                 
    ##     VEDONO            1.000                               0.154    0.208
    ##     n_HO_FORG_RB      5.731    2.284    2.509    0.012    0.882    0.885
    ##     n_HO_FORG_OSSZ    5.714    2.286    2.500    0.012    0.879    0.882
    ##     FGYHO_SZOLG_SZ    5.778    2.409    2.398    0.016    0.889    0.593
    ##     HO_SZOLG_SZAM     3.605    1.486    2.425    0.015    0.555    0.650
    ##     HO_APOLO_SZAM     4.364    1.784    2.447    0.014    0.672    0.588
    ##     HGYO_SZAM        -1.187    0.516   -2.300    0.021   -0.183   -0.458
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   n_LAKAS_PRED ~                                                        
    ##     n_SZJA           -0.213    0.103   -2.076    0.038   -0.213   -0.187
    ##     n_BERUHAZAS       0.150    0.050    2.981    0.003    0.150    0.145
    ##     MUNKA            -0.014    0.012   -1.204    0.229   -0.014   -0.061
    ##     ATLAGAR           0.203    0.057    3.592    0.000    0.203    0.751
    ##     EU               -1.153    1.799   -0.641    0.522   -0.177   -0.172
    ## 
    ## Covariances:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   n_SZJA ~~                                                                
    ##     ATLAGAR              2.538    0.316    8.029    0.000    2.538    0.738
    ##   EU ~~                                                                    
    ##     ATLAGAR             -0.492    0.205   -2.403    0.016   -3.195   -0.838
    ##   n_SZJA ~~                                                                
    ##     MUNKA                0.875    0.208    4.208    0.000    0.875    0.220
    ##     n_BERUHAZAS          0.123    0.044    2.790    0.005    0.123    0.136
    ##   EU ~~                                                                    
    ##     n_SZJA              -0.095    0.040   -2.376    0.018   -0.619   -0.686
    ##  .VEDONO ~~                                                                
    ##    .HO_APOLO_SZAM        0.047    0.034    1.369    0.171    0.047    0.070
    ##     HO_SZAM              0.169    0.042    3.992    0.000    0.169    0.310
    ##    .n_HO_FORG_OSSZ       0.013    0.041    0.308    0.758    0.013    0.037
    ##    .n_HO_FORG_RB         0.023    0.041    0.548    0.584    0.023    0.067
    ##  .n_HO_FORG_RB ~~                                                          
    ##    .HGYO_SZAM           -0.056    0.068   -0.822    0.411   -0.056   -0.340
    ##    .HO_APOLO_SZAM        0.185    0.244    0.760    0.447    0.185    0.432
    ##    .HO_SZOLG_SZAM        0.125    0.202    0.618    0.536    0.125    0.414
    ##    .FGYHO_SZOLG_SZ       0.286    0.326    0.878    0.380    0.286    0.509
    ##    .n_HO_FORG_OSSZ       0.215    0.320    0.671    0.502    0.215    0.984
    ##  .n_HO_FORG_OSSZ ~~                                                        
    ##    .HGYO_SZAM           -0.057    0.068   -0.831    0.406   -0.057   -0.341
    ##    .HO_APOLO_SZAM        0.186    0.244    0.763    0.445    0.186    0.430
    ##    .HO_SZOLG_SZAM        0.122    0.202    0.605    0.545    0.122    0.401
    ##    .FGYHO_SZOLG_SZ       0.291    0.326    0.892    0.373    0.291    0.512
    ##  .FGYHO_SZOLG_SZAM ~~                                                      
    ##    .HGYO_SZAM           -0.247    0.079   -3.146    0.002   -0.247   -0.577
    ##    .HO_APOLO_SZAM        0.543    0.261    2.075    0.038    0.543    0.486
    ##    .HO_SZOLG_SZAM        0.418    0.216    1.938    0.053    0.418    0.534
    ##  .HO_SZOLG_SZAM ~~                                                         
    ##    .HO_APOLO_SZAM        0.423    0.162    2.608    0.009    0.423    0.706
    ##    .HGYO_SZAM           -0.025    0.046   -0.545    0.586   -0.025   -0.109
    ##  .HO_APOLO_SZAM ~~                                                         
    ##    .HGYO_SZAM           -0.069    0.056   -1.220    0.222   -0.069   -0.210
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .VEDONO            0.526    0.057    9.249    0.000    0.526    0.957
    ##    .n_HO_FORG_RB      0.216    0.321    0.674    0.500    0.216    0.217
    ##    .n_HO_FORG_OSSZ    0.221    0.320    0.690    0.490    0.221    0.222
    ##    .FGYHO_SZOLG_SZ    1.461    0.369    3.963    0.000    1.461    0.649
    ##    .HO_SZOLG_SZAM     0.421    0.136    3.091    0.002    0.421    0.577
    ##    .HO_APOLO_SZAM     0.852    0.205    4.162    0.000    0.852    0.654
    ##    .HGYO_SZAM         0.126    0.020    6.309    0.000    0.126    0.791
    ##    .n_LAKAS_PRED      0.408    0.046    8.801    0.000    0.408    0.383
    ##     n_SZJA            0.814    0.084    9.730    0.000    0.814    1.000
    ##     n_BERUHAZAS       0.994    0.106    9.354    0.000    0.994    1.000
    ##     MUNKA            19.497    2.084    9.354    0.000   19.497    1.000
    ##     ATLAGAR          14.533    1.554    9.354    0.000   14.533    1.000
    ##     HO_SZAM           0.567    0.061    9.354    0.000    0.567    1.000
    ##     EU                0.024    0.019    1.248    0.212    1.000    1.000

Munka, EU nem szignifikans?
