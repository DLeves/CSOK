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

    ## Error in solve.default(cvx): system is computationally singular: reciprocal condition number = 1.73515e-26

``` r
corrplot(pcor(na.omit(df[,c(4:8,14:16,19:37)]))$estimate, method = "color")
```

    ## Error in solve.default(cvx): system is computationally singular: reciprocal condition number = 1.92942e-25

Nem tudom miert dob error, elozoleg mukodott rendesen. Azert le van
mentve a kep:

``` r
knitr::include_graphics("tdk_files/figure-gfm/parcialis corrplot-1.png")
```

![](tdk_files/figure-gfm/parcialis%20corrplot-1.png)<!-- -->

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
    ## -41.16654  -2.35169  -0.26661   1.85807 115.01182 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.335409   0.031904  10.513 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA              -0.00423614  0.00233068 -1.8176 0.0691320 .  
    ## MUNKA              0.02062992  0.10171396  0.2028 0.8392734    
    ## BERUHAZAS         -0.00430252  0.00134975 -3.1876 0.0014344 ** 
    ## CSOKTRENDalacsony  2.44430564  0.96179571  2.5414 0.0110410 *  
    ## CSOKTRENDközepes   3.00609988  0.78841000  3.8129 0.0001374 ***
    ## CSOKTRENDmagas     0.73224408  0.74177289  0.9872 0.3235671    
    ## BUNOZES           -0.00008140  0.00017085 -0.4764 0.6337588    
    ## SERTETT           -0.00060558  0.00039219 -1.5441 0.1225585    
    ## PEDAGOGUS          0.44804351  0.46177692  0.9703 0.3319171    
    ## ATLAGAR            1.02401157  0.08798155 11.6389 < 2.2e-16 ***
    ## VEDONO            -0.35732568  0.53663982 -0.6659 0.5055022    
    ## PC1               -1.50319461  0.59665968 -2.5194 0.0117572 *  
    ## PC2               -0.92341069  0.54986403 -1.6793 0.0930851 .  
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
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -41.42511  -2.28946  -0.28322   1.84850 115.08441 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda  0.33769    0.03175  10.636 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA              -0.00427851  0.00179483 -2.3838 0.0171348 *  
    ## BERUHAZAS         -0.00425513  0.00134389 -3.1663 0.0015440 ** 
    ## CSOKTRENDalacsony  2.31504018  0.92950849  2.4906 0.0127525 *  
    ## CSOKTRENDközepes   2.84197432  0.74716397  3.8037 0.0001426 ***
    ## CSOKTRENDmagas     0.59740327  0.65886253  0.9067 0.3645553    
    ## SERTETT           -0.00069127  0.00033106 -2.0880 0.0367939 *  
    ## ATLAGAR            1.03991485  0.08409461 12.3660 < 2.2e-16 ***
    ## PC1               -1.53471672  0.58940621 -2.6038 0.0092187 ** 
    ## PC2               -0.95103783  0.54543796 -1.7436 0.0812250 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
paic(model1_0)
```

    ## [1] 11933.56

``` r
paic(model1_1)
```

    ## [1] 11927.16

``` r
pbic(model1_0)
```

    ## [1] 12015.57

``` r
pbic(model1_1)
```

    ## [1] 11987.3

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
    ## -40.93265  -2.31112  -0.29167   1.81516 114.94541 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.340360   0.031853  10.685 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA              -3.3502e-03  2.3053e-03 -1.4533  0.146152    
    ## MUNKA             -1.7706e-02  1.0166e-01 -0.1742  0.861735    
    ## BERUHAZAS         -4.3944e-03  1.3538e-03 -3.2460  0.001171 ** 
    ## CSOKTRENDalacsony  2.6060e+00  8.6540e-01  3.0113  0.002602 ** 
    ## CSOKTRENDmagas     1.4970e+00  6.9518e-01  2.1534  0.031291 *  
    ## BUNOZES           -7.6803e-05  1.7121e-04 -0.4486  0.653728    
    ## SERTETT           -6.6513e-04  3.9208e-04 -1.6964  0.089805 .  
    ## PEDAGOGUS          3.5340e-01  4.6225e-01  0.7645  0.444563    
    ## ATLAGAR            9.9136e-01  8.7226e-02 11.3655 < 2.2e-16 ***
    ## VEDONO            -4.2435e-01  5.3598e-01 -0.7917  0.428516    
    ## PC1               -1.5089e+00  5.9780e-01 -2.5241  0.011601 *  
    ## PC2               -9.2007e-01  5.5170e-01 -1.6677  0.095375 .  
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
    ## -41.18545  -2.26461  -0.30458   1.79218 114.93827 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.343290   0.031687  10.834 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA              -0.00393598  0.00177518 -2.2172  0.026607 *  
    ## BERUHAZAS         -0.00438086  0.00134700 -3.2523  0.001145 ** 
    ## CSOKTRENDalacsony  2.41929901  0.83194756  2.9080  0.003638 ** 
    ## CSOKTRENDmagas     1.27760666  0.61949995  2.0623  0.039177 *  
    ## SERTETT           -0.00072425  0.00033109 -2.1875  0.028708 *  
    ## ATLAGAR            1.01569234  0.08364099 12.1435 < 2.2e-16 ***
    ## PC1               -1.56809110  0.59047246 -2.6557  0.007915 ** 
    ## PC2               -0.96831009  0.54686355 -1.7707  0.076617 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
paic(model2_0)
```

    ## [1] 11939.69

``` r
paic(model2_1)
```

    ## [1] 11933.3

``` r
pbic(model2_0)
```

    ## [1] 12016.23

``` r
pbic(model2_1)
```

    ## [1] 11987.97

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
    ## -41.69084  -2.29312  -0.27476   1.79037 115.07548 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.353103   0.031446  11.229 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA      -1.6343e-03  2.1722e-03 -0.7524  0.451832    
    ## MUNKA     -4.8100e-02  1.0078e-01 -0.4773  0.633161    
    ## BERUHAZAS -4.4110e-03  1.3553e-03 -3.2546  0.001136 ** 
    ## CSOK       1.4452e-02  7.6436e-03  1.8908  0.058655 .  
    ## BUNOZES   -7.6346e-05  1.7131e-04 -0.4457  0.655849    
    ## SERTETT   -6.9125e-04  3.9216e-04 -1.7626  0.077961 .  
    ## PEDAGOGUS  1.9700e-01  4.5780e-01  0.4303  0.666973    
    ## ATLAGAR    1.0019e+00  8.7563e-02 11.4426 < 2.2e-16 ***
    ## VEDONO    -4.9183e-01  5.3557e-01 -0.9183  0.358455    
    ## PC1       -1.5528e+00  5.9722e-01 -2.6001  0.009319 ** 
    ## PC2       -8.4746e-01  5.5101e-01 -1.5380  0.124044    
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
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -41.86671  -2.22595  -0.31254   1.74449 114.96138 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.356239   0.031303   11.38 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA      -0.00260163  0.00167425 -1.5539 0.1202080    
    ## BERUHAZAS -0.00444956  0.00134790 -3.3011 0.0009631 ***
    ## CSOK       0.01171518  0.00678046  1.7278 0.0840269 .  
    ## SERTETT   -0.00074278  0.00033125 -2.2424 0.0249388 *  
    ## ATLAGAR    1.02845429  0.08414684 12.2221 < 2.2e-16 ***
    ## PC1       -1.62447231  0.58945392 -2.7559 0.0058532 ** 
    ## PC2       -0.90168992  0.54670122 -1.6493 0.0990803 .  
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
    ## -40.79390  -2.27168  -0.25005   1.77529 115.29394 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.347825   0.031371  11.088 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## BERUHAZAS -0.00566847  0.00108951 -5.2028 1.963e-07 ***
    ## CSOK       0.00618757  0.00573496  1.0789  0.280623    
    ## SERTETT   -0.00062966  0.00032320 -1.9482  0.051390 .  
    ## ATLAGAR    0.94934838  0.06599936 14.3842 < 2.2e-16 ***
    ## PC1       -1.81224710  0.58012734 -3.1239  0.001785 ** 
    ## PC2       -0.91401097  0.54735974 -1.6699  0.094948 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
paic(model3_0)
```

    ## [1] 11943.15

``` r
paic(model3_1)
```

    ## [1] 11936.74

``` r
paic(model3_2)
```

    ## [1] 11937.1

``` r
pbic(model3_0)
```

    ## [1] 12014.23

``` r
pbic(model3_1)
```

    ## [1] 11985.95

``` r
pbic(model3_2)
```

    ## [1] 11980.84

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
    ##     Min.  1st Qu.   Median  3rd Qu.     Max. 
    ## -41.5429  -2.2539  -0.2783   1.7431 115.0406 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.354923   0.031444  11.287 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA      -0.00116796  0.00215975 -0.5408 0.5886571    
    ## MUNKA      0.02052928  0.09404338  0.2183 0.8271986    
    ## BERUHAZAS -0.00497177  0.00132374 -3.7559 0.0001727 ***
    ## BUNOZES   -0.00006006  0.00017124 -0.3507 0.7257917    
    ## SERTETT   -0.00074997  0.00039126 -1.9168 0.0552646 .  
    ## PEDAGOGUS  0.09914671  0.45523209  0.2178 0.8275898    
    ## ATLAGAR    0.98214984  0.08693806 11.2971 < 2.2e-16 ***
    ## VEDONO    -0.47365041  0.53595451 -0.8838 0.3768306    
    ## PC1       -1.58652710  0.59743349 -2.6556 0.0079174 ** 
    ## PC2       -0.85702747  0.55146671 -1.5541 0.1201636    
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
    ##     Min.  1st Qu.   Median  3rd Qu.     Max. 
    ## -41.5682  -2.2062  -0.2980   1.7279 114.9932 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.355928   0.031343  11.356 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA      -0.00104360  0.00141533 -0.7374  0.460907    
    ## BERUHAZAS -0.00503708  0.00130539 -3.8587  0.000114 ***
    ## SERTETT   -0.00084719  0.00032599 -2.5988  0.009354 ** 
    ## ATLAGAR    0.98775384  0.08076519 12.2299 < 2.2e-16 ***
    ## PC1       -1.57956006  0.58944500 -2.6797  0.007368 ** 
    ## PC2       -0.85846009  0.54658701 -1.5706  0.116280    
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
    ## -41.04701  -2.22041  -0.27901   1.72421 115.17076 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda  0.35131    0.03124  11.245 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## BERUHAZAS -0.00556361  0.00108450 -5.1301 2.896e-07 ***
    ## SERTETT   -0.00075639  0.00030045 -2.5175  0.011818 *  
    ## ATLAGAR    0.95425731  0.06588061 14.4846 < 2.2e-16 ***
    ## PC1       -1.69638339  0.57144846 -2.9686  0.002992 ** 
    ## PC2       -0.87674976  0.54640342 -1.6046  0.108586    
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
    ## -41.42351  -2.18876  -0.28007   1.77974 115.13596 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.353094   0.031245  11.301 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## BERUHAZAS -0.00567200  0.00108320 -5.2363 1.638e-07 ***
    ## SERTETT   -0.00077035  0.00030052 -2.5634  0.010367 *  
    ## ATLAGAR    0.96830400  0.06528924 14.8310 < 2.2e-16 ***
    ## PC1       -1.76844339  0.56973355 -3.1040  0.001909 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
paic(model4_0)
```

    ## [1] 11944.74

``` r
paic(model4_1)
```

    ## [1] 11937.73

``` r
paic(model4_2)
```

    ## [1] 11936.26

``` r
paic(model4_3)
```

    ## [1] 11936.83

``` r
pbic(model4_0)
```

    ## [1] 12010.35

``` r
pbic(model4_1)
```

    ## [1] 11981.47

``` r
pbic(model4_2)
```

    ## [1] 11974.53

``` r
pbic(model4_3)
```

    ## [1] 11969.63

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
    ## 1 11927.16 11987.30
    ## 2 11933.30 11987.97
    ## 3 11937.10 11980.84
    ## 4 11936.83 11969.63

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

    ## lavaan 0.6.15 did NOT end normally after 10000 iterations
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
    ##     VEDONO              1.000                                67.580    0.410
    ##     HO_FORG_RB        294.406       NA                    19895.896    1.000
    ##     HO_FORG_OSSZ      298.004       NA                    20139.062    1.000
    ##     FGYHO_SZOLG_SZ      0.029       NA                        1.963    0.017
    ##     HO_SZOLG_SZAM       0.020       NA                        1.350    0.008
    ##     HO_APOLO_SZAM       0.450       NA                       30.382    0.213
    ##     HGYO_SZAM          -0.004       NA                       -0.251   -0.002
    ## 
    ## Regressions:
    ##                    Estimate    Std.Err  z-value  P(>|z|)   Std.lv    Std.all
    ##   LAKAS_PRED ~                                                              
    ##     SZJA               -0.008       NA                       -0.008   -0.148
    ##     MUNKA               0.093       NA                        0.093    0.037
    ##     ATLAGAR             2.109       NA                        2.109    0.944
    ##     EU                  0.001       NA                        0.083    0.008
    ## 
    ## Covariances:
    ##                       Estimate    Std.Err  z-value  P(>|z|)   Std.lv    Std.all
    ##   SZJA ~~                                                                      
    ##     ATLAGAR              807.840       NA                      807.840    0.848
    ##   EU ~~                                                                        
    ##     ATLAGAR              131.273       NA                        1.942    0.397
    ##   SZJA ~~                                                                      
    ##     MUNKA                140.725       NA                      140.725    0.165
    ##     BERUHAZAS           2858.505       NA                     2858.505    0.092
    ##   EU ~~                                                                        
    ##     SZJA                5489.263       NA                       81.226    0.418
    ##  .VEDONO ~~                                                                    
    ##    .HO_APOLO_SZAM       7883.121       NA                     7883.121    0.376
    ##     HO_SZAM                1.637       NA                        1.637    0.014
    ##    .HO_FORG_OSSZ      102351.903       NA                   102351.903    3.156
    ##    .HO_FORG_RB         85130.633       NA                    85130.633    1.840
    ##  .HO_FORG_RB ~~                                                                
    ##    .HGYO_SZAM          -3907.925       NA                    -3907.925   -0.111
    ##    .HO_APOLO_SZAM     -17835.243       NA                   -17835.243   -0.416
    ##    .HO_SZOLG_SZAM       4056.519       NA                     4056.519    0.077
    ##    .FGYHO_SZOLG_SZ       511.172       NA                      511.172    0.014
    ##    .HO_FORG_OSSZ      -63879.169       NA                   -63879.169   -0.963
    ##  .HO_FORG_OSSZ ~~                                                              
    ##    .HGYO_SZAM           7798.359       NA                     7798.359    0.315
    ##    .HO_APOLO_SZAM      -8131.738       NA                    -8131.738   -0.270
    ##    .HO_SZOLG_SZAM      -9716.760       NA                    -9716.760   -0.264
    ##    .FGYHO_SZOLG_SZ    -11269.399       NA                   -11269.399   -0.453
    ##  .FGYHO_SZOLG_SZAM ~~                                                          
    ##    .HGYO_SZAM         -13212.392       NA                   -13212.392   -1.000
    ##    .HO_APOLO_SZAM      -2897.362       NA                    -2897.362   -0.180
    ##    .HO_SZOLG_SZAM      16824.708       NA                    16824.708    0.855
    ##  .HO_SZOLG_SZAM ~~                                                             
    ##    .HO_APOLO_SZAM       7573.864       NA                     7573.864    0.318
    ##    .HGYO_SZAM         -16553.111       NA                   -16553.111   -0.846
    ##  .HO_APOLO_SZAM ~~                                                             
    ##    .HGYO_SZAM           3130.536       NA                     3130.536    0.196
    ## 
    ## Variances:
    ##                    Estimate    Std.Err  z-value  P(>|z|)   Std.lv    Std.all
    ##    .VEDONO          22617.313       NA                    22617.313    0.832
    ##    .HO_FORG_RB      94681.202       NA                    94681.202    0.000
    ##    .HO_FORG_OSSZ    46506.565       NA                    46506.565    0.000
    ##    .FGYHO_SZOLG_SZ  13287.288       NA                    13287.288    1.000
    ##    .HO_SZOLG_SZAM   29158.890       NA                    29158.890    1.000
    ##    .HO_APOLO_SZAM   19439.622       NA                    19439.622    0.955
    ##    .HGYO_SZAM       13142.214       NA                    13142.214    1.000
    ##    .LAKAS_PRED         38.242       NA                       38.242    0.320
    ##     SZJA            37805.550       NA                    37805.550    1.000
    ##     MUNKA              19.309       NA                       19.309    1.000
    ##     ATLAGAR            23.992       NA                       23.992    1.000
    ##     BERUHAZAS       25592.021       NA                    25592.021    1.000
    ##     HO_SZAM             0.567       NA                        0.567    1.000
    ##     EU               4567.040       NA                        1.000    1.000
