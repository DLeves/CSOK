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

rm(i, predicted, gammodel)
```

#### Eü adatok GAM modelljei

VEDONO + HO_FORG_RB + HO_FORG_OSSZ + FGYHO_SZOLG_SZAM + HO_SZOLG_SZAM +
HO_APOLO_SZAM + HGYO_SZAM

``` r
sapply(df[,20:37], function(x) sum(is.na(x)))
```

    ##              HO_SZAM            HGYO_SZAM        HO_APOLO_SZAM 
    ##                    1                   50                    1 
    ##        HO_SZOLG_SZAM      HGYO_SZOLG_SZAM       FHO_SZOLG_SZAM 
    ##                    1                   42                   41 
    ##     FGYHO_SZOLG_SZAM   HO_HELY_SZOLG_SZAM HGYO_HELY_SZOLG_SZAM 
    ##                   52                  447                 1160 
    ##         HO_TMED_SZAM      HGYO_TMED_SZAMA         HO_FORG_OSSZ 
    ##                 1575                 1582                    1 
    ##       HGYO_FORG_OSSZ         HGYO_FORG_RB         HGYO_FORG_RK 
    ##                   42                   42                   58 
    ##           HO_FORG_RB           HO_FORG_RK               VEDONO 
    ##                    1                    1                    1

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
corrplot(pcor(na.omit(df[,c(4:8,14:37)]))$estimate, method = "color")
```

    ## Warning in pcor(na.omit(df[, c(4:8, 14:37)])): The inverse of
    ## variance-covariance matrix is calculated using Moore-Penrose generalized matrix
    ## invers due to its determinant of zero.

![](tdk_files/figure-gfm/parcialis%20corrplot-1.png)<!-- -->

``` r
# corrplot(pcor(na.omit(df[,c(4:8,14:19)]))$estimate, method = "color", addCoef.col = "darkolivegreen")
```

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
    ## -42.02227  -2.26348  -0.29101   1.84223 115.24878 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.351480   0.031614  11.118 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA              -4.7023e-03  2.2938e-03 -2.0500 0.0403672 *  
    ## MUNKA             -2.2844e-02  1.0017e-01 -0.2280 0.8196084    
    ## BERUHAZAS         -4.4759e-03  1.3480e-03 -3.3205 0.0008986 ***
    ## CSOKTRENDalacsony  2.5704e+00  9.5966e-01  2.6785 0.0073957 ** 
    ## CSOKTRENDközepes   3.0819e+00  7.8760e-01  3.9131 9.114e-05 ***
    ## CSOKTRENDmagas     7.9941e-01  7.4255e-01  1.0766 0.2816712    
    ## BUNOZES           -7.4589e-05  1.7100e-04 -0.4362 0.6626891    
    ## SERTETT           -6.6433e-04  3.9119e-04 -1.6982 0.0894703 .  
    ## PEDAGOGUS          4.9373e-01  4.6214e-01  1.0683 0.2853639    
    ## ATLAGAR            1.0684e+00  8.6460e-02 12.3567 < 2.2e-16 ***
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
    ## -41.82608  -2.21033  -0.31175   1.72959 115.17952 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.357011   0.031539   11.32 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##                      Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA              -3.7982e-03  2.2725e-03 -1.6714  0.094649 .  
    ## MUNKA             -6.1937e-02  1.0011e-01 -0.6187  0.536111    
    ## BERUHAZAS         -4.5791e-03  1.3517e-03 -3.3876  0.000705 ***
    ## CSOKTRENDalacsony  2.6612e+00  8.6378e-01  3.0808  0.002064 ** 
    ## CSOKTRENDmagas     1.5724e+00  6.9582e-01  2.2599  0.023830 *  
    ## BUNOZES           -7.0103e-05  1.7136e-04 -0.4091  0.682475    
    ## SERTETT           -7.2427e-04  3.9111e-04 -1.8518  0.064051 .  
    ## PEDAGOGUS          3.9736e-01  4.6268e-01  0.8588  0.390433    
    ## ATLAGAR            1.0385e+00  8.5629e-02 12.1280 < 2.2e-16 ***
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
    ## -42.56826  -2.17501  -0.27069   1.72066 115.30826 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.370050   0.031122   11.89 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA      -2.1368e-03  2.1480e-03 -0.9948 0.3198463    
    ## MUNKA     -9.0043e-02  9.9246e-02 -0.9073 0.3642659    
    ## BERUHAZAS -4.6125e-03  1.3528e-03 -3.4096 0.0006505 ***
    ## CSOK       1.5015e-02  7.6514e-03  1.9624 0.0497161 *  
    ## BUNOZES   -7.0221e-05  1.7146e-04 -0.4096 0.6821323    
    ## SERTETT   -7.4899e-04  3.9118e-04 -1.9147 0.0555280 .  
    ## PEDAGOGUS  2.3891e-01  4.5828e-01  0.5213 0.6021415    
    ## ATLAGAR    1.0503e+00  8.5937e-02 12.2220 < 2.2e-16 ***
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
    ## -42.56826  -2.17501  -0.27069   1.72066 115.30826 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.370050   0.031122   11.89 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##              Estimate  Std. Error t-value  Pr(>|t|)    
    ## SZJA      -2.1368e-03  2.1480e-03 -0.9948 0.3198463    
    ## MUNKA     -9.0043e-02  9.9246e-02 -0.9073 0.3642659    
    ## BERUHAZAS -4.6125e-03  1.3528e-03 -3.4096 0.0006505 ***
    ## CSOK       1.5015e-02  7.6514e-03  1.9624 0.0497161 *  
    ## BUNOZES   -7.0221e-05  1.7146e-04 -0.4096 0.6821323    
    ## SERTETT   -7.4899e-04  3.9118e-04 -1.9147 0.0555280 .  
    ## PEDAGOGUS  2.3891e-01  4.5828e-01  0.5213 0.6021415    
    ## ATLAGAR    1.0503e+00  8.5937e-02 12.2220 < 2.2e-16 ***
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
    ## 1 11937.56 12003.17
    ## 2 11943.90 12004.04
    ## 3 11947.52 12002.19
    ## 4 11947.52 12002.19

# SEM

``` r
sem = "
  # measurement model
    EU =~ VEDONO + HO_FORG_RB + HO_FORG_OSSZ + FGYHO_SZOLG_SZAM + HO_SZOLG_SZAM + HO_APOLO_SZAM + HGYO_SZAM
  # regressions
    LAKAS ~ SZJA + MUNKA + ATLAGAR + EU + CSOK
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
summary(fit, standardized = T)
```
