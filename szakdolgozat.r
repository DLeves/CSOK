# initial setup --------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(plm)
library(splm)
library(sp)
library(spdep)
library(mgcv)
library(lmtest)
library(psych)
library(ppcor)

# adat beolvasása, alakítása
df = read.csv("Data/FullDatasetUnpredicted")
df$EV = as.factor(df$EV)
df$JARAS_NEV = as.factor(df$JARAS_NEV)
# df$MEGYE_SZH = as.factor(df$MEGYE_SZH)
# df$BALATON = as.factor(df$BALATON)

cuts = c(-Inf, 1, 75, Inf)
# cuts = c(-Inf, 1, 50, Inf)
labs = c("nincs", "alacsony", "magas")
df$CSOKTREND = cut(df$CSOK, breaks = cuts, labels = labs, include.lowest = T)

rm(cuts, labs)

#*******************************************************************************************************************************
# leíró statisztika, korreláció--------------------------------
#*******************************************************************************************************************************

# adatok felbontása Moranhoz és Gearyhez 

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

# korreláció
cor(df[,4:8], use = "complete.obs")
pcor(na.omit(df[,4:8]))

# sumtrend
sumtrend = df %>%
  group_by(EV) %>%
  summarise(sum = sum(LAKAS, na.rm = T)/175)

balaton = df %>%
  group_by(EV) %>%
    filter(JARAS_NEV == "Siófoki") %>%
      select(EV, JARAS_NEV, LAKAS)

gyor = df %>%
  group_by(EV) %>%
    filter(JARAS_NEV == "Győri") %>%
      select(EV, JARAS_NEV, LAKAS)

bp = df %>%
  group_by(EV) %>%
    filter(JARAS_NEV == "Érdi") %>%
      select(EV, JARAS_NEV, LAKAS)

trendplotdf = data.frame(EV = as.Date(paste(as.character(sumtrend$EV),"1","1",sep = "-")), TREND = sumtrend$sum, BALATON = balaton$LAKAS, GYOR = gyor$LAKAS, BP = bp$LAKAS)
rm(sumtrend, balaton, gyor, bp)

ggplot(trendplotdf, aes(EV))+
  geom_line(aes(y = TREND, color = "Átlag"))+
  geom_line(aes(y = BALATON, color = "Siófoki járás"))+
  geom_line(aes(y = GYOR, color = "Győri járás"))+
  geom_line(aes(y = BP, color = "Érdi járás"))+
  labs(title = "Lakásépítések tízezer lakosra járásonként", x = "Idő", y = "Lakásépítések tízezer lakosra(db)")+
  theme(legend.title = element_blank())+
  theme_minimal()


#*******************************************************************************************************************************
# GAM predict az NA-kra-----------------------------------
#*******************************************************************************************************************************

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

#*******************************************************************************************************************************
# Területi autokorreláció--------------------------------------
#*******************************************************************************************************************************

# szomszédsági mátrixok
listw32 = nb2listw(dnearneigh(df2016[,c(9,10)], 0, 32, longlat = T))
listw36 = nb2listw(dnearneigh(df2016[,c(9,10)], 0, 36, longlat = T))
listw40 = nb2listw(dnearneigh(df2016[,c(9,10)], 0, 40, longlat = T))

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

#*******************************************************************************************************************************
# Modellépítés-----------------------------
#*******************************************************************************************************************************


df2 = df %>% arrange(EV)
listw36 = nb2listw(dnearneigh(df2[df2$EV == "2016",c(9,10)], 0, 36, longlat = T), style = "W")

phtest(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND + MEGYE_SZH + BALATON, df, index = c("JARAS_NEV", "EV"))
phtest(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND, df, index = c("JARAS_NEV", "EV"))
# --> H0 elvet, a fix hatású a jó

pFtest(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND, df, index = c("JARAS_NEV", "EV"))

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

BIC1 = -2 * model1$logLik + (length(model1$coefficients)+1)*log(nrow(df))
BIC2 = -2 * model2$logLik + (length(model2$coefficients)+1)*log(nrow(df))
BIC3 = -2 * model3$logLik + (length(model3$coefficients)+1)*log(nrow(df))
BIC4 = -2 * model4$logLik + (length(model4$coefficients)+1)*log(nrow(df))

AIC1 = 2 * (length(model1$coefficients) + 1) - 2 * model1$logLik
AIC2 = 2 * (length(model2$coefficients) + 1) - 2 * model2$logLik
AIC3 = 2 * (length(model3$coefficients) + 1) - 2 * model3$logLik
AIC4 = 2 * (length(model4$coefficients) + 1) - 2 * model4$logLik

summary(model1)
summary(model2)
summary(model3)
summary(model4)

#*******************************************************************************************************************************
# Balaton nélküli modellek---------------------------------------
#*******************************************************************************************************************************

df3 = df %>% arrange(EV) %>% filter(BALATON == 0)
listw36 = nb2listw(dnearneigh(df3[df3$EV == "2016",c(9,10)], 0, 36, longlat = T), style = "W")

cuts = c(-Inf, 1, 70, 86, Inf)
labs = c("nincs", "alacsony","közepes", "magas")
df3$CSOKTREND = cut(df3$CSOK, breaks = cuts, labels = labs, include.lowest = T)

model5 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND, df3,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

cuts = c(-Inf, 1, 75, Inf)
labs = c("nincs", "alacsony", "magas")
df3$CSOKTREND = cut(df3$CSOK, breaks = cuts, labels = labs, include.lowest = T)

model6 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOKTREND, df3,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

model7 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS + CSOK, df3,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

model8 = spml(LAKAS_PRED ~ SZJA + MUNKA + BERUHAZAS, df3,
              listw = listw36, model = "within", index = c("JARAS_NEV", "EV"), lag = T,
              effect = "individual", spatial.error = "none")

rm(cuts, labs)

BIC5 = -2 * model5$logLik + (length(model5$coefficients)+1)*log(nrow(df))
BIC6 = -2 * model6$logLik + (length(model6$coefficients)+1)*log(nrow(df))
BIC7 = -2 * model7$logLik + (length(model7$coefficients)+1)*log(nrow(df))
BIC8 = -2 * model8$logLik + (length(model8$coefficients)+1)*log(nrow(df))

AIC5 = 2 * (length(model5$coefficients) + 1) - 2 * model5$logLik
AIC6 = 2 * (length(model6$coefficients) + 1) - 2 * model6$logLik
AIC7 = 2 * (length(model7$coefficients) + 1) - 2 * model7$logLik
AIC8 = 2 * (length(model8$coefficients) + 1) - 2 * model8$logLik

summary(model5)
summary(model6)
summary(model7)
summary(model8)

#eddgi volt a szakdoga, innentől