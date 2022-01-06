library(readxl)
luces <- read_excel("G:/Mi unidad/Tesis doctorado/Ensayo luces/dataset.xlsx")
head(luces)
names <- c('dw','CuP','ZnP','AsP','NP','PP','KP','CaP','MgP','CE','Cl')
luces[,names] <- lapply(luces[,names] , as.numeric)
names <- c('Site','trt','light','hum')
luces[,names] <- lapply(luces[,names] , as.factor)
data <- as.data.frame(luces)
str(data)

data<- subset(data, select=c(trt,light,hum,CE,Cl,NP,KP,CaP,MgP,PP,ZnP,CuP,AsP,dw,Site)) 
Ct <- data[data$Site=="Ct",]
Cp <- data[data$Site=="Cp",]

#Data distribution#
library(goft) #gamma and weibull tests #https://cran.r-projeCt.org/web/packages/goft/goft.pdf
library(nortest) #normality test
library(gamlss)
library(gamlss.dist)
library(gamlss.add)
library(fitdistrplus)
library(actuar)

#Ct analysis

hist(Ct$NP)  
ad.test(log(Ct$NP)) # lognormal
gamma_test(Ct$NP)   # gamma is better

hist(Ct$PP)
ad.test(Ct$PP)      # normal
gamma_test(Ct$PP)   # gamma is better

hist(Ct$KP)
ad.test(Ct$KP)      # normal
gamma_test(Ct$KP)  # gamma is better

hist(Ct$CaP)
ad.test(Ct$CaP)     #not normal nor lognormal
gamma_test(Ct$CaP)  #gamma #not loggamma cuz negative values
weibull_test(Ct$CaP) #gamma is acceptable but weibull would be better
f<-fitdist(Ct$CaP,"gamma") #pruebo con norm, lnorm, gamma
plot(f)

hist(Ct$MgP)
ad.test(log(Ct$MgP))  #lognormal
gamma_test(Ct$MgP)    # gamma is better

hist(Ct$AsP)
ad.test(log(Ct$AsP))  # lognormal 
gamma_test(Ct$AsP)    # gamma is better

hist(Ct$CuP)
gamma_test(Ct$CuP)      #gamma
gamma_test(log(Ct$CuP)) 

hist(Ct$ZnP) 
gamma_test(log(Ct$ZnP))      #gamma

hist(Ct$dw)  
gamma_test(Ct$dw)       #gamma

hist(Ct$CE)  
gamma_test(Ct$CE)       #gamma

hist(Ct$Cl)  
gamma_test(Ct$Cl)       #gamma

#Cp analysis
hist(Cp$NP)  
ad.test(log(Cp$NP)) # lognormal
gamma_test(log(Cp$NP))   # gamma is better

hist(Cp$PP)
ad.test(Cp$PP)      # normal
gamma_test(Cp$PP)   # gamma is better

hist(Cp$KP)
ad.test(Cp$KP)      # normal
gamma_test(Cp$KP)  # gamma is better

hist(Cp$CaP)
ad.test(Cp$CaP)     #normal is better
gamma_test(Cp$CaP)  #gamma #not loggamma cuz negative values
weibull_test(Cp$CaP) #gamma is acceptable but weibull would be better
#normal o gamma 

hist(Cp$MgP)
ad.test(log(Cp$MgP))  #lognormal
gamma_test(Cp$MgP)    # gamma

hist(Cp$AsP)
ad.test(log(Cp$AsP))  # lognormal 
gamma_test(Cp$AsP)    # gamma is better

hist(Cp$CuP)
gamma_test(Cp$CuP)      #gamma
gamma_test(log(Cp$CuP)) 

hist(Cp$ZnP) 
gamma_test(log(Cp$ZnP))      #not gamma
weibull_test(Cp$ZnP) 
ad.test(log(Cp$ZnP)) #lognormal is better but not good 
z<-fitdist(Cp$ZnP,"gamma")
plot(z)

hist(Cp$dw)  
gamma_test(Cp$dw)       #gamma
ad.test(log(Cp$dw))     #lognormal is better

hist(Cp$CE)  
gamma_test(Cp$CE)       #gamma

hist(Cp$Cl)  
gamma_test(Cp$Cl)       #gamma


library(car)

glm.dw.Ct<-glm(dw~light*hum, family = Gamma,data=Ct)
summary(glm.dw.Ct)
plot(resid(glm.dw.Ct)) #Ok
Anova(glm.dw.Ct,type=3)

glm.CE.Ct<-glm(CE~light*hum, family = Gamma,data=Ct)
summary(glm.CE.Ct)
plot(resid(glm.CE.Ct)) #Ok
Anova(glm.CE.Ct,type=3)

glm.Cl.Ct<-glm(Cl~light*hum, family = Gamma,data=Ct)
summary(glm.Cl.Ct)
plot(resid(glm.Cl.Ct)) #Ok
Anova(glm.Cl.Ct,type=3) #interaction is not significant
glm.Cl.Ct<-glm(Cl~light+hum, family = Gamma,data=Ct)
Anova(glm.Cl.Ct,type=2)

glm.CuP.Ct<-glm(CuP~light*hum, family = Gamma,data=Ct)
summary(glm.CuP)
plot(resid(glm.CuP)) #Ok
Anova(glm.CuP.Ct,type=3) #interaction is not significant
glm.CuP.Ct<-glm(CuP~light+hum, family = Gamma,data=Ct)
Anova(glm.CuP.Ct,type=2)

#Sandy soil

glm.dw.Cp<-glm(dw~light*hum, family = Gamma,data=Cp)
summary(glm.dw.Cp)
plot(resid(glm.dw.Cp)) #Ok
Anova(glm.dw.Cp,type=3)
glm.dw.Cp<-glm(dw~light+hum, family = Gamma,data=Cp)
Anova(glm.dw.Cp,type=2)

glm.CE.Cp<-glm(CE~light*hum, family = Gamma,data=Cp)
summary(glm.CE.Cp)
plot(resid(glm.CE.Cp)) #Ok
Anova(glm.CE.Cp,type=3)

glm.Cl.Cp<-glm(Cl~light*hum, family = Gamma,data=Cp)
summary(glm.Cl.Cp)
plot(resid(glm.Cl.Cp)) #Ok
Anova(glm.Cl.Cp,type=3) #interaction is not significant
glm.Cl.Cp<-glm(Cl~light+hum, family = Gamma,data=Cp)
Anova(glm.Cl.Cp,type=2)

glm.CuP.Cp<-glm(CuP~light*hum, family = Gamma,data=Cp)
summary(glm.CuP)
plot(resid(glm.CuP)) #Ok
Anova(glm.CuP.Cp,type=3) #interaction is not significant
glm.CuP.Cp<-glm(CuP~light+hum, family = Gamma,data=Cp)
Anova(glm.CuP.Cp,type=2)


#IONOME Ct
glm.NP.Ct<-glm(NP~light*hum, family = Gamma,data=Ct)
summary(glm.NP.Ct)
plot(resid(glm.NP.Ct)) #Ok
Anova(glm.NP.Ct,type=3)
glm.NP.Ct<-glm(NP~light+hum, family = Gamma,data=Ct)
Anova(glm.NP.Ct,type=2)

glm.PP.Ct<-glm(PP~light*hum, family = Gamma,data=Ct)
summary(glm.PP.Ct)
plot(resid(glm.PP.Ct)) #Ok
Anova(glm.PP.Ct,type=3)
glm.PP.Ct<-glm(PP~light+hum, family = Gamma,data=Ct)
Anova(glm.PP.Ct,type=2)

glm.KP.Ct<-glm(KP~light*hum, family = Gamma,data=Ct)
summary(glm.KP.Ct)
plot(resid(glm.KP.Ct)) #Ok
Anova(glm.KP.Ct,type=3) #interactions is not significant
glm.KP.Ct<-glm(KP~light+hum, family = Gamma,data=Ct)
Anova(glm.KP.Ct,type=2)

glm.CaP.Ct<-glm(CaP~light*hum, family = Gamma,data=Ct)
summary(glm.CaP.Ct)
plot(resid(glm.CaP.Ct)) #Ok
Anova(glm.CaP.Ct,type=3) #interactions is not significant
glm.CaP.Ct<-glm(CaP~light+hum, family = Gamma,data=Ct)
Anova(glm.CaP.Ct,type=2)

glm.MgP.Ct<-glm(MgP~light*hum, family = Gamma,data=Ct)
summary(glm.MgP.Ct)
plot(resid(glm.MgP.Ct)) #Ok
Anova(glm.MgP.Ct,type=3)
glm.MgP.Ct<-glm(MgP~light+hum, family = Gamma,data=Ct)
Anova(glm.MgP.Ct,type=2)

glm.ZnP.Ct<-glm(ZnP~light*hum, family = Gamma,data=Ct)
summary(glm.ZnP.Ct)
plot(resid(glm.ZnP.Ct)) #Ok
Anova(glm.ZnP.Ct,type=3)
glm.ZnP.Ct<-glm(ZnP~light+hum, family = Gamma,data=Ct)
Anova(glm.ZnP.Ct,type=2)

glm.AsP.Ct<-glm(AsP~light*hum, family = Gamma,data=Ct)
summary(glm.AsP.Ct)
plot(resid(glm.AsP.Ct)) #Ok
Anova(glm.AsP.Ct,type=3) #interactions is not significant
glm.AsP.Ct<-glm(AsP~light+hum, family = Gamma,data=Ct)
Anova(glm.AsP.Ct,type=2)

#Sandy soil

glm.dw.Cp<-glm(dw~light*hum, family = Gamma,data=Cp)
summary(glm.dw.Cp)
plot(resid(glm.dw.Cp)) #Ok
Anova(glm.dw.Cp,type=3)
glm.dw.Cp<-glm(dw~light+hum, family = Gamma,data=Cp)
Anova(glm.dw.Cp,type=2)

glm.CE.Cp<-glm(CE~light*hum, family = Gamma,data=Cp)
summary(glm.CE.Cp)
plot(resid(glm.CE.Cp)) #Ok
Anova(glm.CE.Cp,type=3)

glm.Cl.Cp<-glm(Cl~light*hum, family = Gamma,data=Cp)
summary(glm.Cl.Cp)
plot(resid(glm.Cl.Cp)) #Ok
Anova(glm.Cl.Cp,type=3) #interaction is not significant
glm.Cl.Cp<-glm(Cl~light+hum, family = Gamma,data=Cp)
Anova(glm.Cl.Cp,type=2)

glm.CaP.Cp<-glm(CaP~light*hum, family = Gamma,data=Cp)
summary(glm.CaP)
plot(resid(glm.CaP)) #Ok
Anova(glm.CaP.Cp,type=3) #interaction is not significant
glm.CaP.Cp<-glm(CaP~light+hum, family = Gamma,data=Cp)
Anova(glm.CaP.Cp,type=2)

#IONOME SS
glm.NP.Cp<-glm(NP~light*hum, family = Gamma,data=Cp)
summary(glm.NP.Cp)
plot(resid(glm.NP.Cp)) #Ok
Anova(glm.NP.Cp,type=3)
glm.NP.Cp<-glm(NP~light+hum, family = Gamma,data=Cp)
Anova(glm.NP.Cp,type=2)

glm.PP.Cp<-glm(PP~light*hum, family = Gamma,data=Cp)
summary(glm.PP.Cp)
plot(resid(glm.PP.Cp)) #Ok
Anova(glm.PP.Cp,type=3)
glm.PP.Cp<-glm(PP~light+hum, family = Gamma,data=Cp)
Anova(glm.PP.Cp,type=2)

glm.KP.Cp<-glm(KP~light*hum, family = Gamma,data=Cp)
summary(glm.KP.Cp)
plot(resid(glm.KP.Cp)) #Ok
Anova(glm.KP.Cp,type=3) #interactions are not significant
glm.KP.Cp<-glm(KP~light+hum, family = Gamma,data=Cp)
Anova(glm.KP.Cp,type=2)

glm.CaP.Cp<-glm(CaP~light*hum, family = Gamma,data=Cp)
summary(glm.CaP.Cp)
plot(resid(glm.CaP.Cp)) #Ok
Anova(glm.CaP.Cp,type=3) #interaction is not significant
glm.CaP.Cp<-glm(CaP~light+hum, family = Gamma,data=Cp)
Anova(glm.CaP.Cp,type=2)

glm.MgP.Cp<-glm(MgP~light*hum, family = Gamma,data=Cp)
summary(glm.MgP.Cp)
plot(resid(glm.MgP.Cp)) #Ok
Anova(glm.MgP.Cp,type=3)
glm.MgP.Cp<-glm(MgP~light+hum, family = Gamma,data=Cp)
Anova(glm.MgP.Cp,type=2)

glm.ZnP.Cp<-glm(ZnP~light*hum, family = Gamma,data=Cp)
summary(glm.ZnP.Cp)
plot(resid(glm.ZnP.Cp)) #Ok
Anova(glm.ZnP.Cp,type=3)
glm.ZnP.Cp<-glm(ZnP~light+hum, family = Gamma,data=Cp)
Anova(glm.ZnP.Cp,type=2)

glm.AsP.Cp<-glm(AsP~light*hum, family = Gamma,data=Cp)
summary(glm.AsP.Cp)
plot(resid(glm.AsP.Cp)) #Ok
Anova(glm.AsP.Cp,type=3) #interaction is not significant
glm.AsP.Cp<-glm(AsP~light+hum, family = Gamma,data=Cp)
Anova(glm.AsP.Cp,type=2)





