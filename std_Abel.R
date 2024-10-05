
setwd("C:/Users/REACT-BF/Desktop/doucuments lenovo/Antivec grant Data")

library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(readxl)

MB_prevalence<-read_excel("STD.xlsx", sheet = "MB_prevalence")

View(MB_prevalence)

str(MB_prevalence)

summary(MB_prevalence)

MB_prevalence$specie<-as.factor(MB_prevalence$specie)
MB_prevalence$locality<-as.factor(MB_prevalence$locality)
MB_prevalence$Microsporidia_sp_MB_statut<-as.factor(MB_prevalence$Microsporidia_sp_MB_statut)

str(MB_prevalence)

summary(MB_prevalence)

###binomial glm models################

model1 <-glm(Microsporidia_sp_MB_statut~specie+locality, family = binomial, data=MB_prevalence )

check_overdispersion(model1)

summary(model1)

##### effect of specie on Microsporidia sp MB prevalence#######

model2 <-glm(Microsporidia_sp_MB_statut~locality, family = binomial, data=MB_prevalence )

summary(model2)

anova(model1, model2)####specie is no significant

#### effect of locality on Microsporidia sp MB prevalence ######

model3 <-glm(Microsporidia_sp_MB_statut~specie, family = binomial, data=MB_prevalence )

summary(model3)

anova(model1, model3)#### Locality is significant######


####Simplification du modèle avec library car ###########

library(car)

model1 <-glm(Microsporidia_sp_MB_statut~specie+locality, family = binomial, data=MB_prevalence )

summary(model1)

Anova(model1, type=2)

#### Barplots de la prevalence de Microsporidi sp MB en fonction des espèces et des localités####

attach(MB_prevalence)

p<-table(Microsporidia_sp_MB_statut,specie,locality)
p

library(Hmisc)
library(tidyr)
d1<-data.frame(p)
d1
d2<-spread(d1,Microsporidia_sp_MB_statut,Freq) ## cela permet de passer d'un format long (6 lignes 3 variables) ? un format large (3 lignes, 3 variables)
d2 ## on remarque qu'automatiquement cela a cr?? 2 nouvelles colonnes "0" et "1" provenant des 2 modalit?s de la pr?c?dente variable "infection".
names(d2)[3] <- "infected"
names(d2)[4] <- "uninfected"
d2

CI<-binconf(d2$infected,(d2$infected+d2$uninfected),method="wilson") ## calcul des proportions et des Intervalles de confiance
CI

d3<-cbind(d2,CI) ## la fonction cbind permet de "coller" deux dataframes diff?rents
d3

library(ggplot2)

ggplot(d3, aes(x=specie, y=PointEst, fill=specie)) +
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2,  position=position_dodge(.9))+
  xlab("species")+
  ylab("Microsporidia_sp_MB_prevalence in An. gambiae complex strains")+
  coord_cartesian(ylim = c(0, 1))+
  theme_bw()+
  facet_wrap(~locality)


#### barplots with ggplot2 on Microsporidia sp MB prevalence in An. gambiae strains#####

attach(MB_prevalence)

p<-table(Microsporidia_sp_MB_statut,specie)
p

library(Hmisc)
library(tidyr)

d1<-data.frame(p)
d1

d2<-spread(d1,Microsporidia_sp_MB_statut,Freq) ## cela permet de passer d'un format long (6 lignes 3 variables) ? un format large (3 lignes, 3 variables)

d2 ## on remarque qu'automatiquement cela a cr?? 2 nouvelles colonnes "0" et "1" provenant des 2 modalit?s de la pr?c?dente variable "infection".

names(d2)[2] <- "infected"
names(d2)[3] <- "uninfected"

d2

CI<-binconf(d2$infected,(d2$infected+d2$uninfected),method="wilson") ## calcul des proportions et des Intervalles de confiance
CI

d3<-cbind(d2,CI) ## la fonction cbind permet de "coller" deux dataframes diff?rents
d3

library(ggplot2)

ggplot(d3, aes(x=specie, y=PointEst, fill=specie)) +
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2,  position=position_dodge(.9))+
  xlab("species")+
  ylab("Microsporidia_sp_MB_prevalence")+
  coord_cartesian(ylim = c(0, 0.3))+
  theme_bw()

#### barplot of MB prevalence in two molecular form of complex gambiae################################

library (ggplot2)
library(ggthemes)
library(scales)
library(dplyr)

ggplot(MB_prevalence, aes(x=specie, 
                          fill=Microsporidia_sp_MB_statut)) +
  geom_bar(stat = "count", 
           position="fill")+
  scale_y_continuous(labels= scales::percent)
  
  

### Microsporidia sp MB transmission between generation from F0 to F3#######

setwd("C:/Users/REACT-BF/Desktop/doucuments lenovo/Antivec grant Data")

library(readxl)

MB_generation<- read_excel("STD.xlsx", sheet = "MB_generation_transmission")

View(MB_generation)

str(MB_generation)

summary(MB_generation)

#### transforms varaibles as factors####

attach(MB_generation)

MB_generation$species<-as.factor(MB_generation$species)

MB_generation$locality<-as.factor(MB_generation$locality)

MB_generation$generation<-as.factor(MB_generation$generation)

MB_generation$Microsporida_sp_MB_statut<-as.factor(MB_generation$Microsporida_sp_MB_statut)

str(MB_generation)

summary(MB_generation)

###Analyses statistiques avec binomial glm models#####

model8 <-glm(Microsporida_sp_MB_statut~species+locality+generation, family = binomial, data=MB_generation)

library(performance)

check_overdispersion(model8)

summary(model8)

### Effect of species on MB_transmission _generation######

model9 <-glm(Microsporida_sp_MB_statut~locality+generation, family = binomial, data=MB_generation )

summary(model9)

anova(model8, model9)####species is  significant

### Effect of locality on MB_transmission_generation####"

model10 <-glm(Microsporida_sp_MB_statut~species+generation, family = binomial, data=MB_generation )

summary(model10)

anova(model8, model10)### locality is significant

#### effect of generation on MB_transmission_generation#########

model11 <-glm(Microsporida_sp_MB_statut~species+locality, family = binomial, data=MB_generation )

summary(model11)

anova(model8, model11)### generation is no significant

#### simplification du modèle avec library car ##############################

library(car)

model8 <-glm(Microsporida_sp_MB_statut~species+locality+generation, family = binomial, data=MB_generation)

Anova(model8, type=2)

#### barplots with ggplot2 on MB_transmission_generation in An. gambiae strains#######

attach(MB_generation)

str(MB_generation)

summary(MB_generation)

p<-table(Microsporida_sp_MB_statut,species, generation)
p

library(Hmisc)
library(tidyr)

d1<-data.frame(p)
d1

d2<-spread(d1,Microsporida_sp_MB_statut,Freq) ## cela permet de passer d'un format long (6 lignes 3 variables) ? un format large (3 lignes, 3 variables)
d2 ## on remarque qu'automatiquement cela a cr?? 2 nouvelles colonnes "0" et "1" provenant des 2 modalit?s de la pr?c?dente variable "infection".

names(d2)[3] <- "infected"
names(d2)[4] <- "uninfected"
d2

CI<-binconf(d2$infected,(d2$infected+d2$uninfected),method="wilson") ## calcul des proportions et des Intervalles de confiance
CI

d3<-cbind(d2,CI) ## la fonction cbind permet de "coller" deux dataframes diff?rents
d3

library(ggplot2)

ggplot(d3, aes(x=generation, y=PointEst, fill=generation)) +
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2,  position=position_dodge(.9))+
  xlab("species")+
  ylab("MB_prevalence")+
  coord_cartesian(ylim = c(0, 0.45))+
  theme_bw()+
  facet_wrap(~species)

#### bar chart with ggplot 2#######
library (ggplot2)
library(ggthemes)
library(scales)
library(dplyr)

attach(MB_generation)

View(MB_generation)

ggplot(MB_generation, aes(x=species, 
                          fill=Microsporida_sp_MB_statut)) +
  geom_bar(stat = "count", 
           position="fill")+
  scale_y_continuous(labels= scales::percent)+
  facet_wrap(~generation)
 





#### STD of Microsporidia sp MB in An. gambiae complex mosquitoes#######

setwd("C:/Users/REACT-BF/Desktop/doucuments lenovo/Antivec grant Data")

library(readxl)

STD<- read_excel("STD.xlsx", sheet = "std_data")

View(STD)

str(STD)

summary(STD)

#### transform variables as factors #######

STD$specie<-as.factor(STD$specie)

STD$sex_ratio<-as.factor(STD$sex_ratio)

STD$mode_of_mating<-as.factor(STD$mode_of_mating)

STD$Microsporidia_sp_MB_statut<-as.factor(STD$Microsporidia_sp_MB_statut)

str(STD)

summary(STD)

#### statistical analysis###########

model5 <-glm(Microsporidia_sp_MB_statut~sex_ratio+mode_of_mating, family = binomial, data=STD )

summary(model5)

check_overdispersion(model5)

#### effect of sex_ration on STD of MB##############

model6 <-glm(Microsporidia_sp_MB_statut~mode_of_mating, family = binomial, data=STD )

summary(model6)

anova(model5, model6)####sex ratio is no significant

#####effect of mode of mating on STD of MB#########

model7 <-glm(Microsporidia_sp_MB_statut~sex_ratio, family = binomial, data=STD )

summary(model7)

anova(model5, model7)### mating mode is significant

##### simplification du modèle####################

library(car)

model5 <-glm(Microsporidia_sp_MB_statut~sex_ratio+mode_of_mating, family = binomial, data=STD )

summary(model5)

Anova(model5, type=2)

#### barplots of MB  STD with ggplot2########## 

attach(STD)

p<-table(Microsporidia_sp_MB_statut,mode_of_mating)

p


library(Hmisc)
library(tidyr)

d1<-data.frame(p)

d1

d2<-spread(d1,Microsporidia_sp_MB_statut,Freq) ## cela permet de passer d'un format long (6 lignes 3 variables) ? un format large (3 lignes, 3 variables)

d2 ## on remarque qu'automatiquement cela a cr?? 2 nouvelles colonnes "0" et "1" provenant des 2 modalit?s de la pr?c?dente variable "infection".

names(d2)[2] <- "infected"

names(d2)[3] <- "uninfected"

d2

CI<-binconf(d2$infected,(d2$infected+d2$uninfected),method="wilson") ## calcul des proportions et des Intervalles de confiance

CI

d3<-cbind(d2,CI) ## la fonction cbind permet de "coller" deux dataframes diff?rents

d3

library(ggplot2)

ggplot(d3, aes(x=mode_of_mating, y=PointEst, fill=mode_of_mating)) +
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Lower, ymax=Upper),width=.2,  position=position_dodge(.9))+
  xlab("mating moode")+
  ylab("MB_prevalence")+
  coord_cartesian(ylim = c(0, 0.75))+
  theme_bw()

#### barplot on std MB prevalence#####

library (ggplot2)
library(ggthemes)
library(scales)
library(dplyr)

attach(STD)


ggplot(STD, aes(x=mode_of_mating, 
                          fill=Microsporidia_sp_MB_statut)) +
  geom_bar(stat = "count", 
           position="fill")+
  scale_y_continuous(labels= scales::percent)


################ P. falciparum prevalence in positive mosquitoes to Microsporidia sp MB######

setwd("C:/Users/REACT-BF/Desktop/doucuments lenovo/Antivec grant Data")

library(readxl)

MB_Pf<- read_excel("STD.xlsx", sheet = "pcr_MB_Pf")

view(MB_Pf)

MB_Pf$Microsporidia_sp_MB_statut<-as.factor(MB_Pf$Microsporidia_sp_MB_statut)
MB_Pf$sporozoite_P_falciparum_statut<-as.factor(MB_Pf$sporozoite_P_falciparum_statut)

str(MB_Pf)

summary(MB_Pf)

#### statistical analysis###########

attach(MB_Pf)

p<-table(Microsporidia_sp_MB_statut, sporozoite_P_falciparum_statut)

p
chisq.test(p)

############barplot with ggplot2 on sporozoite rate on positive mosquitoes to MB#############################################

library (ggplot2)
library(ggthemes)
library(scales)
library(dplyr)

attach(MB_Pf)

view(MB_Pf)

ggplot(data=MB_Pf, aes(x=Microsporidia_sp_MB_statut, 
                fill=sporozoite_P_falciparum_statut )) +
  geom_bar(stat = "count", 
           position="fill")+
  scale_y_continuous(labels= scales::percent)
