#### William Keilsohn
### Factorial Anova's and Nested Anova's 

### Load Packages:
library(dplyr)
library(agricolae)
library(xlsx)

### Begin Problems:

## Comparisons of Ph and nutrients on algeal growth with differing levels of interaction

# No significant Interaction
# One significant factor
FA1<-aov(PhR1~Nitr*Oz,PhRD)
shapiro.test(residuals(FA1))
summary(FA1)

# No significant interaction
# two significant factors
FA2<- aov(PhR2~Nitr*Oz,PhRD)
shapiro.test(residuals(FA2))
PhRD2<-mutate(PhRD, Ph2 = log(PhRD$PhR2))
FA22<-aov(Ph2~Nitr*Oz,PhRD2)
shapiro.test(residuals(FA22))
summary(FA2)

# Significant Interaction
FA3 <- aov(PhR3~Nitr*Oz,PhRD)
shapiro.test(residuals(FA3))
summary(FA3)
CA1<-aov(PhR3~Trt, data = PhRD)
summary(CA1)
H1<-HSD.test(CA1,"Trt")
H1

## Plant height is measured in responce to temperature and irrigation
## Plants are placed in a series of plots and exposed to conditions from both variables
## Tests look to determine the best conditions for plants overall
PLD<-mutate(PLD, Trt = paste(PLD$Temp, PLD$Irr)) ### No more copying and pasting for you!
PA1 <- aov(Height~Temp*Irr,PLD)
shapiro.test(residuals(PA1))
summary(PA1) ### The interaction is normally destributed so...
PA2<- aov(Height~Trt,data = PLD)
summary(PA2)
TukeyHSD(PA2)
HSD.ou1<-HSD.test(PA2, "Trt")

# Fish weight was measured as a result of diet
# Additionally fish were provided either clean or "effluent" water believed to be contaminated
# Fish were examined for the effects of the food and the water.
FiD<-mutate(FiD, Trt = paste(FiD$Feeding, FiD$Water))
FDA1<- aov(Size~Feeding*Water, FiD)
shapiro.test(residuals(FDA1))
summary(FDA1)
FDA2<-aov(Size~Trt, data = FiD)
summary(FDA2)
HSD.out<-TukeyHSD(FDA2)
HSD.out2<-HSD.test(FDA2,"Trt")

## Model the effects of a drog on cholestoral level using a nested design
nested.out<-aov(chol~Drug+Error(Drug/Source),NestD)# Error is not in the data. It is "made" from between the group and level.
# Ignore the "Error model is singular" error.

## Model the effects of a series of treatments on glycogen content of rat liver
## As multiple samples were taken from the same rat(s) a nested model is used
RAD$Treatment<-factor(RAD$Treatment)
RAD$Rat<-factor(RAD$Rat)
RAD$Liver<-factor(RAD$Liver)
nested.out2<-aov(Glycogen~Treatment+Error(Treatment/Rat/Liver),RAD)
summary(nested.out2)
