library(RXKCD)
getXKCD(323)
getXKCD(527)
getXKCD(552)

acs <- read.table('http://www.jaredlander.com/data/acs_ny.csv', sep=',', header=TRUE, stringsAsFactors=FALSE)
dim(acs)
head(acs)
acs$Income <- acs$FamilyIncome >= 150000
head(acs)
tail(acs)
library(ggplot2)
library(useful)
ggplot(acs, aes(x=FamilyIncome)) + geom_density(fill='grey',color='grey') + geom_vline(xintercept=150000)

income1 <- glm(Income ~ HouseCosts + NumWorkers + OwnRent + NumBedrooms + FamilyType, data=acs, family=binomial)
summary(income1)
library(coefplot)
coefplot(income1, sort='mag')
coef(income1)


# poisson
head(acs)
children1 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent, data=acs,family=poisson)
summary(children1)
coefplot(children1, sort='mag')
unique(acs$FamilyType)
# quasipoisson
children2 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent, data=acs, family=quasipoisson)
summary(children2)
multiplot(children1, children2)+ theme(legend.position='bottom')
