library(UsingR)
head(father.son)
ggplot(father.son,aes(x=fheight, y=sheight)) + geom_point() + geom_smooth(method='lm')
heightsMod <- lm(sheight ~ fheight, data=father.son)
heightsMod
summary(heightsMod)
housing <- read.table('http://www.jaredlander.com/data/housing.csv', sep=',', header=TRUE, stringsAsFactors=FALSE)
head(housing)
View(housing)
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt", 
                    "SqFt", "Income", "IncomePerSqFt", "Expense", 
                    "ExpensePerSqFt", "NetIncome", "Value", 
                    "ValuePerSqFt", "Boro")
names(housing)
ggplot(housing,aes(x=ValuePerSqFt)) + geom_histogram()
ggplot(housing,aes(x=ValuePerSqFt,fill=Boro)) + geom_histogram()
housing <- housing[housing$Units < 1000,]

head(model.matrix(ValuePerSqFt ~ Units, data=housing))
head(model.matrix(ValuePerSqFt ~ Units+Income, data=housing))
head(model.matrix(~ Boro, data=housing))
head(model.matrix(~ Boro+Units, data=housing))
head(model.matrix(~ Units + Income, data=housing))
head(model.matrix(~ Units * Income, data=housing))

# regression 
house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing)
house1
summary(house1)
library (coefplot)
coefplot(house1, sort='mag')
house2 <- lm(ValuePerSqFt ~ Units * SqFt + Boro, data=housing)
house3 <- lm(ValuePerSqFt ~ Units:SqFt + Boro, data=housing)
house4 <- lm(ValuePerSqFt ~ Units * SqFt*Income, data=housing)
multiplot(house1, house2, house3)
# making prediction

housing <- read.table('http://www.jaredlander.com/data/housingNew.csv', sep=',', header=TRUE, stringsAsFactors=FALSE)
dim(housing)
summary(housing)
View(housing)
housePredict <- predict(house1, newdata=housingNew, se.fit=TRUE, interval='prediction', level=.95)
housing <- read.table('http://www.jaredlander.com/data/housing.csv', sep=',', header=TRUE, stringsAsFactors=FALSE)
housingNew<- read.table('http://www.jaredlander.com/data/housingNew.csv', sep=',', header=TRUE, stringsAsFactors=FALSE)
housePredict <- predict(house1, newdata=housingNew, se.fit=TRUE, interval='prediction', level=.95)
head(housePredict$fit)
View(housePredict)
head(housingNew)
summary(house1)

AIC(house1,house2, house3, house4)
BIC(house1,house2, house3, house4)
