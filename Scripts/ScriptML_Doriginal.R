
install.packages("openxlsx")   
# Install openxlsx R package
library("readxl") 

dataO<-read_excel("C:/Users/dassy/Desktop/ING3/S2/ADD/Projet ADD/Data.xlsx") 

dim(dataO)

colnames(dataO)
summary(dataO)
cor(dataO)

boxplot(DFdataO$age)
boxplot(DFdataO$Medu )
boxplot(DFdataO$Fedu)
boxplot(DFdataO$traveltime)
boxplot(DFdataO$studytime)
boxplot(DFdataO$freetime)
boxplot(DFdataO$health)
boxplot(DFdataO$absences)

#Nous allons Maintenant diviser notre Dataset en 2:
#  -Dataset train
#  -Dataset test

DFdataO <- data.frame(dataO)

dt = sort(sample(nrow(DFdataO), nrow(DFdataO)*.8))
nrow(DFdataO)
DatatrainO <- DFdataO[dt,]
DatatestO <- DFdataO[-dt,]
nrow(DatatrainO)
nrow(DatatestO)

# Rergression logistique 

modelO <- glm(Success ~ ., data = DatatrainO)
plot(modelO)

summary(modelO)


# test sur les données test
library(caret)
library(mlbench)


control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# Méthode d'évaluation Cross-Validation
# On crée 10 blocs et on fait de la cross-validation dessus 3 x de suites

metric <- "Accuracy"

preProcess = c("center", "scale")


DatatrainO$Success = as.factor(DatatrainO$Success)

library(car)
library(caret)
library(mlbench)
library(MASS)

m0 <- lm(Success~1,data=DatatrainO)  # choix du modèle avec constante seulement
m0

mf <- lm(Success~.,data=DatatrainO)  # choix du modèle avec constante seulement
mf

step(m0, scope=list(lower=m0, upper=mf),data=DatatrainO, direction="forward")

 
#On change la collonne status en factor

seed <- 7
set.seed(seed)
fit.glm <- train(Success~ age + Medu + Fedu + freetime  , data=DatatrainO, method="glm", metric=metric, trControl=control)

# kNN
set.seed(seed)
fit.knn <- train(Success~ age + Medu + Fedu + freetime , data=DatatrainO, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)

# CART
set.seed(seed)
fit.cart <- train(Success~ age + Medu + Fedu + freetime , data=DatatrainO, method="rpart", metric=metric, trControl=control)

# Bagged CART
set.seed(seed)
fit.treebag <- train(Success~ age + Medu + Fedu + freetime , data=DatatrainO, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(Success~ age + Medu + Fedu + freetime , data=DatatrainO, method="rf", metric=metric, trControl=control)




results <- resamples(list(logistic=fit.glm,knn=fit.knn,cart=fit.cart, bagging=fit.treebag, rf=fit.rf))

results
summary(results)


newModele <- train(Success~., data=DatatrainO, method="treebag", metric=metric, trControl=control)
newModele


bwplot(results)
dotplot(results)



DatatestO$Success = as.factor(DatatestO$Success)

TestPredict <- predict(fit.treebag, newdata= DatatestO)
summary(TestPredict)

confusionMatrix(TestPredict, DatatestO$Success)










