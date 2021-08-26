##########################################################################################################
########################################### DataSet Original #############################################
#  Load le DataSet 

install.packages("openxlsx")   
# Install openxlsx R package
library("readxl") 

dataO<-read_excel("C:/Users/dassy/Desktop/ING3/S2/ADD/Projet ADD/Data.xlsx") 

dim(dataO)

colnames(dataO)
summary(dataO)
cor(dataO)

#Mettre les champs en format numeric
dataO$age <-as.numeric(dataO$age )
dataO$Medu <-as.numeric(dataO$Medu )
dataO$traveltime <-as.numeric(dataO$traveltime )
dataO$Fedu <-as.numeric(dataO$Fedu )
dataO$Score <-as.numeric(dataO$Score )
dataO$freetime <-as.numeric(dataO$freetime )
dataO$health <-as.numeric(dataO$health )
dataO$absences <-as.numeric(dataO$absences )
#Afficher les types des classes du dataFrames
sapply(dataO,class)


cor(dataO)


#Recherche de valeurs aberrantes :


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

# Application de la regression linéaire 

memory.limit(size=56000)

modeleO<-lm(Score~.,data=DatatrainO)
summary(modeleO)

plot(modeleO)


trainO <- DatatrainO 
testO <- DatatestO 
#Prediction sur le train

prediction_lr<-predict(modeleO,DatatrainO)
DatatrainO$Prediction <- prediction_lr
head(DatatrainO)

#Prévision sur le test
prediction_lr<-predict(modeleO,DatatestO)
DatatestO$Prediction <- prediction_lr
head(DatatestO)


#Optimisation du modèle

m0 <- lm(Score~1,data=trainO)  # choix du modèle avec constante seulement
m0

mf <- lm(Score~.,data=trainO)  # choix du modèle avec constante seulement
mf

step(m0, scope=list(lower=m0, upper=mf),data=trainO, direction="forward")


step(m0, scope=list(lower=m0, upper=mf),data=testO, direction="forward",test="F")


new_modele<-lm(formula = Score ~  age + Medu + Fedu  , data = trainO)
summary(new_modele)

prediction_2<-predict(new_modele,testO)
testO$Prediction <- prediction_2
head(testO)

Score <- as.factor(testO$Score)
pred <- as.factor(round(testO$Prediction,digits = 0))

Score 
pred 
confusionMatrix(pred,Score)
confusionMatrix(factor(pred, levels=1:8), factor(Score, levels=1:8))




library(car)
library(caret)
library(mlbench)
library(MASS)

seed <- 7
set.seed(seed)
fit.knn <- train(Score~ age + Medu + Fedu + studytime + traveltime , data=trainO, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)



#Ici nous choisissons pour commencer d'appliquer une régression linéaire à nos donées afin de voir l'impact de chaque Variable X,
#sur notre variable Y = Score

#On va désormais appliquer une régression linéaire sur nos données test:


DataOModel <- lm(Score ~ ., data = DatatrainO)
plot(DataOModel)

summary(DataOModel)

seed <- 7
set.seed(seed)
#Application d'autre algorithmes'

# kNN
set.seed(seed)
fit.knn <- train(Score~., data=DatatrainO, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)

# CART
set.seed(seed)
fit.cart <- train(Score~., data=DatatrainO, method="rpart", metric=metric, trControl=control)

# Bagged CART
set.seed(seed)
fit.treebag <- train(Score~., data=DatatrainO, method="treebag", metric=metric, trControl=control)


#Comparatif des résultats 

results <- resamples(list( knn=fit.knn, cart=fit.cart ))
# Table comparison
results
summary(results)

bwplot(results)
dotplot(results)

#Tester sur le dataSet de testtrain$Score
DatatestO$Score = as.factor(DatatestO$Score)

TestPredict <- predict(DataOModel, newdata= DatatestO)
summary(TestPredict)


TestPredict<-  as.factor(TestPredict)


Score <- as.factor(DatatestO$Score)
table(TestPredict, Score)
confusionMatrix(TestPredict,DatatestO$Score)

Score
TestPredict













