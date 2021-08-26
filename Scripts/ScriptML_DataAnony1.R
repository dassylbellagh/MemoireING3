
install.packages("openxlsx")   
# Install openxlsx R package
library("readxl") 

data2<-read_excel("C:/Users/dassy/Desktop/ING3/S2/ADD/Projet ADD/DataAn1.xlsx") 

dim(data2)

colnames(data2)
summary(data2)



#Nous allons Maintenant diviser notre Dataset en 2:
#  -Dataset train
#  -Dataset test

DFdata2 <- data.frame(data2)

dt2 = sort(sample(nrow(DFdata2), nrow(DFdata2)*.8))
nrow(DFdata2)
Datatrain2 <- DFdata2[dt,]
Datatest2 <- DFdata2[-dt,]
nrow(Datatrain2)
nrow(Datatest2)



library(caret)
library(mlbench)


control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# Méthode d'évaluation Cross-Validation
# On crée 10 blocs et on fait de la cross-validation dessus 3 x de suites

metric <- "Accuracy"

preProcess = c("center", "scale")


Datatrain2$Success = as.factor(Datatrain2$Success)



seed <- 7
set.seed(seed)
fit.glm <- train(Success~ age + Medu + Fedu + freetime , data=Datatrain2, method="glm", metric=metric, trControl=control)

# kNN
set.seed(seed)
fit.knn <- train(Success~ age + Medu + Fedu + freetime , data=Datatrain2, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)

# CART
set.seed(seed)
fit.cart <- train(Success~ age + Medu + Fedu + freetime , data=Datatrain2, method="rpart", metric=metric, trControl=control)

# Bagged CART
set.seed(seed)
fit.treebag<- train(Success~ age + Medu + Fedu + freetime , data=Datatrain2, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(Success~ age + Medu + Fedu + freetime , data=Datatrain2, method="rf", metric=metric, trControl=control)




results2 <- resamples(list(logistic=fit.glm,knn=fit.knn,cart=fit.cart, bagging=fit.treebag, rf=fit.rf))

results2
summary(results2)




bwplot(results)
dotplot(results)



Datatest2$Success = as.factor(Datatest2$Success)

TestPredict2 <- predict(fit.knn, newdata= Datatest2)
summary(TestPredict2)

confusionMatrix(TestPredict2, Datatest2$Success)










