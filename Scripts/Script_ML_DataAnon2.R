
install.packages("openxlsx")   
# Install openxlsx R package
library("readxl") 

data3<-read_excel("C:/Users/dassy/Desktop/ING3/S2/ADD/Projet ADD/DataAn2.xlsx") 

dim(data3)

colnames(data3)
summary(data3)



#Nous allons Maintenant diviser notre Dataset en 2:
#  -Dataset train
#  -Dataset test

DFdata3 <- data.frame(data3)
dt3 = sort(sample(nrow(DFdata3), nrow(DFdata3)*.8))
nrow(DFdata3)
Datatrain3 <- DFdata3[dt,]
Datatest3 <- DFdata3[-dt,]
nrow(Datatrain3)
nrow(Datatest3)



library(caret)
library(mlbench)


control3 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# Méthode d'évaluation Cross-Validation
# On crée 10 blocs et on fait de la cross-validation dessus 3 x de suites

metric3 <- "Accuracy"

preProcess = c("center", "scale")


Datatrain3$Success = as.factor(Datatrain3$Success)
#On change la collonne status en factor

seed <- 7
set.seed(seed)
fit.glm <- train(Success~ age + Medu + Fedu + freetime, data=Datatrain3, method="glm", metric=metric, trControl=control3)

# kNN
set.seed(seed)
fit.knn <- train(Success~ age + Medu + Fedu + freetime, data=Datatrain3, method="knn", metric=metric3, preProc=c("center", "scale"), trControl=control3)

# CART
set.seed(seed)
fit.cart <- train(Success~ age + Medu + Fedu + freetime, data=Datatrain3, method="rpart", metric=metric3, trControl=control3)

# Bagged CART
set.seed(seed)
fit.treebag <- train(Success~ age + Medu + Fedu + freetime, data=Datatrain3, method="treebag", metric=metric3, trControl=control3)
# Random Forest
set.seed(seed)
fit.rf <- train(Success~ age + Medu + Fedu + freetime, data=Datatrain3, method="rf", metric=metric3, trControl=control3)




results <- resamples(list(logistic=fit.glm,knn=fit.knn,cart=fit.cart, bagging=fit.treebag, rf=fit.rf))

results
summary(results)





bwplot(results)
dotplot(results)



Datatest3$Success = as.factor(Datatest3$Success)

TestPredict3 <- predict(fit.cart3, newdata= Datatest3)
summary(TestPredict3)

confusionMatrix(TestPredict3, Datatest3$Success)










