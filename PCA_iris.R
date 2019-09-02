#DATA
data <- iris
str(data)
summary(data)
#---------------
set.seed(111)
#Spliting the data into test & train
ind <- sample(2,nrow(iris),replace=T,prob = c(0.8,0.2))
training <- data[ind==1,]
test <- data[ind==2,]
#Scatter plot and co-relation
library(psych)
pairs.panels(training[,-5],
             gap=0.5,
             bg=c("red","blue","green")[training$Species],
             pch=21)
#We see that the corelation between Petal.Length and Petal.Width
#Visualizing in 4D is difficult so we use PCA to change it to 2D
pc <- prcomp(training[,-5],
             center = T,
             scale. = T)
#Centre to make sure mean is zero and Scale is so that data is normalized
#Checking the attributes of pc
attributes(pc)
#Center means the avg of each 4 varriable
pc$center
#Lets see PC of Varriables
print(pc)
summary(pc)
#Orthology of PCA
pairs.panels(pc$x,
             gap=0.5,
             bg=c("yellow","blue","green")[data$Species],
             pch=21)
#BI-PLOT
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = training$Species,
              ellipse = T,
              circle = T,
              ellipse.prob = 0.68)
g <- g +scale_color_discrete(name="")
g <- g + theme(legend.direction = "horizontal",
               legend.position = "top")
print(g)
#Prediction
pred <-predict(pc,training)
#all values converted in PCs
pred
pred <- data.frame(pred,training$Species)
head(pred)
#We have PCs with label
tst <- predict(pc,test)
tst <- data.frame(tst,test$Species)
tst
#Multinomial Logistic Regression with top 2PC
library(nnet)
pred$training.Species <- relevel(pred$training.Species,ref = "setosa")
my_model <- multinom(training.Species~.,data=pred)
summary(my_model)
#Summary provides interceps with coef.
#Confusion Matrix and Misclassification error in training data
p <- predict(my_model,pred)
tab <- table(p,pred$training.Species)
tab
#For test data
t <- predict(my_model,tst)
con <- table(t,tst$test.Species)
con
