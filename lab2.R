library(datasets)
library(MASS) #LDA
library(nnet) 
library(mvtnorm)
library(caret)


data(iris)
summary(iris)
set.seed(12345)

dimen =function(x_){
  print(NROW(x_))
  print(NCOL(x_))
}

softmax =function(x){
  num1 = exp(x)
  num2 = sum(exp(x))
  return (num1/num2)
}


plot(iris$Sepal.Length, iris$Sepal.Width,col = iris$Species, main="Iris Data")
#legend(5,5,unique(iris$Species),col=1:length(iris$Species),pch=1)
#Setosa will be easy to classify using LDA since they are clearly 
#seperated. Versicolor and viginica have no clear border between 
#them and LDA will be harder.

mean1_len = mean(iris$Sepal.Length[iris$Species=='setosa'])
mean2_len = mean(iris$Sepal.Length[iris$Species=='versicolor'])
mean3_len = mean(iris$Sepal.Length[iris$Species=='virginica'])

mean1_wid = mean(iris$Sepal.Width[iris$Species=='setosa'])
mean2_wid = mean(iris$Sepal.Width[iris$Species=='versicolor'])
mean3_wid = mean(iris$Sepal.Width[iris$Species=='virginica'])

mean1 <- c(mean1_len, mean1_wid)
mean2 <- c(mean2_len, mean2_wid)
mean3 <- c(mean3_len, mean3_wid)

N1 = length(iris$Species[iris$Species=='setosa'])
N2 = length(iris$Species[iris$Species=='versicolor'])
N3 = length(iris$Species[iris$Species=='virginica'])
pi_1 = N1 / length(iris$Species)
pi_2 = N2 / length(iris$Species)
pi_3 = N3 / length(iris$Species)

#Covariance matricies
(cov(subset(iris,subset=Species=='setosa',select=-Species)) -> S1)
(cov(subset(iris,subset=Species=='versicolor',select=-Species)) -> S2)
(cov(subset(iris,subset=Species=='virginica',select=-Species)) -> S3)
#cov(x = [x1, x2]) 

x = matrix(c(iris$Sepal.Length,iris$Sepal.Width),nrow=length(iris$Sepal.Length))

overall_cov = ginv((S1[1:2,1:2]*N1 + S2[1:2,1:2]*N2 + S3[1:2,1:2]*N3)/sum(N1,N2, N3))

w01 = (-1/2)*t(mean1)%*%overall_cov%*%mean1 + log(pi_1)
w1 = overall_cov  %*% mean1
disc1 = (x%*% w1) + w01[1,1]

w02 = (-1/2)*t(mean2)%*%overall_cov%*%mean2 + log(pi_2)
w2 = overall_cov  %*% mean2
disc2 = (x%*% w2) + w02[1,1]

w03 = (-1/2)*t(mean3)%*%overall_cov%*%mean3 + log(pi_3)
w3 = overall_cov  %*% mean3
disc3 = (x%*% w3) + w03[1,1]

prediction =function(disc1_, disc2_, disc3_){
  
  #disc <- cbind(softmax(disc1_), softmax(disc2_), softmax(disc3_))
  disc <- cbind(disc1_, disc2_, disc3_)
  num2 = cbind(1:nrow(disc), max.col(disc, 'first'))

  return (num2[,2])
    
}
num2 = prediction(disc1, disc2, disc3)

#, xlim=c(0,10), ylim=c(0,10)

plot(iris$Sepal.Length, iris$Sepal.Width, col = num2,
     main="Iris Data prediction")
table(iris[,5],num2)

#Solve: w3*x+w03 = w1*x+w01

k_ = w1-w2
k=k_[1]/k_[2]
m = (w01[1,1]-w02[1,1])/k_[2]
abline(m, k)

k_ = w1-w3
k=k_[1]/k_[2]
m = (w01[1,1]-w03[1,1])/k_[2]
abline(m, k)

k_ = w3-w2
k=k_[1]/k_[2]
m = (w03[1,1]-w02[1,1])/k_[2]
abline(m, k)

#Fit the model
model = lda( iris$Species ~ iris$Sepal.Length + iris$Sepal.Width, iris)

# Make predictions
predictions = predict(model, newdata=iris[,c(1,2,3,4)])$class
# Model accuracy
table(iris[,5],predictions)

#Q4

num5 = cbind(rmvnorm(50, mean = mean1, sigma = S1[1:2,1:2]), 1)
num6 = cbind(rmvnorm(50, mean = mean2, sigma = S2[1:2,1:2]), 2)
num7 = cbind(rmvnorm(50, mean = mean3, sigma = S3[1:2,1:2]), 3)
new_data = rbind(num5, num6, num7)
plot( new_data[,1], new_data[,2],
      col = new_data[,3],
      xlab="Sepal Length", ylab="Sepal Width",
      main="Generated data")

#Q5
model_nnet = multinom(iris$Species ~ iris$Sepal.Length + iris$Sepal.Width,
                      iris)

nnet_predictions <- predict(model_nnet,iris,type = "class")
table(iris$Species,nnet_predictions)

cfm1 <- confusionMatrix(iris$Species, nnet_predictions)
cfm2 <- confusionMatrix(iris$Species, predictions)

plot(iris$Sepal.Length, iris$Sepal.Width,col = nnet_predictions, main="NNET")



