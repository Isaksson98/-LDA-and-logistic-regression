library(datasets)
library(MASS) #LDA
library(mvtnorm)

data(iris)
summary(iris)

dimen =function(x_){
  print(NROW(x_))
  print(NCOL(x_))
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


discriminant =function(x,mean, S, pi_){
    w1 = (-1/2)*t(mean)%*%S[1:2,1:2]%*%mean + log(pi_)
    w2 = S1[1:2,1:2]  %*% mean1
    disc = (x%*% w2) + w1[1,1]
    return (disc)
}
x = matrix(c(iris$Sepal.Length,iris$Sepal.Width),nrow=length(iris$Sepal.Length))

disc1 = discriminant(x, mean1, S1, pi_1)
disc2 = discriminant(x, mean2, S2, pi_2)
disc3 = discriminant(x, mean3, S3, pi_3)

overall_cov = (S1*N1 + S2*N2 + S3*N3)/sum(N1,N2, N3)

#w_1i*x + w_0i = w_1j*x + w_0j i /= j

softmax =function(x){
  num1 = exp(x)
  num2 = sum(exp(x))
  return (num1/num2)
}
a1 = softmax(disc1)
a1
###STEP 3###
#Fit the model
model = lda( iris$Species ~ iris$Sepal.Length + iris$Sepal.Width, iris)
# Make predictions
predictions = predict(model, newdata=iris[,c(1,2,3,4)])$class
# Model accuracy
table(iris[,5],predictions)

plot(model)
