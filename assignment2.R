set.seed(12345)
library(tree)
data = read.csv2('bank-full.csv', header=TRUE, stringsAsFactors = TRUE)
data$duration = NULL
head(data)

#Partion data ####
n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.4)) 
train=data[id,]
id1=setdiff(1:n, id)

set.seed(12345) 
id2=sample(id1, floor(n*0.3)) 
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]












#Fit decision tree to data####
control1 = tree.control(nobs=dim(train)[1], minsize=7000)
control2 = tree.control(nobs=dim(train)[1], mindev=0.0005)

fit_default = tree(formula=y~., data=train)
fit_node_size = tree(formula= y~., data=train, control=control1)
fit_deviance = tree(formula= y~., data=train, control=control2)


predict_default = predict(fit_default, newdata=valid, type='class')

predict_node_size = predict(fit_node_size, newdata=valid, type='class')
predict_deviance = predict(fit_deviance, newdata=valid, type='class')


missclass1 = 1-sum(diag(table(valid$y,predict_default))/sum(table(valid$y,predict_default)))
missclass2 = mean(predict_node_size != valid$y)
missclass3 = mean(predict_deviance != valid$y)
missclass1
missclass2
missclass3

#Find optimal depth####

train_dev = rep(NA, 50)
valid_dev = rep(NA, 50)

for (i in 2:50){
  pruned_tree = prune.tree(fit_deviance, best=i)

  pred = predict(pruned_tree, newdata=valid, type='tree')
  
  train_dev[i]=deviance(pruned_tree)
  valid_dev[i]=deviance(pred)
}

plot(valid_dev, col='red')
points(train_dev, col='blue')
valid_dev
train_dev


loss_matrix = matrix(c(0,5,1,0),nrow=2,ncol=2)

library(rpart)
fit = rpart(y ~ ., data=test, parms = list(loss = loss_matrix))

























dataPure = read.csv('bank-full.csv', sep=';', header=TRUE, colClasses=colClasses)
# TASK 1
# Split data 40/30/30
data = subset(dataPure, select = -duration)
n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.4))
id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n*0.3))
id3 = setdiff(id1,id2)
train = data[id, ]
valid = data[id2,]
test = data[id3,]
# TASK 2
# Fit decision trees to training data
library(tree)
n = dim(train)[1]
fita = tree(y ~ ., data=train)
fitb = tree(y ~ ., data=train, control=tree.control(nobs= n, minsize=7000))
fitc = tree(y ~ ., data=train, control=tree.control(nobs= n, mindev=0.0005))
print("Training data")
summary(fita)
summary(fitb)
summary(fitc)
print("Validation data")
predicta = predict(fita, newdata=valid, type='class')
1-sum(diag(table(valid$y,predicta))/sum(table(valid$y,predicta)))
predictb = predict(fitb, newdata=valid, type='class')
1-sum(diag(table(valid$y,predictb))/sum(table(valid$y,predictb)))
predictc = predict(fitc, newdata=valid, type='class')
1-sum(diag(table(valid$y,predictc))/sum(table(valid$y,predictc)))
# TASK 3
# Finding optimal tree depth
library(ggplot2)
fit = tree(y ~ ., data=train, control=tree.control(nobs= n, mindev=0.0005))
trainScore=rep(0,50)
validScore=rep(0,50)
n_train = dim(train)[1]
n_valid = dim(valid)[1]
for(i in 2:50) {
  prunedTree=prune.tree(fit,best=i)
  20
  pred=predict(prunedTree, newdata=valid, type="tree")
  trainScore[i]=deviance(prunedTree)/n_train
  validScore[i]=deviance(pred)/n_valid
}
data_frame = data.frame(y1=trainScore[2:50], y2=validScore[2:50], x=2:50)
ggplot(data_frame, aes(x=x)) +
  geom_point(aes(y=y1, color='Training score')) +
  geom_point(aes(y=y2, color='Validation score')) + labs(color="") +
  xlab('Number of leaves') + ylab('Deviance')
bestTree=prune.tree(fit,best=22)
summary(bestTree)
Yfit=predict(bestTree, newdata=test, type="class")
print('Task 3 missclass')
table(test$y,Yfit)
1-(sum(diag(table(test$y,Yfit)))/sum(table(test$y,Yfit)))
# TASK 4
library(rpart)
library(rpart.plot)
l = matrix(c(0, 5, 1, 0), ncol = 2)
fit = rpart(y ~ ., data=test, parms = list(loss = l))
pred <- predict(fit, type = "class")
table(test$y, pred)
1-(sum(diag(table(test$y,pred)))/sum(table(test$y,pred)))
# TASK 5
library(e1071)
library(ggplot2)
fit_bayes = naiveBayes(y ~ ., data=train)
y_bayes = predict(fit_bayes, newdata=test, type='raw')[,2]
y_tree = predict(bestTree, test, type='vector')[,2]
tpr_bayes=vector()
fpr_bayes=vector()
tpr_tree=vector()
fpr_tree=vector()
i=1
for (thr in seq(from=0.00, to=1, by=0.05)) {
  y_hat_bayes = ifelse(y_bayes>thr, 'yes', 'no')
  y_hat_tree = ifelse(y_tree>thr, 'yes', 'no')
  conf_m_bayes = table(test$y, y_hat_bayes)
  conf_m_tree = table(test$y, y_hat_tree)
  if (is.na(table(y_hat_tree)[2])) {
    if (colnames(conf_m_tree)[1] == 'yes') {
      conf_m_tree = cbind(c(0,0), conf_m_tree)
    } else {
      conf_m_tree = cbind(conf_m_tree, c(0,0))
    }
  }
  if (is.na(table(y_hat_bayes)[2])) {
    if (colnames(conf_m_bayes)[1] == 'yes') {
      conf_m_bayes = cbind(c(0,0), conf_m_bayes)
    } else {
      conf_m_bayes = cbind(conf_m_bayes, c(0,0))
    }
  }
  tpr_bayes[i] = conf_m_bayes[2,2]/sum(conf_m_bayes[2,])
  fpr_bayes[i] = conf_m_bayes[1,2]/sum(conf_m_bayes[1,])
  tpr_tree[i] = conf_m_tree[2,2]/sum(conf_m_tree[2,])
  fpr_tree[i] = conf_m_tree[1,2]/sum(conf_m_tree[1,])
  i = i + 1
}
df <- data.frame(tpr=c(tpr_tree,tpr_bayes), fpr=c(fpr_tree, fpr_bayes),
                 Method=c(rep(paste("Optimal Tree"), each=length(tpr_tree)),
                          rep(paste("Naïve Bayes"), each=length(tpr_bayes))) )
ggplot(aes(x=fpr, y=tpr, color=Method), data=df) +
  geom_line() + xlim(0,1) + ylim(0,1) +
  xlab('fpr') + ylab('tpr')



