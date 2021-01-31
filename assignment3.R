
original_data = read.csv('communities.csv')

data = original_data[,1:100]
data = scale(data)
n=dim(data)[1]
S = 1/n*t(data)%*%data

eig = eigen(S)
eig_values = eig$values

variance_percentage = eig_values/sum(eig_values)*100

temp = 0
i=0
while (temp < 0.95){
  temp = sum(variance_percentage[1:i])/100
  i=i+1
}

PCA = princomp(data)
screeplot(PCA)

U = PCA$loadings
U1 = U[,1]
U1_vec = as.vector(U1)






