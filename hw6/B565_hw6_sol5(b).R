

# Generated 1st cluster points
X1 = matrix(0, nrow = 50, ncol=2)
C1 = rep(1,50)
t1 = matrix(rnorm(2),ncol = 2, nrow = 2)
b1 = matrix(rnorm(2,0,10),ncol = 1, nrow = 2)
for (i in 1:50){
  z1 = matrix(rnorm(2),ncol = 1, nrow = 2)
  
  X1[i,] = t1%*%z1 + b1
}
X1 = cbind(X1,C1)

# Generated 2nd cluster points
X2 = matrix(0, nrow = 50, ncol=2)
C2 = rep(2,50)
t2 = matrix(rnorm(2),ncol = 2, nrow = 2)
b2 = matrix(rnorm(2,0,10),ncol = 1, nrow = 2)
for (i in 1:50){
  z2 = matrix(rnorm(2),ncol = 1, nrow = 2)
  
  X2[i,] = t2%*%z2 + b2
}

X2 = cbind(X2,C2)


# Generated 3rd cluster points
X3 = matrix(0, nrow = 50, ncol=2)
C3 = rep(3,50)
t3 = matrix(rnorm(2),ncol = 2, nrow = 2)
b3 = matrix(rnorm(2,0,10),ncol = 1, nrow = 2)
for (i in 1:50){
  z3 = matrix(rnorm(2),ncol = 1, nrow = 2)
  
  X3[i,] = t3%*%z3 + b3
}
X3 = cbind(X3,C3)


# Generated 4th cluster points
X4 = matrix(0, nrow = 50, ncol=2)
C4 = rep(4,50)
t4 = matrix(rnorm(2),ncol = 2, nrow = 2)
b4 = matrix(rnorm(2,0,10),ncol = 1, nrow = 2)
for (i in 1:50){
  z4 = matrix(rnorm(2),ncol = 1, nrow = 2)
  
  X4[i,] = t4%*%z4 + b4
}
X4 = cbind(X4,C4)


# Shuffling data for non-biased initialisation of prototype points
X = rbind(X1,X2,X3,X4)
X_rand = X[sample(nrow(X)),]

# Plotting data points of different cluster with different colour
 

# (b)
# Intialising cluster = 4 and sampling 4 centroid points
K = 4
df = cbind(X_rand[,c(1,2)],rep(1,200))
m = df[sample(nrow(df),4),c(1,2)]

# Plotting the scatter plot with inital cluster same for all points
plot(df[,1], df[,2],col=df[,3])
lines(m[,1],m[,2],col="blue",pch=4,cex=3,type="p")

# Looping through 50 iteration with user input of cluster formation
dist_matrix = matrix(0,nrow=200,ncol= 4)



n = 'yes'
for (i in 1:10){
  while (n == "yes") {
    for (j in 1:nrow(m)) {
      dist_matrix[,j] = sqrt((df[,1]-m[j,1])^2+(df[,2]-m[j,2])^2)
    }
    
    cluster = apply(dist_matrix, 1, which.min)
    df[,3] = cluster
    for (k in 1:nrow(m)){
      subset = df[df[,3] == k,]
      m[k,1] = mean(subset[,1])
      m[k,2] = mean(subset[,2])
    }
    
    plot(df[,1], df[,2],col=df[,3])
    lines(m[,1],m[,2],col="blue",pch=4,cex=3,type="p")
    n = as.character(readline(prompt="continue:"));
  }  
}
