##########################################
#    TP3 : 
#   -Génération de nuage de points
#   -Classification non supervisée
	Mama DEMBELE - Zakaria DAOUMA
#########################################
#generation de x, y uniformes sur [0,1]
x1 = runif(100,0,1)
y1 = runif(100,0,1)


x2 = rnorm(100,4,sqrt(1))
y2 = rnorm(100,0,sqrt(1))

x3 = rnorm(100,0.5,sqrt(2))
y3 = rnorm(100,6,sqrt(2))

m1 <- cbind(x1,y1)
m2 <- cbind(x2,y2)
m3 <- cbind(x3,y3)

m <- rbind(m1,m2,m3)


par(mfrow=c(2,2))

plot(-5:10, -5:10, type = "n" , main="nuage ")
points(x1,y1, col="skyblue", pch=19)
points(x2,y2, col="green", pch=19)
points(x3,y3, col="red", pch=19)



### Classification NS """

class_asc <- function(data, t) {
  count = 1
  n = nrow(data)
  mat = diag(n)
  dist = matrix(numeric(0), n, n)
  
  #calcul des distances
  for (m in 1:n) {
    for (h in 1:n) {
      dist[m, h] = sqrt( (data[m, 1] - data[h, 1])** 2 + (data[m, 2] - data[h, 2])**2 )
    }
  }
  
  while (count <= (n - t) ) {
    dmin = dist[2, 1]
    pi = 2
    pj = 1
    
    # minimum
    for (i in 2:(n - count + 1)) {
      for (j in 1:(i - 1)) {
        if (dist[i, j] < dmin) {
          dmin = dist[i, j]
          p_i = i
          p_j = j
        }
      }
    }
    
    mat[pj, ] = mat[pj, ] + mat[pi, ]
    
    data_tmp = data[mat[pj,]==1,]
    g_tmp = c( mean(data_tmp[1]), mean(data_tmp[2]))
    
    for (i in 1:(n - count + 1)) {
      datatmp = data[mat[i,]==1,]
      gitmp = c( mean(datatmp[1]), mean(datatmp[2]) )
      dist_ = sqrt( (g_tmp[1]-gitmp[1])**2 + (g_tmp[2]-gitmp[2])**2 )
      if (i<p_j){
        dist[p_j,i]=dist_
      }
      else {
        dist[i,p_j]=dist_
      }
    }
    mat=mat[-p_i,]
    dist=dist[-p_i,]
    dist=dist[,-p_i]
    count = count + 1
  }
  return ( t(mat) )
}

C = class_asc(m,3)

z = matrix(0,300,1)

for (i in 1:nrow(m)){
  if(C[i,1]==1){
    z[i]=1
  }
  if(C[i,2]==1){
    z[i]=2
  }
  if(C[i,3]==1){
    z[i]=3
  }
  
}

plot(m, col = c("red", "skyblue", "green")[z], pch=19, main="nuage  classes")



D <- dist(m, method = "euclidean")

Asc_ierarchique <- hclust(D, method = "complete") #complete ou ward

# plot(AscHierarchique, cex = 0.6, hang = -1)

cluster = cutree(Asc_ierarchique, 3)


#qusetion 3

fac1 <- factor(cluster)

# le calcul du barycentre  :
barycentre <- function(dfxy, fac){
  wt = rep(1, length(fac))
  f1 <- function(cl) {
    n <- length(cl)
    cl <- as.factor(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1:n) + n * (unclass(cl) - 1)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    data.frame(x)
  }
  dfxy <- data.frame(dfxy)
  distri <- f1(fac) * wt
  w1 <- unlist(lapply(distri, sum))
  dfdistri <- t(t(distri)/w1)
  coo2 <- t(distri) %*% as.matrix(dfxy)
  rownames(coo2) <- levels(fac)
  coo2
}

ba <- barycentre(m, fac1)


points(ba1, pch = 19, col = "darkgreen")
plot(m, col = c("red", "skyblue", "green")[cluster], pch=19,main="test")
points(ba1, pch = 19, col = "darkgreen")
db1 <- (t(ba)-colMeans(m))^2
db1 <- colSums(db1)
db1 <- db1*table(fac1)
iner.int <- sum(db1)/sum((t(m)-colMeans(m))^2)
iner.intra <- 1-iner.int
  
