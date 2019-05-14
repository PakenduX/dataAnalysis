##################################
# TP2                            #
# DEMBELE Mama - DAOUMA Zakaria  #
#################################

library("plot3D")
my_data2 <- read.delim("/home/pakendux/dataAnalysis/data1TP2.txt")
scatter3D(my_data2$Stature, my_data2$Poids, my_data2$Taille, colvar = my_data2$Taille, col = NULL, add = FALSE)

# question 2
moyStature <- mean(my_data2$Stature) 
moyPoids <- mean(my_data2$Poids) 
moyTaille <- mean(my_data2$Taille) 
tabStature <- c()
tabPoids <- c()
tabTaille <- c()

for (i in 1:length( my_data2$Stature)) {
  tabStature[i] <- my_data2$Stature[i]- moyStature
  
}
for (i in 1:length( my_data2$Poids)) {
  tabPoids[i] <- my_data2$Poids[i]- moyPoids
 
}
for (i in 1:length( my_data2$Taille)) {
  tabTaille[i] <- my_data2$Taille[i]- moyTaille
 
}
B<-matrix( c(tabStature, tabPoids, tabTaille), # the data elements 
  nrow=10,              # number of rows 
  ncol=3,              # number of columns 
  byrow = FALSE
)

print(B)
V <- cov(B)
print(V)
#question 3
x = eigen(V)
print("les valeurs propres ")
print(x$values)
print(x$vectors)
#question 4
# l'axe principale est le premier axe des vecteurs propres 
#question 5
C <- B%*%x$vectors
print(C)
princomp(my_data2)$scores
#question 6
scatter3D(x =c (0,-300*x$vectors[1,1]), y =c(0,-300*x$vectors[2,1]), z = c(0,-300*x$vectors[3,1]), add = TRUE, type ="l")
plotrgl()
# question 7
plot(C[,1],C[,2])


