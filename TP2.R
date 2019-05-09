#Implémentation algo analyse en composantes principales
library('plot3D')
#1
data = read.table("~/dataAnalysis/data1TP2.txt", header = TRUE)
scatter3D(data$Stature, data$Poids, data$Taille, xlab = "Stature", ylab = "Poids", zlab = "Taille")

#2
meanStature = mean(data$Stature)
meanPoids = mean(data$Poids)
meanTaille = mean(data$Taille)

B = cbind(data$Stature - meanStature, data$Poids - meanPoids, data$Taille - meanTaille)
V = cov(B)
print(V)

#3
vvp = eigen(V)
vectP = vvp$vectors
valP = vvp$values
print(vectP)
print(valP)

#4

#5
C = B %*% vectP
princomp(data)$scores

