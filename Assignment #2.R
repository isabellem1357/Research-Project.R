#Assignment 2

#2.9
A = matrix(c(1, 6, 11, 2, 7, 12, 3, 8, 13, 4, 9, 14, 5, 10, 15), nrow = 3, byrow = FALSE)
A
B = svd(A)
B

#2.10
round(svd(A)$d[1]*svd(A)$u[,1]%*%t(svd(A)$v[,1]))

#2.11
C = matrix(c(1.2, -0.5, 0.9, -0.6, 1.0, -0.7, -0.4, 0.9, -0.2, 1.1, 1.6, -0.4), nrow = 3, byrow = TRUE)
C
Ct = t(C)
CCt = C%*%Ct
eigCCt = eigen(CCt)

CtC = Ct%*%C
eigCtC = eigen(CtC)

svdC = svd(C)
svdC

eigCCt$values
eigCCt$vectors
eigCtC$values
eigCtC$vectors

svdC$d
svdC$u
svdC$v

U <- svdC$u
V <- svdC$v

for (i in 1:ncol(U)) {
  for (j in 1:ncol(U)) {
    if (i != j) {
      dot_prod <- sum(U[, i] * U[, j])
      cat("Dot product of U", i, "and U", j, ":", round(dot_prod), "\n")
    } else {
      norm_val <- sqrt(sum(U[, i] * U[, i]))
      cat("Norm of U", i, ":", norm_val, "\n")
    }
  }
}

for (i in 1:ncol(V)) {
  for (j in 1:ncol(V)) {
    if (i != j) {
      dot_prod <- sum(V[, i] * V[, j])
      cat("Dot product of V", i, "and V", j, ":", round(dot_prod), "\n")
    } else {
      norm_val <- sqrt(sum(V[, i] * V[, i]))
      cat("Norm of V", i, ":", norm_val, "\n")
    }
  }
}


#2.12
SLPD = read.table("SLPdarwin2023SDAnom+.txt", header = FALSE, sep = "")
SLPD
SLPT = read.table("SLPtahiti2023SDAnom.txt", header = FALSE, sep = "")
SLPT
tv1 = SLPT[12:61,13]
tv1 = c(as.numeric(tv1))
dv1 = SLPD[12:61, 13]
dv1 = c(as.numeric(dv1))
tvd = matrix(c(tv1, dv1), nrow = 2, byrow = TRUE)
colnames(tvd) = c(1961:2010)
rownames(tvd) = c("Tahiti", "Darwin")
tvd


svd(tvd)



V = svd(tvd)$v
time = 1961:2010
plot(time, V[,1], type = "l", xlab = "Time (Year)", ylab = "PC1 - First Singular Vector in V",
     main = "Time Series Curve of First Principal Component")


#2.13
plot(time, V[,2], type = "l", xlab = "Time (Year)", ylab = "PC2 - Second Singular Vector in V",
     main = "Time Series Curve of Second Principal Component")


#2.14
tv1 = SLPT[12:61,2]
tv1 = c(as.numeric(tv1))
dv1 = SLPD[12:61, 2]
dv1 = c(as.numeric(dv1))
tvd = matrix(c(tv1, dv1), nrow = 2, byrow = TRUE)
colnames(tvd) = c(1961:2010)
rownames(tvd) = c("Tahiti", "Darwin")
tvd
svd(tvd)

V1 = svd(tvd)$v
time = 1961:2010

plot(time, V1[,1], type = "l", xlab = "Time (Year)", ylab = "PC1 - First Singular Vector in V",
     main = "Time Series Curve of First Principal Component")

plot(time, V1[,2], type = "l", xlab = "Time (Year)", ylab = "PC2 - Second Singular Vector in V",
     main = "Time Series Curve of Second Principal Component")



















