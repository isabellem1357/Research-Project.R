#Interactive Linear Algebra Book
#By Sam Shen
#August 2023

#Chapter 1

#Create your working directory named LinAlg and go there
setwd('/Users/sshen/LinAlg') 
getwd() #Verify that you are in the directory/folder
#[1] "/Users/sshen/LinAlg"

#Matrix subtraction
matrix(c(1,1,1,-1), nrow=2) - matrix(c(1,3,2,0), nrow=2) 
# [,1] [,2]
#[1,] 0 -1
#[2,] -2 -1


#Dot product
#install.packages('geometry')
library(geometry)
a = c(1, 1)
b = c(1, 0)
# Calculating dot product using dot()
dot(a, b)

#Another way to compute dot product is to 
a%*%b
#[,1]
#[1,]    1

#Calculate the angle between two vectors
am = norm(a, type="2")  
am
#[1] 1.414214
bm = norm(b, type="2") 
bm
#[1] 1
angleab = acos(dot(a, b)/(am*bm))*180/pi
angleab
#[1] 45 degrees

#Scaler times a matrix
A = matrix(c(1,1,1,-1), ncol = 2, byrow=T)
A
#[,1] [,2]
#[1,]    1    1
#[2,]    1   -1
3*A
#[,1] [,2]
#[1,]    3    3
#[2,]    3   -3

#Matrix multiplication by command %*%
A = matrix(c(1,1,1,-1), nrow=2) 
B = matrix(c(1,2,3,4), nrow=2)
A%*%B
#     [,1] [,2]
#[1,]    3    7
#[2,]   -1   -1
B%*%A
#     [,1] [,2]
#[1,]    4   -2
#[2,]    6   -2

#Matrix transpose
A = matrix(c(1,2,3,4), ncol =2, byrow=T)
A
#     [,1] [,2]
#[1,]    1    2
#[2,]    3    4
t(A)
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#Generate a diagonal matrix
D = diag(c(2, 1, -3))
round(D, digits = 0)
#     [,1] [,2] [,3]
#[1,]    2    0    0
#[2,]    0    1    0
#[3,]    0    0   -3

#Generate an 3-dimensional identity matrix
I = diag(3)
I
#     [,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1

#Generate a 2-by-3 zero matrix
M = matrix(0, 2, 3)
M
#     [,1] [,2] [,3]
#[1,]    0    0    0
#[2,]    0    0    0

#Compute the inverse of a matrix
A = matrix(c(1,1,1,-1), nrow=2)
invA = solve(A)
invA
#     [,1] [,2]
#[1,]  0.5  0.5
#[2,]  0.5 -0.5

#Verify the inverse
invA %*% A
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1


#Delete rows or/and columns
A = matrix(1:9, nrow = 3)
A
#     [,1] [,2] [,3]
#[1,]    1    4    7
#[2,]    2    5    8
#[3,]    3    6    9

A[-3,]
#     [,1] [,2] [,3]
#[1,]    1    4    7
#[2,]    2    5    8

A[,-1]
#     [,1] [,2]
#[1,]    4    7
#[2,]    5    8
#[3,]    6    9
A[-3,-1]
#     [,1] [,2]
#[1,]    4    7
#[2,]    5    8

A[1:2, 2:3] #Sub-matrix
#     [,1] [,2]
#[1,]    4    7
#[2,]    5    8

A[1:2, ] #keep all the columns
#     [,1] [,2] [,3]
#[1,]    1    4    7
#[2,]    2    5    8

#Insert a row or column to a matrix
A = matrix(1:4, nrow = 2)
br = 5:6
bc = 7:8
rbind(A, br)
#   [,1] [,2]
#1    3
#2    4
#br    5    6

rbind(br, A)
#   [,1] [,2]
#br    5    6
#      1    3
#      2    4

rbind(rbind(A[1,], br), A[2,])
#   [,1] [,2]
#      1    3
#br    5    6
#      2    4
Abc = cbind(A, bc)
Abc
Abc = cbind(A, bc)
#         bc
#[1,] 1 3  7
#[2,] 2 4  8

cbind(Abc, A)#stack two matrices
#         bc    
#[1,] 1 3  7 1 3
#[2,] 2 4  8 2 4


#
#
#Row or column statistics
A = matrix(1:6, nrow = 2)
rowSums(A)
#[1]  9 12
rowMeans(A)
#[1] 3 4
rowCumsums(A) #cumulative sum
#     [,1] [,2] [,3]
#[1,]    1    4    9
#[2,]    2    6   12
colMeans(A)
#[1] 1.5 3.5 5.5
library(matrixStats) #SD needs the library
rowSds(A) 
#[1] 2 2
colSds(A)
#[1] 0.7071068 0.7071068 0.7071068

#Sweep a matrix by a vector using subtraction
A = matrix(1:6, nrow=2, byrow = TRUE)
A
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    4    5    6
u = 1:3
Br = sweep(A, 2, u) #2 means sweep every row
Br
#     [,1] [,2] [,3]
#[1,]    0    0    0
#[2,]    3    3    3
v= 1:2
Bc = sweep(A, 1, v) #1 means sweep every column
Bc
#     [,1] [,2] [,3]
#[1,]    0    1    2
#[2,]    2    3    4

c = colMeans(A) #means of each column
sweep(A, 2, c) #anomaly data matrix
#[1,] -1.5 -1.5 -1.5

#[2,]  1.5  1.5  1.5

sin(A)#function operation on each matrix element
#           [,1]       [,2]       [,3]
#[1,]  0.8414710  0.9092974  0.1411200
#[2,] -0.7568025 -0.9589243 -0.2794155

A^2 #not equal to A%*%A
#     [,1] [,2] [,3]
#[1,]    1    4    9
#[2,]   16   25   36

#Sweep a matrix by a vector using multiplication
A = matrix(1:6, nrow=2, byrow = TRUE)
w = 1:2
w*A
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    8   10   12
A*w # yields the same result as w*A
w3 = 1:3
#sweep each row by transposing A
t(w3*t(A))
#     [,1] [,2] [,3]
#[1,]    1    4    9
#[2,]    4   10   18
w3*A #multiplication sweep by row-dimensions not matching
#     [,1] [,2] [,3]
#[1,]    1    6    6
#[2,]    8    5   18

A/w #sweeping by division
#     [,1] [,2] [,3]
#[1,]    1  2.0    3
#[2,]    2  2.5    3


#Conversions between a Vector and a Matrix
v = c(60, 58, 67, 70, 55, 53)
M = matrix(v, nrow = 2) #from vector to matrix
M
#     [,1] [,2] [,3]
#[1,]   60   67   55
#[2,]   58   70   53
c(M) #from matrix to vector by column
#[1] 60 58 67 70 55 53
c(t(M)) #from matrix to vector by row
#[1] 60 67 55 58 70 53

#Reduce the dimension of an nD array
x <- array(1:(2*3*4), dim=c(2,3,4))
dim(x)
#[1] 2 3 4
x #a stack of four 2-by-3 matrices
#, , 1 #the first of the 3rd dim index

#[,1] [,2] [,3]
#[1,]    1    3    5
#[2,]    2    4    6
# ...
#install.packages('R.utils')
library(R.utils)
#flat all the other dim except the 3rd one
#flat the 1st and 2nd dim
y <- wrap(x, map=list(3, NA)) 
dim(y)
#[1] 4 6
y
#     [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]    1    2    3    4    5    6
#[2,]    7    8    9   10   11   12
#[3,]   13   14   15   16   17   18
#[4,]   19   20   21   22   23   24

#back to the original 3D array
array(t(y), dim = c(2,3,4))


#Solve linear equations
A = matrix(c(1,1,1,-1), nrow = 2)
b = c(20,4)
solve(A, b)
#[1] 12  8  #This is the result x1=12, and x2=8.

#Spatial covariance matrix
dat = matrix(c(0,-1,1,2,3,4), nrow=3)
dat
colMeans(dat)
A = sweep(dat, 2, colMeans(dat))
A
#     [,1] [,2]
#[1,]    0   -1
#[2,]   -1    0
#[3,]    1    1
covm=(1/(dim(A)[2]))*A%*%t(A)
covm #is the covariance matrix.
#     [,1] [,2] [,3]
#[1,]  0.5  0.0 -0.5
#[2,]  0.0  0.5 -0.5
#[3,] -0.5 -0.5  1.0

u = c(1, 1, 0)
v = covm %*% u
v
#     [,1]
#[1,]  0.5
#[2,]  0.5
#[3,] -1.0
#u and v are in different directions

#Eigenvectors of a covariance matrix 
ew = eigen(covm)
ew
#$values
#[1] 1.500000e+00 5.000000e-01 1.332268e-15

#$vectors
#           [,1]          [,2]      [,3]
#[1,] -0.4082483 -7.071068e-01 0.5773503
#[2,] -0.4082483  7.071068e-01 0.5773503
#[3,]  0.8164966  8.881784e-16 0.5773503
#This is the first eigenvector

#Verify the eigenvectors and eigenvalues
covm%*%ew$vectors[,1]/ew$values[1]
#           [,1]
#[1,] -0.4082483
#[2,] -0.4082483
#[3,]  0.8164966

w = ew$vectors[,1] # is an eigenvector

#R code for SVD
#Develop a 2-by-3 space-time data matrix for SVD
A=matrix(c(1,-1,2,0,3,1),nrow=2)
A
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]   -1    0    1
#Perform SVD calculation
msvd=svd(A)
msvd
msvd$d
#[1] 3.784779 1.294390
msvd$u
#          [,1]       [,2]
#[1,] -0.9870875 -0.1601822
#[2,] -0.1601822  0.9870875
msvd$v
#           [,1]       [,2]
#[1,] -0.2184817 -0.8863403
#[2,] -0.5216090 -0.2475023
#[3,] -0.8247362  0.3913356
#One can verify that A=UDV', where V' is transpose of V.
verim=msvd$u%*%diag(msvd$d)%*%t(msvd$v)
verim
#     [,1]         [,2] [,3]
#[1,]    1 2.000000e+00    3
#[2,]   -1 1.665335e-16    1
round(verim)
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]   -1    0    1
#This is the original data matrix A

covm = (1/(dim(A)[2]))*A%*%t(A)
eigcov = eigen(covm)
eigcov$values
#[1] 4.7748518 0.5584816
eigcov$vectors
#           [,1]       [,2]
#[1,] -0.9870875  0.1601822
#[2,] -0.1601822 -0.9870875

((msvd$d)^2)/(dim(A)[2])
#[1] 4.7748518 0.5584816
eigcov$values
#[1] 4.7748518 0.5584816

x1=c(1,2,3) #Given the coordinates of the 3 points
x2=c(2,1,3)
y=c(-1,2,1)
df=data.frame(x1,x2,y) #Put data into the data.frame format
fit <- lm(y ~ x1 + x2, data=df)
fit#Show the regression results
#Call:
#  lm(formula = y ~ x1 + x2, data = df)
#Coefficients:
#  (Intercept)           x1           x2  
#-5.128e-16    1.667e+00   -1.333e+00  

1.667*x1-1.333*x2  #Verify that 3 points determining a plane
#[1] -0.999  2.001  1.002


#Multilinear regression
u=c(1,2,3,1)
v=c(2,4,3,-1)
w=c(1,-2,3,4)
mydata=data.frame(u,v,w)
myfit <- lm(w ~ u + v, data=mydata)
summary(myfit)#Show the result
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)   1.0000     1.8708   0.535    0.687
#u             2.0000     1.2472   1.604    0.355
#v            -1.5000     0.5528  -2.714    0.225

#Multilinear regression example for more data
dat=matrix(rnorm(40),nrow=10, dimnames=list(c(letters[1:10]), c(LETTERS[23:26])))
fdat=data.frame(dat)
fit=lm(Z~ W + X + Y, data=fdat)
summary(fit)

#Coefficients\index{linear regression!coefficients}
#            Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  0.36680    0.16529   2.219   0.0683 
#W            0.11977    0.20782   0.576   0.5853  
#X           -0.53277    0.19378  -2.749   0.0333 
#Y           -0.04389    0.14601  -0.301   0.7739


#Chapter 2

#Create your working directory named LinAlg and go there
setwd('/Users/sshen/LinAlg') 
getwd() #Verify that you are in the directory/folder
#[1] "/Users/sshen/LinAlg"

#R code: Computational examples of matrices
A = matrix(c(1,0,0,4,3, 2), nrow = 3, byrow = TRUE)
B = matrix(c(0,1,-1,2), nrow = 2) #form a matrix by columns
C = A%*%B #matrix multiplication
C
#[1,]    0   -1
#[2,]    4    8
#[3,]    2    1
t(C) # transpose matrix of C
#[1,]    0    4    2
#[2,]   -1    8    1

A = matrix(c(1, -1, 1, 2), nrow =2, byrow = TRUE)
solve(A) #compute the inverse of A
#[1,]  0.6666667 0.3333333
#[2,] -0.3333333 0.3333333
A%*%solve(A) #verify the inverse of A
#[1,] 1.000000e+00    0
#[2,] 1.110223e-16    1

#Solve linear equations
A = matrix(c(30, 40, 1, 1), nrow =2, byrow = TRUE)
b = c(1000, 30)
solve(A,b)
#[1] 20 10
solve(A)%*%b #Another way to solve the equations
det(A) #compute the determinant
#[1] -10

library(Matrix)
rankMatrix(A) #Find the rank of a matrix
#[1] 2 #rank(A) = 2

#Orthogonal matrices
p = sqrt(2)/2
Q = matrix(c(p,-p,p,p), nrow=2) 
Q #is an orthogonal matrix 
#           [,1]      [,2]
#[1,]  0.7071068 0.7071068
#[2,] -0.7071068 0.7071068
Q%*%t(Q) #verify O as an orthogonal matrix
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
det(Q) #The determinant of an orthogonal matrix is 1 or -1
#[1] 1

#R code for eigenvectors and eigenvalues
A = matrix(c(1, 2, 2, 1), nrow=2)
eigen(A)
#$values
#[1]  3 -1
#$vectors
#[,1]       [,2]
#[1,] 0.7071068 -0.7071068
#[2,] 0.7071068  0.7071068

#R plot eigenvector v vs a non-eigenvector u
setwd('/Users/sshen/LinAlg')
setEPS() #Plot the figure and save the file
postscript("fig0502.eps", width = 6)
par(mar=c(4.5,4.5,2.0,0.5))
plot(9,9,
     main = 'An eigenvector vs a non-eigenvector',
     cex.axis = 1.4, cex.lab = 1.4,
     xlim = c(0,3), ylim=c(0,3),
     xlab = bquote(x[1]), ylab = bquote(x[2]))
arrows(0,0, 1,0, length = 0.25, 
       angle = 8, lwd = 5, col = 'blue')
arrows(0,0, 1,2, length = 0.3, 
       angle = 8, lwd = 2, col = 'blue',  lty = 3)
arrows(0,0, 1,1, length = 0.25, 
       angle = 8, lwd = 5, col='red') 
arrows(0,0, 3,3, length = 0.3, 
       angle = 8, lwd = 2, col='red', lty = 3)
text(1.4,0.1, 'Non-eigenvector u', cex =1.4, col = 'blue')
text(1.0,2.1, 'Au', cex =1.4, col = 'blue')
text(1.5,0.9, 'Eigenvector v', cex =1.4, col = 'red')
text(2.8, 2.95, 'Av', cex =1.4, col = 'red')
dev.off()

# Verify diagonalization and decomposition: R code
C = matrix(c(2,1,1,2), nrow = 2)
eigen(C)
#$values
#[1] 3 1
#$vectors
#  [,1]       [,2]
#[1,] 0.7071068 -0.7071068
#[2,] 0.7071068  0.7071068
Q = eigen(C)$vectors
D = t(Q)%*%C%*%Q #Matrix diagonalization
D
#[1,]    3    0
#[2,]    0    1
Q%*%D%*%t(Q) #Matrix decomposition
#[1,]    2    1
#[2,]    1    2
D[1,1]*Q[,1]%*%t(Q[,1]) + D[2,2]*Q[,2]%*%t(Q[,2])
#[1,]    2    1
#[2,]    1    2

#Hadamard product of two matrices
#install.packages('matrixcalc')
library(matrixcalc)
A = matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
B = matrix( c( 2, 4, 6, 8 ), nrow=2, byrow=TRUE )
hadamard.prod(A, B)
#     [,1] [,2]
#[1,]    2    8
#[2,]   18   32

#Jordan product of A and B
A = matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
B = matrix( c( 2, 1, 2, 1 ), nrow=2, byrow=TRUE )
(A%*%B + B%*%A)/2
#     [,1] [,2]
#[1,]  5.5  5.5
#[2,]  9.5  7.5

#R commutator of A and B
A = matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
B = matrix( c( 2, 1, 2, 1 ), nrow=2, byrow=TRUE )
A%*%B - B%*%A
#     [,1] [,2]
#[1,]    1   -5
#[2,]    9   -1
#install.packages('psych')
library(psych)
tr(A%*%B - B%*%A) #tr for trace
#[1] 0

#Cross product
library(pracma)
x = 1:3
y = 4:6
cross(x, y)	
#[1] -3  6 -3
dot(x, y)
#[1] 32

#Outer product of two vectors
a = 1:2
b = 1:4
a%o%b #outer product a_2-by-1 times t(b)_1-by-4
#     [,1] [,2] [,3] [,4]
#[1,]    1    2    3    4
#[2,]    2    4    6    8


#Outer product of A_mn and B_nq
A = matrix(1:4, ncol = 2)
B = matrix(1:6, ncol = 3)
A%o%B
dim(A%o%B)
#[1] 2 2 2 3

#Outer product of A_mn and B_pq
A = matrix(1:4, ncol = 2)
B = matrix(1:9, ncol = 3)
A%o%B
dim(A%o%B)
#[1] 2 2 2 3

#Kronecker product
library(fastmatrix)
A <- diag(1:2)
B <- matrix(1:4, ncol = 2)
kronecker.prod(A, B)
#     [,1] [,2] [,3] [,4]
#[1,]    1    3    0    0
#[2,]    2    4    0    0
#[3,]    0    0    2    6
#[4,]    0    0    4    8

# an example with vectors
ones <- rep(1, 2)
y <- 1:4
kronecker.prod(ones, t(y)) # 2-by-4 matrix
#     [,1] [,2] [,3] [,4]
#[1,]    1    2    3    4
#[2,]    1    2    3    4

#Cholesky decomposition
dat = matrix(1:4, ncol = 2)
A = dat%*%t(dat)
chol(A)

library(dplyr)


#Direct sum of two matrices
A = matrix(1:4, ncol = 2)
B = matrix(1:6, ncol = 3)
A1 = rbind(A, matrix(rep(0, 4), ncol = 2))
B1 = rbind(matrix(rep(0, 6), ncol = 3), B)
C = cbind(A1, B1) #= direct sum of A and B
C
#     [,1] [,2] [,3] [,4] [,5]
#[1,]    1    3    0    0    0
#[2,]    2    4    0    0    0
#[3,]    0    0    1    3    5
#[4,]    0    0    2    4    6

#Express the direct sum by Kronecker products
kronecker.prod(diag(1,0), A) + kronecker.prod(diag(0,1), B)
#     [,1] [,2] [,3] [,4] [,5]
#[1,]    1    3    0    0    0
#[2,]    2    4    0    0    0
#[3,]    0    0    1    3    5
#[4,]    0    0    2    4    6

#SVD example for a 2-by-3 matrix: R code
A=matrix(c(-1,1,0,2,-2,3),nrow=2)
A #Show the 2-by-3 matrix
#     [,1] [,2] [,3]
#[1,]   -1    0   -2
#[2,]    1    2    3
svdA=svd(A) #Compute the SVD of A and put the results in svdA
svdA #Show SVD results: d, U, and V
round(svdA$d, digits=2) #Show only the singular values
#[1] 4.22 1.09
round(svdA$u, digits=2) #Show only matrix U
#      [,1] [,2]
#[1,] -0.48 0.88
#[2,]  0.88 0.48
round(svdA$v, digits=2)#Show only matrix V
#     [,1]  [,2]
#[1,] 0.32 -0.37
#[2,] 0.42  0.88
#[3,] 0.85 -0.29
sqrt(eigen(A%*%t(A))$values)
#[1] 4.221571 1.085514


#Data reconstruction by singular vectors: R code
round(svdA$d[1]*svdA$u[,1]%*%t(svdA$v[,1]), 
      digits=1)
#     [,1] [,2] [,3]
#[1,] -0.7 -0.8 -1.7
#[2,]  1.2  1.5  3.2

round(svdA$d[1]*svdA$u[,1]%*%t(svdA$v[,1]) + 
        svdA$d[2]*svdA$u[,2]%*%t(svdA$v[,2]), 
      digits =2)
#     [,1] [,2] [,3]
#[1,]   -1    0   -2
#[2,]    1    2    3


#R plot schematic diagram of SVD
setwd('/Users/sshen/climstats')
setEPS() #Plot the figure and save the file
postscript("fig0503.eps", width = 11)
par(mar=c(0,0,0,0))
plot(200, axes = FALSE,
     xlab = "", ylab = "",
     xlim = c(-3,28), ylim = c(-3,16))
text(13,15.5, cex=2.2,
     bquote("SVD:" ~ A==UDV^t~ "when n > m or n < m"))
#Space-time data matrix A when n>m
segments(x0 = c(0,0,3,3),
         y0 = c(6,12,12,6) +1,
         x1 = c(0,3,3,0),
         y1 = c(12,12,6,6) +1, 
         col = c('blue','red','blue','red'),lwd =3)
segments(x0 = c(0.5,1.0),
         y0 = c(6,6)+1,
         x1 = c(0.5,1.0),
         y1 = c(12,12)+1,
         lwd =1.3, lty = 3)
text(-.8, 9+1, 'n', srt=90, col ='blue', cex = 1.4)
text(1.5, 12.8+1, 'm', col = 'red',  cex = 1.4)
text(2.0, 9+1, '...',  cex = 1.4)
text(2, 5+1, bquote(A[n%*%m]),  cex = 2.5)
text(5, 9+1, '=',  cex = 3)
#Spatial matrix U
segments(x0 = c(7,7,10,10),
         y0 = c(6,12,12,6)+1,
         x1 = c(7,10,10,7),
         y1 = c(12,12,6,6)+1, 
         col = c('blue','blue','blue','blue'), lwd =3)
segments(x0 = c(7.5,8),
         y0 = c(6,6)+1,
         x1 = c(7.5,8),
         y1 = c(12,12)+1,
         lwd =1.3, lty = 3, col = 'blue')
text(6.2, 9+1, 'n', srt=90, col ='blue', cex = 1.4)
text(8.5, 12.8+1, 'm', col = 'red',  cex = 1.4)
text(9, 9+1, '...',  cex = 1.4, col='blue')
text(8.7, 5.0+1, bquote(U[n%*%m]),  cex = 2.5, col= 'blue')
#Singular value diagonal matrix D
segments(x0 = c(12,12,15,15),
         y0 = c(9,12,12,9)+1,
         x1 = c(12,15,15,12),
         y1 = c(12,12,9,9)+1, 
         col = c('brown','brown','brown','brown'), lwd =3)
segments(x0 = 12, y0 = 12+1, x1 = 15, y1 = 9+1, lty=3,
         col = c('brown'), lwd =1.3)#diagonal line
text(11.2, 10.5+1, 'm', srt=90, col ='red', cex = 1.4)
text(13.5, 12.8+1, 'm', col = 'red',  cex = 1.4)
text(14.1, 11.3+1, '0', col = 'brown',  cex = 1.4)
text(12.9, 10.0+1, '0', col = 'brown',  cex = 1.4)
text(13.9, 8.0+1, bquote(D[m%*%m]),  cex = 2.5, col='brown')
#Temporal matrix V
segments(x0 = c(17,17,20,20),
         y0 = c(9,12,12,9)+1,
         x1 = c(17,20,20,17),
         y1 = c(12,12,9,9)+1, 
         col = c('red','red','red','red'), lwd =3)
segments(x0 = c(17,17),
         y0 = c(11.5,10.8)+1,
         x1 = c(20,20),
         y1 = c(11.5,10.8)+1, 
         col = c('red','red'), lty=3, lwd =1.3)
text(16.2, 10.5+1, 'm', srt=90, col ='red', cex = 1.4)
text(18.5, 12.5+1, 'm', col = 'red',  cex = 1.4)
text(19.5, 8+1, bquote((V^t)[m%*%m]),  cex = 2.5, col='red')
text(18.5, 10+1, '...',  col='red', srt=90, cex =1.4)
# Space-time data matrix B when n < m
segments(x0 = c(0,0,6,6),
         y0 = c(0,3,3,0),
         x1 = c(0,6,6,0),
         y1 = c(3,3,0,0), 
         col = c('blue','red','blue','red'), lwd =3)
segments(x0 = c(1,2,5),
         y0 = c(0,0,0),
         x1 = c(1,2,5),
         y1 = c(3,3,3),
         lwd =1.3, lty = 3)
text(-0.8, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(3, 3.8, 'm', col = 'red',  cex = 1.4)
text(3.5, 1.5, '...',  cex = 1.4)
text(3, -1.5, bquote(A[n%*%m]),  cex = 2.5)
text(8, 1.5, '=',  cex = 3)
#Spatial matrix U
segments(x0 = c(11,11,14,14),
         y0 = c(0,3,3,0),
         x1 = c(11,14,14,11),
         y1 = c(3,3,0,0), 
         col = c('blue','blue','blue','blue'), lwd =3)
segments(x0 = c(11.5,12.2),
         y0 = c(0,0),
         x1 = c(11.5,12.2),
         y1 = c(3,3),
         lwd =1.3, lty = 3, col = 'blue')
text(10.2, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(12.5, 3.8, 'n', col = 'blue',  cex = 1.4)
text(13.2, 1.5, '...',  cex = 1.4, col='blue')
text(12.5, -1.5, bquote(U[n%*%n]),  cex = 2.5, col= 'blue')
#Singular value diagonal matrix D
segments(x0 = c(16,16,19,19),
         y0 = c(0,3,3,0),
         x1 = c(16,19,19,16),
         y1 = c(3,3,0,0), 
         col = c('brown','brown','brown','brown'), lwd =3)
segments(x0 = 16, y0 = 3, x1 = 19, y1 = 0, lty=3,
         col = c('brown'), lwd =1.3)#diagonal line
text(15.2, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(17.5, 3.8, 'n', col = 'blue',  cex = 1.4)
text(18.1, 2.3, '0', col = 'brown',  cex = 1.4)
text(16.9, 1.0, '0', col = 'brown',  cex = 1.4)
text(17.5, -1.5, bquote(D[n%*%n]),  cex = 2.5, col='brown')
#Temporal matrix V
segments(x0 = c(21,21,27,27),
         y0 = c(0,3,3,0),
         x1 = c(21,27,27,21),
         y1 = c(3,3,0,0), 
         col = c('red','red','red','red'),
         lwd =3)
segments(x0 = c(21,21),
         y0 = c(2.5,1.8),
         x1 = c(27,27),
         y1 = c(2.5,1.8), 
         col = c('red','red'), lty=3, lwd =1.3)
text(20.2, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(24, 3.8, 'm', col = 'red',  cex = 1.4)
text(24, -1.5, bquote((V^t)[n%*%m]),  cex = 2.5, col='red')
text(24, 1, '...',  col='red', srt=90, cex =1.4)
dev.off()

#R SVD analysis for the weighted SOI from SLP data
#for seven years: 2009-2015
setwd("/Users/sshen/climmath")
Pda<-read.table("data/PSTANDdarwin.txt", header=F)
dim(Pda) 
#[1] 65 13 #Monthly Darwin data from 1951-2015
pdaDec<-Pda[,13] #Darwin Dec standardized SLP anomalies data
Pta<-read.table("data/PSTANDtahiti.txt", header=F)
ptaDec=Pta[,13] #Tahiti Dec standardized SLP anomalies
ptada1 = cbind(pdaDec, ptaDec) #space-time data matrix

#Space-time data format
ptada = t(ptada1[59:65,]) #2009-2015 data
colnames(ptada)<-2009:2015
rownames(ptada)<-c("Darwin", "Tahiti")
ptada #6 year of data for two stations
#       2009 2010 2011 2012 2013 2014 2015
#Darwin  0.5 -2.3 -2.2  0.3  0.3  0.1 -0.4
#Tahiti -0.7  2.5  1.9 -0.7  0.4 -0.8 -1.3
svdptd = svd(ptada) #SVD for the 2-by-6 matrix
U=round(svdptd$u, digits=2)
U
#[1,] -0.66 0.75
#[2,]  0.75 0.66
D=round(diag(svdptd$d), digits=2)
D
#[1,]  4.7 0.00
#[2,]  0.0 1.42
V =round(svdptd$v, digits=2)
t(V)
#[1,] -0.18  0.72  0.61 -0.15 0.02 -0.14 -0.15
#[2,] -0.06 -0.06 -0.28 -0.17 0.34 -0.32 -0.82


#R SVD analysis for the weighted SOI from SLP data
#For the entire period of 1951-2015
setwd("/Users/sshen/LinAlg")
Pda<-read.table("data/PSTANDdarwin.txt", header=F)
dim(Pda) 
#[1] 65 13 #Monthly Darwin data from 1951-2015
pdaDec<-Pda[,13] #Darwin Dec standardized SLP anomalies data
Pta<-read.table("data/PSTANDtahiti.txt", header=F)
ptaDec=Pta[,13] #Tahiti Dec standardized SLP anomalies
ptada1 = cbind(pdaDec, ptaDec) #space-time data matrix

#Space-time data format
ptada = t(ptada1) #2009-2015 data
colnames(ptada)<-1951:2015
rownames(ptada)<-c("Darwin", "Tahiti")
ptada[,1:6] #6 year of data for two stations
#       1951 1952 1953 1954 1955 1956
#Darwin  1.4  0.9  0.6 -0.8  0.2 -1.6
#Tahiti  0.1 -1.1 -0.1  1.5  1.8  0.1
#df = data.frame(ptada)
dim(ptada)
#[1]  2 65
svdptd = svd(ptada) #SVD for the 2-by-65 matrix
U=round(svdptd$u, digits=2)
U
#      [,1] [,2]
#[1,] -0.75 0.66
#[2,]  0.66 0.75
D=round(diag(svdptd$d), digits=2)
D
#      [,1] [,2]
#[1,] 10.02 0.00
#[2,]  0.00 7.09
V =round(svdptd$v, digits=2)
t(V)[,1:5] #The first five year from 1951-1955
#      [,1]  [,2]  [,3] [,4] [,5]
#[1,] -0.10 -0.14 -0.05 0.16 0.10
#[2,]  0.14 -0.03  0.05 0.08 0.21

#
plot(1951:2015, -V[,1], type = 'o', 
     ylab = "PC", xlab = 'Year',
     main = "Tahiti-Darwin SLP Principal Components",
     col = 'black', ylim = c(-0.45, 0.45))
lines(1951:2015, -V[,2], type = 'l', 
      lty = 2, col = 'purple')
legend(1950, 0.5, lwd = 2, c("PC1", "PC2"), col = c('blue', 'Purple'),
       lty = c(1,2), bty="n")
#El Nino samples
points(c(1982, 1997), c(-V[32,1], -V[47,1]), 
       pch = 16, col = 'red')
text(1982, -V[32,1] + 0.07, "EN", col = 'red')
text(1997, -V[47,1] + 0.07, "EN", col = 'red')
#La Nina samples
points(c(1975, 2010), c(-V[25,1], -V[60,1]), 
       pch = 16, col = 'blue')
text(1975, -V[25,1] - 0.07, "LN", col = 'blue')
text(2010, -V[60,1] - 0.07, "EN", col = 'blue')

t = 1951:2015
y = -3*V[,1] + 0.3*cumsum(V[,1]) - 0.3*cumsum(V[,2])
plot(t, y, type = 'o', 
     ylab = "PC", xlab = 'Year',
     main = "Tahiti-Darwin SLP Principal Components",
     col = 'black', ylim = c(-1, 1))
lines(1951:2015, -V[,2], type = 'l', 
      lty = 2, col = 'purple')
legend(1950, 0.5, lwd = 2, c("PC1", "PC2"), col = c('blue', 'Purple'),
       lty = c(1,2), bty="n")
#El Nino samples
points(c(1982, 1997), c(-V[32,1], -V[47,1]), 
       pch = 16, col = 'red')
text(1982, -V[32,1] + 0.07, "EN", col = 'red')
text(1997, -V[47,1] + 0.07, "EN", col = 'red')
#La Nina samples
points(c(1975, 2010), c(-V[25,1], -V[60,1]), 
       pch = 16, col = 'blue')
text(1975, -V[25,1] - 0.07, "LN", col = 'blue')
text(2010, -V[60,1] - 0.07, "EN", col = 'blue')


t= 1951:2015
plot(t, cumsum(V[,1]), type = 'l', 
     ylim = c(-1,1), ylab = "Cumsum index")
lines(t, cumsum(V[,2]), 
      type = 'l', col = 'red') 
lines(t, -cumsum(V[,1]) + cumsum(V[,2]), 
      type = 'l', col = 'blue') 

#For a better computer display
library(plotly)
PC = data.frame(1951:2015, V)
colnames(PC) = c("Year", "PC1", "PC2")
plot_ly(data = PC, x = ~Year, y = ~(-PC1), 
        type = 'lines+markers') %>%
  layout(yaxis = list(title = 'PC1'))


#Minors and co-factor
minors <- function(b){
  n <- nrow(b)
  a <- matrix(NA, n, n)
  for(i in 1:n)
    for(j in 1:n)
      a[i, j] = det(b[-i, -j])
  a
}
b = matrix(c(2,3,5,6,7,1,9,4,5), 
           nrow = 3, ncol = 3)
minors(b) 
#     [,1] [,2] [,3]
#[1,]   31   -5  -32
#[2,]   21  -35  -28
#[3,]  -39  -19   -4


cofactors <- function(b) (-1)^(row(b)+col(b)) *minors(b)
cofactors(b)
#     [,1] [,2] [,3]
#[1,]   31    5  -32
#[2,]  -21  -35   28
#[3,]  -39   19   -4
#Adjoint matrix, aka, adjugate of a square matrix
A <- matrix(c(1,4,5,3,7,2,2,8,3),nrow=3,ncol=3)
A
#install.packages('RConics')
library(RConics)
B <- adjoint(A)
B
#B = det(A) * inverse of A
C = det(A)*solve(A)
C - B

#Jingle Bells Music in R
#by Keith McNulty in 2018
#https://paulvanderlaken.com/2017/12/18/jingle-bells-in-r/

if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"audio" %in% installed.packages()) install.packages("audio")

library("dplyr")
library("audio")

notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)

pitch <- paste("E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E",
               "E D D E",
               "D G",
               "E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E E",
               "G G F D",
               "C",
               "G3 E D C",
               "G3",
               "G3 G3 G3 E D C",
               "A3",
               "A3 F E D",
               "B3",
               "G G F D",
               "E",
               "G3 E D C",
               "G3",
               "G3 E D C",
               "A3 A3",
               "A3 F E D",
               "G G G G A G F D",
               "C C5 B A G F G",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "E D D D D E",
               "D D E F G F E D",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "G C5 B A G F E D",
               "C C E G C5")

duration <- c(1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              2, 2,
              1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 0.5, 0.5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, .5, .5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 0.5, 0.5, 1,
              1, 0.33, 0.33, 0.33, 1, 0.33, 0.33, 0.33,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.33, 0.33, 0.33, 2)

jbells <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                     duration = duration)

jbells <- jbells %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
           {suppressWarnings(as.numeric(.))} %>%
           ifelse(is.na(.), 4, .),
         note = notes[substr(pitch, 1, 1)],
         note = note + grepl("#", pitch) -
           grepl("b", pitch) + octave * 12 +
           12 * (note < 3),
         freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 250

sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

jbells_wave <- mapply(make_sine, jbells$freq, jbells$duration) %>%
  do.call("c", .)

play(jbells_wave)

#
#
#Jingle Bells Music in R
#by Keith McNulty in 2018
#https://paulvanderlaken.com/2017/12/18/jingle-bells-in-r/


#Original version
if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"audio" %in% installed.packages()) install.packages("audio")

library("dplyr")
library("audio")

notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)

pitch <- paste("E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E",
               "E D D E",
               "D G",
               "E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E E",
               "G G F D",
               "C",
               "G3 E D C",
               "G3",
               "G3 G3 G3 E D C",
               "A3",
               "A3 F E D",
               "B3",
               "G G F D",
               "E",
               "G3 E D C",
               "G3",
               "G3 E D C",
               "A3 A3",
               "A3 F E D",
               "G G G G A G F D",
               "C C5 B A G F G",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "E D D D D E",
               "D D E F G F E D",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "G C5 B A G F E D",
               "C C E G C5")

duration <- c(1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              2, 2,
              1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 0.5, 0.5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, .5, .5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 0.5, 0.5, 1,
              1, 0.33, 0.33, 0.33, 1, 0.33, 0.33, 0.33,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.33, 0.33, 0.33, 2)

jbells <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                     duration = duration)

jbells <- jbells %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
           {suppressWarnings(as.numeric(.))} %>%
           ifelse(is.na(.), 4, .),
         note = notes[substr(pitch, 1, 1)],
         note = note + grepl("#", pitch) -
           grepl("b", pitch) + octave * 12 +
           12 * (note < 3),
         freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 250

sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

jbells_wave <- mapply(make_sine, jbells$freq, jbells$duration) %>%
  do.call("c", .)

play(jbells_wave)

#Revised version

if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"audio" %in% installed.packages()) install.packages("audio")

library("dplyr")
library("audio")

notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)

pitch <- paste("F F F",
               "G G G",
               "G G C D",
               "E",
               "F F F F",
               "G B B B",
               "E D D E",
               "D G",
               "E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E E",
               "G G F D",
               "C",
               "G3 E D C",
               "G3",
               "G3 G3 G3 E D C",
               "A3",
               "A3 F E D",
               "B3",
               "G G F D",
               "E",
               "G3 E D C",
               "G3",
               "G3 E D C",
               "A3 A3",
               "A3 F E D",
               "G G G G A G F D",
               "C C5 B A G F G",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "E D D D D E",
               "D D E F G F E D",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "G C5 B A G F E D",
               "C C E G C5")

duration <- c(1, 3, 2,
              3, 1, 2,
              1, 2, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 3, 1,
              2, 2,
              1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 0.5, 0.5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, .5, .5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 0.5, 0.5, 1,
              1, 0.33, 0.33, 0.33, 1, 0.33, 0.33, 0.33,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.33, 0.33, 0.33, 2)

jbells <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                     duration = duration)

jbells <- jbells %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
           {suppressWarnings(as.numeric(.))} %>%
           ifelse(is.na(.), 4, .),
         note = notes[substr(pitch, 1, 1)],
         note = note + grepl("#", pitch) -
           grepl("b", pitch) + octave * 12 +
           12 * (note < 3),
         freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 250

sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

jbells_wave <- mapply(make_sine, jbells$freq, jbells$duration) %>%
  do.call("c", .)

play(jbells_wave)





