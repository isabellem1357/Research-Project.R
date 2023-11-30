#3.1
points <- matrix(c(1, 1, 2, 2, 2, 3, 3, 4, 4, 4), ncol = 2, byrow = TRUE)
kmeans_result <- kmeans(points, centers = 2)
plot(points, col = kmeans_result$cluster, pch = 19, xlab = "X-coordinate", ylab = "Y-coordinate", main = "K-means Clustering")
points(kmeans_result$centers, col = 1:2, pch = 4)
text(1.6, 2, bquote(C[1]), col = 'black', cex = 1)
text(3.4, 4, bquote(C[2]), col = 'red', cex = 1)
text(1.1, 1, bquote(P[1]), cex = 1, col = 'black')
text(2.1, 2, bquote(P[2]), cex = 1, col = 'black')
text(2.1, 3, bquote(P[3]), cex = 1, col = 'black')
text(2.9, 4, bquote(P[4]), cex = 1, col = 'red')
text(4, 3.9, bquote(P[5]), cex = 1, col = 'red')
tWCSS <- kmeans_result$tot.withinss
tWCSS

#3.3
data <- read.csv('MiamiIntlAirport2001_2020.csv')
which(data$DATE == '2015-01-01')
which(data$DATE == '2015-12-31')
tmin <- data[(5114:5478),12]
wdf2 <- data[(5114:5478),13]
matrix <- matrix(c(tmin, wdf2), ncol = 2, byrow = FALSE)
matrix
kmeans_result <- kmeans(matrix, centers = 2)
plot(matrix, col = kmeans_result$cluster, pch = 19, xlab = "TMIN_2015", ylab = "WDF2_2015", main = "K-means Clustering")
points(kmeans_result$centers, col = 1:2, pch = 8)

#3.8
library(e1071)
library(ggplot2)
points <- matrix(c(1, 1, 2, 2, 2, 3, 3, 4, 4, 4), ncol = 2, byrow = TRUE)
labels <- c(1, 1, 1, 2, 2)
svm_model <- svm(points, labels, type = "C-classification", kernel = "linear")
w <- t(svm_model$coefs) %*% svm_model$SV
b <- svm_model$coefs[1, 1]
Dm <- 1 / sqrt(sum(w^2))
w
b
Dm
x = matrix(c(1, 1, 2, 2, 2, 3, 3, 4, 4, 4),
           ncol = 2, byrow = TRUE)
y = c(1, 1, 1, -1, -1) 
plot(x, col = y + 5, pch = 19,
     xlim = c(0, 6), ylim = c(0, 6),
     xlab = 'X Values', ylab = 'Y Values', main = "SVM Analysis")
library(e1071)
dat = data.frame(x, y = as.factor(y)) 
svm3P = svm(y ~ ., data = x,
            kernel = "linear", cost = 10, 
            scale = FALSE ,
            )
w = t(svm3P$coefs) %*% svm3P$SV
w
b = svm3P$rho
b
support_vectors <- points[svm_model$index, ]
support_vectors
2/norm(w, type = '2')

x1 = seq(0, 10, len = 31)
x2 = (b - w[1]*x1)/w[2]
x2p = (1 + b - w[1]*x1)/w[2]
x2m = (-1 + b - w[1]*x1)/w[2] 
x20 = (b - w[1]*x1)/w[2]
axis(2, at = (-2):8, tck = 1, lty = 2, col = "grey", labels = NA)
axis(1, at = (-2):8, tck = 1, lty = 2, col = "grey", labels = NA)
lines(x1, x2p, lty = 4, col = 2)
lines(x1, x2m, lty = 2, col = 4) 
lines(x1, x20, lwd = 1.5, col = 'purple') 
text(x = 4.5, y = 4.5, labels = "w * x - b = 1", col = "lightblue", font = 2, cex = 1)
text(x = 2.5, y = 0.5, labels = "w * x - b = -1", col = "red", font = 2, cex = 1)
text(x = 3.5, y = 2.5, labels = "w * x - b = 0", col = "purple", font = 2, cex = 1)



#3.9
xnew = matrix(c(1.5, 1, 3, 3),
              ncol = 2, byrow = TRUE)
points(xnew, pch = 17, cex = 2)
predict(svmP, xnew)
for(i in 1:2){
  text(xnew[i,1], xnew[i,2] - 0.4 ,
       paste('Q',i), cex = 1) 
  }

#3.10
data(iris)
library(randomForest)
str(iris)
plot(iris[,1], type = 'o', pch = 16, cex = 0.5, ylim = c(-1, 9),
     xlab = 'Sorted order of the flowers for measurement', ylab = 'Length or width [cm]',
     main = 'R.A. Fisher data of irish flowers', col = 1, cex.lab = 1.3, cex.axis = 1.3)
  lines(iris[,2], type = 'o', pch = 16, cex = 0.5, col=2) 
  lines(iris[,3], type = 'o', pch = 16, cex = 0.5, col=3) 
  lines(iris[,4], type = 'o', pch = 16, cex = 0.5, col=4) 
  legend(0, 9.5, legend = c('Sepal length', 'Sepal width', 'Petal length', 'Petal width'), 
         col = 1:4, lty = 1, lwd = 2, bty = 'n',
         y.intersp = 0.8, cex = 1.2)
  text(25, -1, 'Setosa 1-50', cex = 1.3)
  text(75, -1, 'Versicolor 51-100', cex = 1.3) 
  text(125, -1, 'Virginica 101-150', cex = 1.3)
train_id = sort(sample(1:150, 120, replace = FALSE))
train_data = iris[train_id,]
dim(train_data)
train_id
new_data = iris[-train_id,]
dim(new_data)
classifyRF = randomForest(x = train_data[, 1:4], 
                          y = train_data[, 5], ntree = 800)
classifyRF
predict(classifyRF, newdata = new_data[, 1:4])


#3.14
library(randomForest)
data(iris)
set.seed(42)
indices_setosa <- sample(which(iris$Species == "setosa"), 0.2 * sum(iris$Species == "setosa"))
indices_versicolor <- sample(which(iris$Species == "versicolor"), 0.2 * sum(iris$Species == "versicolor"))
indices_virginica <- sample(which(iris$Species == "virginica"), 0.2 * sum(iris$Species == "virginica"))
train_indices <- c(indices_setosa, indices_versicolor, indices_virginica)
train_data <- iris[train_indices, ]
test_indices <- setdiff(1:nrow(iris), train_indices)
test_data <- iris[test_indices, ]
rf_model <- randomForest(Species ~ ., data = train_data, importance = TRUE, ntree = 500)
rf_model
new_data_indices <- sample(test_indices, 0.1 * length(test_indices))
new_data <- iris[new_data_indices, ]
predicted_species <- predict(rf_model, newdata = new_data)
predicted_species
comparison <- data.frame(ActualSpecies = new_data$Species, PredictedSpecies = predicted_species)
comparison

#3.19
data(iris)
iris$setosa = iris$Species == "setosa"
iris$virginica = iris$Species == "virginica"
iris$versicolor = iris$Species == "versicolor"
p = 0.5
train.idx = sample(x = nrow(iris), size = p*nrow(iris))
train = iris[train.idx,]
test = iris[-train.idx,]
dim(train)
library(neuralnet)
iris.nn = neuralnet(setosa + versicolor + virginica ~
                      Sepal.Length + Sepal.Width +
                      Petal.Length + Petal.Width ,
                    data = train , hidden=c(10, 10),
                    rep = 5, err.fct = "ce",
                    linear.output = F, lifesign = "minimal",
                    stepmax = 1000000, threshold = 0.001)
iris.nn
plot(iris.nn, rep = "best")
prediction = neuralnet :: compute(iris.nn, test[,1:4])
prediction$net.result[1:2,]
pred.idx <- apply(prediction$net.result ,1, which.max)
pred.idx
predicted <- c('setosa', 'versicolor', 'virginica')[pred.idx]              
predicted[1:6]                  
table(predicted, test$Species)                  
                  
#3.20
