#*******************************************
#Vector normalization
#*******************************************

#Vector magnitude
calcMagnitude <- function(v) {
  sq <- (v)^2
  sumSq <- sum(sq)
  vectLength <- sqrt(sumSq)
  vectLength
}

normalizeVect <- function(v) {
  sq <- (v)^2
  sumSq <- sum(sq)
  vectLength <- sqrt(sumSq)
  unitVector <- (1/vectLength)*v
  unitVector
}

#Quiz

vect1 <- c(-0.221, 7.437)
calcMagnitude(vect1)
# [1] 7.440283

vect2 <- c(8.813, -1.331, -6.247)
calcMagnitude(vect2)
# [1] 10.88419

vect3 <- c(5.581, -2.136)
normalizeVect(vect3)
# [1]  0.9339352 -0.3574423

vect4 <- c(1.996, 3.108, -4.554)
normalizeVect(vect4)
# [1]  0.3404013  0.5300437 -0.7766470

rm(vect1, vect2, vect3, vect4)

#*******************************************
#Dot product
#*******************************************

v1 <- c(7.884, 4.138)
w1 <- c(-8.802, 6.776)
v2 <- c(-5.955, -4.904, -1.874)
w2 <- c(-4.496, -8.755, 7.103)
v3 <- c(3.183, -7.627)
w3 <- c(-2.688, 5.319)
v4 <- c(7.35, 0.221, 5.188)
w4 <- c(2.751, 8.259, 3.985)

findCosineTheta <- function(x, y) {
  
  length.prod <- sqrt(sum(x ^ 2)) * sqrt(sum(y ^ 2))
  dot.prod <- sum(x * y) 
  cos.theta <- dot.prod / length.prod
  cos.theta
}



findTheta <- function(x,y) {
  theta <- acos(findCosineTheta(x,y))
  
  ##acos produces result in rad
  
  theta <- as.numeric(theta)
  theta
}

#convert rad to deg 
radToDeg <- function (rad) {
  deg <- (rad * 180) / (pi)
  deg
}

#convert deg to rad
degToRad <- function (deg) {
  rad <- (deg * (pi)) / 180
  rad
}

#Quiz
#dot product
sum(v1*w1)
# [1] -41.35588
#Udacity bug?

sum(v2*w2)
# [1] 56.39718


findTheta(v3,w3)
# [1] 3.069027
# result matches Wolfram, Udacity bug?

radToDeg(findTheta(v4,w4))
# [1] 60.27581


#*******************************************
#Parallel And Orthogonal Vectors
#*******************************************
v1 <- c(-7.579, -7.88)
w1 <- c(22.737, 23.64)
v2 <- c(-2.029, 9.97, 4.172)
w2 <- c(-9.231, 6.639, -7.245)
v3<- c(-2.238, -7.284, -1.214)
w3 <- c(-1.821, 1.072, -2.94)
v4<- c(2.118, 4.827)
w4 <- c(0,0)

#check if vectors are parallel -> abs(unitVector) should be the same
#                             OR
#                              -> angle between the 0 or 180 deg / 0 or (pi) rad
##                                findTheta(v1,w1) == (pi)
##                                [1] TRUE
checkVectParallel <- function (x, y) {
#with 3 decimal precision
  ifelse(calcMagnitude(x) == 0 || calcMagnitude(y) == 0, TRUE, all(round(abs(normalizeVect(x))) == round(abs(normalizeVect(y)))))
}
checkVectParallel(v1, w1)
# [1] TRUE

checkVectParallel(v2, w2)
# [1] FALSE

checkVectParallel(v3, w3)
# [1] FALSE

checkVectParallel(v4, w4)
# [1] TRUE

##check if orthogonal -> dot product should be 0
checkVectOrthogonal <- function (x, y) {
  #won't work without rounding
  round(sum((x)*(y))) == 0
}
checkVectOrthogonal(v1, w1)
# [1] FALSE
checkVectOrthogonal(v2, w2)
# [1] FALSE
checkVectOrthogonal(v3, w3)
# [1] TRUE
checkVectOrthogonal(v4, w4)
# [1] TRUE

rm(v1,v2,v3,v4,w1,w2,w3,w4)

#*******************************************
#Vector Projections
#*******************************************

#Quiz
v1 <- c(3.039, 1.879)
b1 <- c(0.825, 2.036)
v2 <- c(-9.88, -3.264, -8.159)
b2 <- c(-2.155, -9.353, -9.473)
v3 <- c(3.009, -6.172, 3.692, -2.51)
b3 <- c(6.404, -9.144, 2.759, 8.718)

#projection of v on b (v par)
#v perp
findProjection <- function(v,b) {
  unitVectBase <- normalizeVect(b)
  dotProd <- sum(v*unitVectBase)
  projection <- dotProd*unitVectBase
  projection
}

findPerp <- function(v, b) {
  vPerp <- v - findProjection(v,b)
  vPerp
}

findProjection(v1, b1)
# [1] 1.082607 2.671743

findPerp(v2, b2)
# [1] -8.350081  3.376061 -1.433746

findProjection(v3, b3)
# [1]  1.968516 -2.810761  0.848085  2.679813
findPerp(v3,b3)
# [1]  1.040484 -3.361239  2.843915 -5.189813

rm(v1,v2,v3,b1,b2,b3)
#*******************************************
#Cross Products
#*******************************************

#Quiz
#3 dimensional vectors
#cross product
#area of parallelogram
#area of triangle

v1 <- c(8.462, 7.893, -8.187)
w1 <- c(6.984, -5.975, 4.778)
v2 <- c(-8.987, -9.838, 5.031)
w2 <- c(-4.268, -1.861, -8.866)
v3<- c(1.5, 9.547, 3.691)
w3<- c(-6.007, 0.124, 5.772)

findCrossProd <- function(v, w) {
  crossProd <- c(v[[2]]*w[[3]] - w[[2]]*v[[3]], -(v[[1]]*w[[3]] - w[[1]]*v[[3]]), v[[1]]*w[[2]]-w[[1]]*v[[2]])
  crossProd
}

crossProd1 <- findCrossProd(v1, w1)
crossProd1 
# [1]  -11.20457  -97.60944 -105.68516
checkVectOrthogonal(w1, crossProd1)
# [1] TRUE ##when rounding sum prod


findParallelogram <- function (v, w) {
  sqCrossProd <- findCrossProd(v,w)^2
  sumSqCrossProd <- sum(sqCrossProd)
  lengthCrossProd <- sqrt(sumSqCrossProd)
  lengthCrossProd
}
findParallelogram(v2, w2)
# [1] 142.1222
## OR - sanity check
calcMagnitude(findCrossProd(v2, w2))
# [1] 142.1222 ##same

findParallelogram(v3, w3)*0.5
# [1] 42.56494 ##triangle area