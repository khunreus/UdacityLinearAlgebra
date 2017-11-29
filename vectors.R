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
