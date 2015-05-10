#  Name:  corner-detect.R
#  In:    EBImage file
#  Out:   EBImage File (filter applied to highlight a few “pointy” spots)

corner_detect <- function(target_image){
  target_image <- imageData(target_image)
  
  y <- dim(target_image)[1]
  x <- dim(target_image)[2]

  grad.x <- matrix(rep(0, times = x*y), nrow = y)
  grad.y <- matrix(rep(0, times = x*y), nrow = y)
  grad.xy <- matrix(rep(0, times = x*y), nrow = y)
  grad.r <- matrix(rep(0, times = x*y), nrow = y)

  # X gradient
  for(i in 2:(x-1))
    for(j in 2:(y-1))
      grad.x[j,i] <- (target_image[j,i+1]-target_image[j,i-1])/2
  
  # Y gradient
  for(i in 2:(x-1))
    for(j in 2:(y-1))
      grad.y[j,i] <- (target_image[j+1,i]-target_image[j-1,i])/2
  

  sumgrad <- grad.x+grad.y
  
  # H matrix.
  for(i in 2:(x-1))
    for(j in 2:(y-1))
      grad.xy[j,i] <- (sumgrad[j+1,i]-sumgrad[j-1,i])/2
  
  
  # H matrix.
  #  According to R. Collins:
  #  k is imperically determined to be in (0.04, 0.06)
  k <- 0.04
  for(i in 2:(x-1))
    for(j in 2:(y-1)){
      h <- matrix(c(grad.xy[j,i]^2, grad.xy[j,i], grad.xy[j,i], grad.y[j,i]^2), nrow = 2)
      grad.r[j,i] <- det(h) - k*sum(diag(h))^2
      
    }
  
  return(grad.r)  
  
}

# Bytecode Compiled Version: ---------------------------
library(compiler)

byte_corner <- function(target_image){
  #img_dim <- dim(target_image)
  y <- dim(target_image)[1]
  x <- dim(target_image)[2]
  
  grad.x <- matrix(rep(0, times = x*y), nrow = y)
  grad.y <- grad.x
  grad.xy <- grad.x
  grad.r <- grad.x
  
  #  According to R. Collins:
  #  k is imperically determined to be in (0.04, 0.06)
  k <- 0.04
  
  
  # X gradient
  for(i in 2:(x-1))
    for(j in 2:(y-1)){
      grad.x[j,i] <- (target_image[j,i+1]-target_image[j,i-1])/2
      grad.y[j,i] <- (target_image[j+1,i]-target_image[j-1,i])/2
    }


  # H matrix.
  for(i in 2:(x-1))
    for(j in 2:(y-1))
      grad.xy[j,i] <- (grad.x[j+1,i]-grad.x[j-1,i])/2
  
  
  # H matrix.
  #  According to R. Collins:
  #  k is imperically determined to be in (0.04, 0.06)
  k <- 0.04
  for(i in 2:(x-1))
    for(j in 2:(y-1)){
      h <- matrix(c(grad.xy[j,i]^2, grad.xy[j,i], grad.xy[j,i], grad.y[j,i]^2), nrow = 2)
      grad.r[j,i] <- det(h) - k*sum(diag(h))^2
    }
  
  return(grad.r)  
  
}

bc <- cmpfun(byte_corner)
bc2 <- cmpfun(corner_detect)


# Threshold ---------------------------
px_thresh <- function(target_image, thresh = 0.05){
  fish <- as.vector(imageData(target_image))
  fish_quantile  <- quantile(fish[fish<1],thresh)
  paste(fish_quantile)
  high <-ifelse(fish>quantile(fish,fish_quantile),1,0)
  high <- matrix(high, nrow = dim(target_image)[1])
  return(high)
}

quantile(fish,0.001)
thresh  <- 0.1

high_spots <- px_thresh(images[[1]], thresh = 0.0015)

image(high_spots)
system.time(corner_detect(high_spots))
high_corner <- corner_detect(high_spots)
image(high_corner)

image(images[[2]])
View(images[[2]])
sum(images[[2]]<0.2)
high_spots <- px_thresh(images[[2]], thresh = 0.05)
hist(high_spots)
image(high_spots)

test_mat <- matrix(runif(n = 25), nrow = 5)
hist(test_mat[test_mat>0.2])
hist(images[[1]][images[[1]]<1])
hist(images[[1]])

image(high_spots)
system.time(corner_detect(high_spots))
high_corner <- corner_detect(high_spots)
image(high_corner)


target_image <- images[[1]]

image(high)




# Code I used in constructing (to be deleted) ---------------------------

corners <- corner_detect(images[[1]])
image(corners)
image(images[[1]])
image(images[[2]])
?diag

image(grad.x+grad.y)
image(grad.xy)
image(grad.r)


#zeros <- matrix(rep(0,100),nrow = 10)



sobel_x <- matrix(data = c(-1,-2, -1, 0, 0, 0, 1, 2, 1), nrow = 3)
sobel_y <- matrix(data = c(-1, 0, 1, -2, 0, 2, -1, 0, 1), nrow = 3)



    #grad.x[j,i] <- target_image[j,i]

image(grad.x)
image(grad.y)
image(grad.xy)


# XY Gradient **
for(i in 2:(x-1))
  for(j in 2:(y-1))
    grad.xy[j,i] <- (grad.x[j+1,i]-grad.x[j-1,i])/2



grad.yx <- matrix(rep(0, times = x*y), nrow = y)
# YX Gradient
for(i in 2:(x-1))
  for(j in 2:(y-1))
    grad.yx[j,i] <- (grad.y[j,i+1]-grad.y[j,i-1])/2

image(grad.yx)

    zeros[i,j] <- 1


image(images[[1]])
image(x = im_edge)
