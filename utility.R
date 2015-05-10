# Utility functions ---------------------------
#  These functions should interface with Ethan's CART analysis.

library(EBImage)


#  Name:  corner-detect.R
#  In:    EBImage file
#  Out:   Matrix (filter applied to highlight a few “pointy” spots)

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



#  Name:     corner-count.R
#  In:         EBImage file, n = 3 (n x n grid)
#  Out:     n x n matrix containing # of “corners” in each part of the grid.

corner_count  <- function(target_image, n = 1){
  #  create nxn matrix of zeros
  corner_mat <- matrix(rep(0, n*n), nrow = n)
  
  #  dimensions of target image
  target_image <- imageData(target_image)
  img_dim <- dim(target_image)
  
  #  floor to find number of pixels per x, pixels per y division
  pix_per <- (img_dim/n)
  #pix_per <- (img_dim/n)
  
  #  for each element in the nxn matrix
  for(i in 1:n)
    for(j in 1:n)
      corner_mat[j,i] <- sum(target_image[
        ( floor((j-1)*pix_per[1]) ):( floor(j*pix_per[1]) ), 
        ( floor((i-1)*pix_per[2]) ):( floor(i*pix_per[2]) )
        ])
  return(corner_mat)
}



# Detect Edges ---------------------------
#  Create a function find the edges via 3 methods.
#  In:  image_target (in format of EBImage)
#       edge_type    (type of detection:  Laplacian, Sobel, Robertcross)
#  Out: edges (in format of EBImage)
edge_detect  <- function (image_target, edge_type = "Sobel"){
  require(adimpro)
  require(EBImage)
  adim_img  <- make.image(image_target)
  adim_edges <- edges(img = adim_img, type = edge_type)
  edges <- imageData<-(y = extract.image(adim_edges) )
  return(edges)
}


# Threshold ---------------------------
#  Use only pixels Above Quantile, else 0.
#  In:  target_image (EBImage file)
#       thresh  (threshold, % of pixels above which we keep)
#  Out: matrix
px_thresh <- function(target_image, thresh = 0.05){
  fish <- as.vector(imageData(target_image))
  fish_quantile  <- quantile(fish[fish<1],thresh)
  paste(fish_quantile)
  high <-ifelse(fish>quantile(fish,fish_quantile),1,0)
  high <- matrix(high, nrow = dim(target_image)[1])
  return(high)
}


# Modified image loader for ethan's functions. ---------------------------
mini_loader <- function(folders_path, test = F){
  require(EBImage)
  files <- list.files(path = folders_path, full.names = T)
  #  Default:  Load all images in folder
  #  If test = T, load first 5 images from each folder
  
  n_images  <- length(files);
  if (test == T)
    n_images  <- min(5, n_images)
  
  images <- vector("list", n_images)
  
  for (i in 1:n_images){
    images[[i]] <- as.matrix(readImage(files[i]))
  }
  
  names(images) <- list.files(folders_path)[1:n_images]
  
  return(images)
}

