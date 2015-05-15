# Utility functions ---------------------------
#  These functions should interface with Ethan's CART analysis.

library(EBImage)
library(MASS)


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
px_thresh <- function(target_image, thresh = 0.95){
  fish <- 1-as.vector(imageData(target_image))
  fish_quantile  <- quantile(fish[fish>0],thresh)
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

#  Convert image into points, fit a linear model.
#  in:  target_image (EBImage type)
#  out:  linear model
image_lm <- function(target_image, thresh = 0.95, plots = F, robust = F){
  lrt <- px_thresh(target_image, thresh)
  lrt_dim <- dim(lrt)
  x <- 1:lrt_dim[2]
  y <- 1:lrt_dim[1]
  lrt_vec <- as.vector(lrt)
  x_mat <- lrt_vec*rep(y,lrt_dim[2])
  y_mat <- lrt_vec*rep(1:lrt_dim[1], each=lrt_dim[2])
  
  if (robust == T){
    #message("Robust Regression Enabled")
    hydro_lm <- tryCatch(expr = ltsreg(x_mat[x_mat>0]~y_mat[x_mat>0]),
                         error = function(cond){
                           hydro_lm <- vector(mode = "list", length = 2)
                           hydro_lm$r.squared  <-  0
                           return(hydro_lm)
                         })
    }
    if (robust == F){
    hydro_lm <- tryCatch(expr = lm(x_mat[x_mat>0]~y_mat[x_mat>0]), 
                         error = function(cond){
                           #message("An image was singular:")
                           #message(deparse(substitute(target_image)))
                           # message(cond)
                           #hydro_lm <- c(r.squared = 0)
                           #plots <- F  #something is wrong with the scope of this, so don't use it till its fixed.
                           hydro_lm <- vector(mode = "list", length = 2)
                           hydro_lm$r.squared  <-  0
                           return(hydro_lm)
                         })  #return(c(r.squared = 0)))
    
  }


  if (plots == T){
    image(target_image)
    image(lrt)
    #  Cool Gradient:
    image(matrix(x_mat, nrow = dim(lrt)[1]))
    image(matrix(y_mat, nrow = dim(lrt)[1]))
    plot(x = x_mat[x_mat>0], y = y_mat[x_mat>0])
    abline(hydro_lm)
  }  # Just for plotting.

  return(hydro_lm)
}

# Orient Image ---------------------------
#  in: EBImage
#  out:  EBImage
orient_image <- function(target_image, robust = F){
  rocket_lm <- image_lm(target_image, robust = robust)
  if (is.numeric(rocket_lm$coefficients[[2]])==F)
    rocket_angle <- 0
  if (is.numeric(rocket_lm$coefficients[[2]])==T)
    rocket_angle <- atan(x = rocket_lm$coefficients[[2]])*180/pi
  
  origin <- computeFeatures.moment(target_image)[1:2]
  rocket_rotate <- rotate(target_image,angle = -rocket_angle, bg.col = "white")
  return(rocket_rotate)
  
}

# 
#  Find the percentatge of pixels in the middle part of the image.  Take the center, and add ~25% on each side.
rvar.centrality <- function(target_image, n = 5, robust = F){
  rotated <- orient_image(target_image, robust = robust)
  rotated_threshed <- px_thresh(target_image = rotated, thresh = 0.999)
  count_rotated_threshed <- corner_count(rotated_threshed, n = n)
  center <- ceiling(n+1)/2
  top <- center + floor(n/4)
  bottom <- center - floor(n/4)
  #message(paste(bottom, " ", center, " ", top))
  ratio <- sum(count_rotated_threshed[bottom:top,bottom:top])/sum(count_rotated_threshed)
  if(is.nan(ratio)==T)
    ratio <- 0  
  if(is.infinite(ratio)==T)
    ratio <- 0
  return(ratio)  
}

#  Find the percentage of pixels in the (n+1)/2 row (the center row) of an image, after rotation
rvar.line <- function(target_image, n = 5){
    rotated <- orient_image(target_image, robust = F)
  rotated_threshed <- px_thresh(target_image = rotated, thresh = 0.999)
  count_rotated_threshed <- corner_count(rotated_threshed, n = 5)
  ratio <- sum(count_rotated_threshed[,ceiling((n+1)/2)]/sum(count_rotated_threshed))
  if(is.nan(ratio)==T)
    ratio <- 0  
  return(ratio)
}

# Ratio of left to right.
rvar.left_right <- function(target_image, n = 5){
  rotated <- orient_image(target_image, robust = robust)
  rotated_threshed <- px_thresh(target_image = rotated, thresh = 0.999)
  count_rotated_threshed <- corner_count(rotated_threshed, n = n)
  dim_max <- dim(count_rotated_threshed)[2]
  center <- ceiling(n+1)/2
  left <- 1+floor(n/4)
  right <- dim_max - floor(n/4)
  #indices <- c(1:left,right:dim_max)
  #message(paste(bottom, " ", center, " ", top))
  ratio <- sum(count_rotated_threshed[1:left,])/sum(count_rotated_threshed[right:dim_max,])
  if(is.nan(ratio)==T)
    ratio <- 0
  if(is.infinite(ratio)==T)
    ratio <- 0
  return(ratio)  
  
}

# ratio of corners to center
rvar.corners <- function(target_image, n = 5){
  rotated <- orient_image(target_image, robust = robust)
  rotated_threshed <- px_thresh(target_image = rotated, thresh = 0.999)
  count_rotated_threshed <- corner_count(rotated_threshed, n = n)
  dim_max <- dim(count_rotated_threshed)[2]
  center <- ceiling(n+1)/2
  left <- 1+floor(n/4)
  right <- dim_max - floor(n/4)
  indices <- c(1:left,right:dim_max)
  ratio <- sum(count_rotated_threshed[indices,indices])/sum(count_rotated_threshed)
  if(is.nan(ratio)==T)
    ratio <- 0  
  return(ratio)
}

# Descriptive Statistic:  Linear Model ---------------------------
#  in: EBImage
#  out:  R2 value
rvar.lm <- function(target_image){
  rocket_lm <- image_lm(target_image = target_image)
  #  rocket_summary <- summary(rocket_lm)$r.squared
  rocket_summary <- tryCatch(expr = summary(rocket_lm)$r.squared, 
                             error = function(cond){
                               return(0)
                             })

  return(rocket_summary)
}

