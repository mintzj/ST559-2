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


# Old version of corner count ---------------------------


corner_count  <- function(target_image, n = 1){
  #  create nxn matrix of zeros
  corner_mat <- matrix(rep(0, n*n), nrow = n)
  
  #  dimensions of target image
  target_image <- imageData(target_image)
  img_dim <- dim(target_image)
  
  #  floor to find number of pixels per x, pixels per y division
  pix_per <- floor(img_dim/n)
  #pix_per <- (img_dim/n)
  
  #  for each element in the nxn matrix
  for(i in 1:n)
    for(j in 1:n)
      corner_mat[j,i] <- sum(target_image[
        ( (j-1)*pix_per[1] ):( j*pix_per[1]), 
        ( (i-1)*pix_per[2] ):( i*pix_per[2])
        ])
  return(corner_mat)
}



#  Testing:
system.time(image(images[[1]]))
system.time(corner_detect(target_image = amphipods[[1]]))
corners_1 <- corner_detect(target_image = amphipods[[2]])

#  Bytecode compiled:
corners_1 <- byte_corner(target_image = images[[1]])
system.time(byte_corner(target_image = images[[1]]))

corners_1 <- byte_corner(target_image = images[[1]])
system.time(bc2(target_image = images[[1]]))


image(corners_1)
image(images[[1]])


count_1 <- (corner_count(target_image = corners_1, n = 10))

?system.time
image(count_1)
image(corners_1)
image(tunicate_salp[[1]])

# detect corners on thresh'd image ---------------------------
high_spots <- px_thresh(tunicate_salp[[5]], thresh = 0.001)
high_spots <- px_thresh(copepod_calanoid_large[[5]], thresh = 0.001)
corners_2 <- corner_detect(high_spots)
count_2 <- corner_count(target_image = corners_2, n = 10)
image(high_spots)
image(corners_2)
image(count_2)
