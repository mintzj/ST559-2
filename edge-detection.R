#  Edge Detection ---------------------------
#  Use package adimpro for edge detection. 
#   (Includes filters Laplacan, Sobel, Robert Cross)
#   Does *not* include an implementation of Gabor wavelet filter.
#  If we want, we may be able to do our own, but there is a starting
#   point in the package "grt", which was originally for matlab, but
#   was ported to R.

#install.packages("adimpro")
library(adimpro)
#  NOTE:  This masks "combine" function from EBImage

images2 <- make.image(images[[1]])
show.image(images2)
edges <- edges(images2, type="Laplacian")
show.image(edges)
edges <- edges(images2, type="Sobel")
show.image(edges)
edges <- edges(images2, type="Robertcross")
show.image(edges)

#  Create a function to take in a matrix (or EBImage file?)
#  Output a matrix representing the edges.

function edge_detect(){
  
  
}