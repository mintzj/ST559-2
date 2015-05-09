#  Edge Detection ---------------------------
#  Use package adimpro for edge detection. 
#   (Includes filters Laplacan, Sobel, Robert Cross)
#   Does *not* include an implementation of Gabor wavelet filter.
#  If we want, we may be able to do our own, but there is a starting
#   point in the package "grt", which was originally for matlab, but
#   was ported to R.


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

image(images[[1]])
im_edge <- edge_detect(images[[1]], edge_type = "Robertcross")
image(x = im_edge)
im_edge2 <- edge_detect(im_edge, edge_type = "Robertcross")
image(x = im_edge2)
im_edge3 <- edge_detect(im_edge2, edge_type = "Robertcross")
image(im_edge3)
im_edge4 <- edge_detect(im_edge3, edge_type = "Robertcross")
image(im_edge4)





