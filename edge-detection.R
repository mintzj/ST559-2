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

