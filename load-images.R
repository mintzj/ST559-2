#  Improved data loading ---------------------------
#  Use package EBImage to load files.

#  Save your plot settings.
def.par <- par(no.readonly = TRUE) 

#  Install packages ---------------------------
#  You need to run these lines the first time in order to install EBImage.
source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(EBImage)
#browseVignettes(package = "EBImage")


#  Load file names ---------------------------
training <- "Z:/ST599/Project2/train"

# Get the folder names and locations
folders.path <- list.files(path = training, full.names = T)
folders.name <- list.files(path = training, full.names = F)

# Get the files within a folder (we fixed the first folder number 1 here.)
j <-  16
files <- list.files(path = folders.path[j], full.names = T)


#  For the sake of speed and sanity we will use 
#  only the first 10 images from each folder for now.

image_max <- 9 # length(x = files)

# Create a list to hold the filenames.
images <- vector("list", image_max)

#  Divide the device into 3 rows and 3 columns
layout(matrix(c(1:9), 3, 3, byrow = TRUE))


for (i in 1:image_max){
  images[[i]] <- as.matrix(readImage(files[i]))
  image(images[[i]])
}
mtext(folders.name[j],
      side=3, line=1, font=2, cex=1, col='red')

par(def.par)  #- reset to default plot settings.


# To do:  Load a folder via function
#  Have an argument to create plots.
function load_folder(){
  
  
}







