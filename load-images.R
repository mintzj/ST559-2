#  Improved data loading ---------------------------
#  Use package EBImage to load files.


#  Load a folder via function
#  In:   folders_path
#        n_images        #number of images to load. (default 9)
#  OUt:  list containing the images.
load_folder <- function(folders_path, n_images = 9){
  require(EBImage)
  
  files <- list.files(path = folders_path, full.names = T)
  #  Use  n_images, or all files, whichever is smaller
  n_images  <- min(n_images, length(files));
  
  #  For the sake of speed and sanity we will use 
  #  only the first 9 images from each folder for now.
  
  # Create a list to hold the filenames.
  images <- vector("list", n_images)
  
  for (i in 1:n_images){
    images[[i]] <- as.matrix(readImage(files[i]))
  }
  return(images)
}

folders_path <- "Z:/ST599/Project2/train"

# Grab the folder names and paths.
folders.path <- list.files(path = folders_path, full.names = T)
folders.name <- list.files(path = folders_path, full.names = F)


#  Load up n images from specified folder.
images <- load_folder(folders.path[20], n_images = 9)
plot_images(images = images, folder_name = folders.name[20])

#image(images[[6]])
#folders.name[15]





#  plot_images()
#  In:  list of images, folder_number
#  Out:  ---
#  Plot 9x9 grids of all images

plot_images <- function(images, folder_name){
  #  Save your plot settings.
  def.par <- par(no.readonly = TRUE) 
  
  #  Divide the device into 3 rows and 3 columns
  layout(matrix(c(1:9), 3, 3, byrow = TRUE))
  
  for (i in 1:length(images)){
    image(images[[i]])
  }
  mtext(folder_name,
        side=3, line=1, font=2, cex=1, col='red')
  
  par(def.par)  #- reset to default plot settings. 
}








# Prevous version of loader. ---------------------------



#  Save your plot settings.
def.par <- par(no.readonly = TRUE) 

#  Install packages ---------------------------
#  You need to run these lines the first time in order to install EBImage.
#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
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




folders.path <- list.files(path = folders_path, full.names = T)
folders.name <- list.files(path = folders_path, full.names = F)

# Get the files within a folder (we fixed the first folder number 1 here.)
j <-  16
files <- list.files(path = folders.path[j], full.names = T)




im_out2 <- load_folder(folders.path[2], n_images = 10)
image(im_out2[[2]])



