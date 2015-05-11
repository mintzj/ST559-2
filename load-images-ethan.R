# All code in this preamble is entirely of Jeffrey's design.
# I've removed some commentary in this instance of the code,
# Since his design choices can be observed in the original code
# (load-images.R).

# Loader functions:

#  You need to run these lines the first time in order to install EBImage.
#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
library(EBImage)
library(dplyr)

source("utility.R")

load_folder <- function(folders_path){
  require(EBImage)
  files <- list.files(path = folders_path, full.names = T)
  #  Use  n_images, or all files, whichever is smaller
  n_images  <- length(files);

  images <- vector("list", n_images)
  
  for (i in 1:n_images){
    images[[i]] <- as.matrix(readImage(files[i]))
  }
  
  names(images) <- list.files(folders_path)
  
  return(images)
}

folders_path <- "D:/Media/Documents/School/STAT599-1/ST559-2/train"

folders.path <- list.files(path = folders_path, full.names = T)
folders.name <- list.files(path = folders_path, full.names = F)

# images <- load_folder(folders.path[5], n_images = 200)
# plot_images(images = images, folder_name = folders.name[5])

# image(images[[6]])
# folders.name[15]


# So, from here, we need to load the entire training set and
# simultaneously construct a statistic matrix:

# all.images <- sapply(folders.path, load_folder, n_images=200)
  # This creates a 121 element, LARGE list; that's kind of a pain computationally.
  # Maybe creating separate lists? Using assign() and get():

# Load all images into lists:
for (i in 1:length(folders.name)) {
  assign(folders.name[i], load_folder(folders.path[i]))
}

# We now have 121 lists of manageable size. From here, we need to access
# each image in the list. Let's do this through a function, "analyze.list".

# analyze.list should take a given list of images, run EBImage on each one,
# and finally return a matrix/frame of statistics + assignment.
# analyze.list <- function(imagelist) {
#   v.leng <- length(imagelist); h.leng <- 9;   # EDIT h.leng AS MORE STATISTICS ARE FIGURED OUT!
#   stats.mat <- as.data.frame(matrix(numeric(v.leng*h.leng), nrow=v.leng))
#   names(stats.mat) <- c("filename", "class", "mean ink", "ink variance", "aspect ratio", "std x moment",
#                         "std y moment", "eccentricity", "theta")
#   
#   for (i in 1:length(imagelist)) {
#     stats.mat[i,1] <- names(imagelist)[i]          # Likely unnecessary for training set
#     stats.mat[i,2] <- as.character(substitute(imagelist))
#     stats.mat[i,3] <- as.numeric(mean(imageData(imagelist[[i]])))
#     stats.mat[i,4] <- var(as.numeric(imageData(imagelist[[i]])))
#     stats.mat[i,5] <- dim(imageData(imagelist[[i]]))[1] / dim(imageData(imagelist[[i]]))[2]
#     stats.mat[i,6] <- computeFeatures.moment(imagelist[[i]])[,1] / dim(imageData(imagelist[[i]]))[1]
#     stats.mat[i,7] <- computeFeatures.moment(imagelist[[i]])[,2] / dim(imageData(imagelist[[i]]))[2]
#     stats.mat[i,8] <- computeFeatures.moment(imagelist[[i]])[,4]
#     stats.mat[i,9] <- computeFeatures.moment(imagelist[[i]])[,5]
#     # ADD MORE STATISTICS HERE AS NECESSARY!
#   }  
#   return(stats.mat)
# }

# And now, we incorporate all summary statistics into one data frame for generation!

# agg.stats <- NULL
# for (i in folders.name) {
#   agg.stats <- rbind(agg.stats, analyze.list(get(i)))
# }

# And... this throws an error.  
# Further examination leads to the conclusion:
# Attempting to reference each list with "get(i)", where i is a character string
# with the variable name, is throwing the error.

# Perhaps it's trying to get(i) after analyze is called?
# Fixing:

# agg.stats <- NULL
# for (i in folders.name) {
#   worklist <- get(i)
#   agg.stats <- rbind(agg.stats, analyze.list(worklist))
# }

# And this WORKS! ...except for how it assigns class "worklist," not classname.
# So a band-aid workaround would be to pass in both "i" (the character string)
# and "worklist" (the list), right?
# Let's fix it to do this:

analyze.list <- function(imagelist, classname) {
  v.leng <- length(imagelist); h.leng <- 9;   # EDIT h.leng AS MORE STATISTICS ARE FIGURED OUT!
  stats.mat <- as.data.frame(matrix(numeric(v.leng*h.leng), nrow=v.leng))
  names(stats.mat) <- c("filename", "class", "mean ink", "ink variance", "aspect ratio", "std x moment",
                        "std y moment", "eccentricity", "theta")
  
  for (i in 1:length(imagelist)) {
    stats.mat[i,1] <- names(imagelist)[i]          # Likely unnecessary for training set
    stats.mat[i,2] <- classname
    stats.mat[i,3] <- as.numeric(mean(imageData(imagelist[[i]])))
    stats.mat[i,4] <- var(as.numeric(imageData(imagelist[[i]])))
    stats.mat[i,5] <- dim(imageData(imagelist[[i]]))[1] / dim(imageData(imagelist[[i]]))[2]
    stats.mat[i,6] <- computeFeatures.moment(imagelist[[i]])[,1] / dim(imageData(imagelist[[i]]))[1]
    stats.mat[i,7] <- computeFeatures.moment(imagelist[[i]])[,2] / dim(imageData(imagelist[[i]]))[2]
    stats.mat[i,8] <- computeFeatures.moment(imagelist[[i]])[,4]
    stats.mat[i,9] <- computeFeatures.moment(imagelist[[i]])[,5]
    # ADD MORE STATISTICS HERE AS NECESSARY!
  }  
  return(stats.mat)
}

agg.stats <- NULL

agg.time <- proc.time()
for (i in folders.name) {
  worklist <- get(i)
  agg.stats <- rbind(agg.stats, analyze.list(worklist, i))
}
proc.time() - agg.time

# We are given (after 17m, 56s) 30336 objects on 9 variables!
# Let's use dplyr to summarize them:

sum.stats <- (agg.stats[,-1] %>% group_by(class)) %>% summarise_each(funs(mean))

# With this, we have our data in the basic form necessary. Random forest method to follow;
# from here, accuracy can best be improved in two ways:
#
#   1. Expanding data scope with rotations/transformations
#     (as a side note, this renders std x moment and std y moment less useful?)
#     (maybe include "deviation from center" instead)
#
#   2. Finding more invariant statistics
#     Right now, we have very few "strong" statistics; the best one we have is mean ink,
#     Which really isn't sufficient with 121 classes, and aspect ratio, which is rotation-variant.
#     (Maybe same solution with x/y moment; "deviation from 1" ?)