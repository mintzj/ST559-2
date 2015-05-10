# All code in this preamble is entirely of Jeffrey's design.
# I've removed some commentary in this instance of the code,
# Since his design choices can be observed in the original code
# (load-images.R).

# Loader functions:

library(EBImage)

source(utility.R)

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
analyze.list <- function(imagelist) {
  v.leng <- length(imagelist); h.leng <- 9;   # EDIT h.leng AS MORE STATISTICS ARE FIGURED OUT!
  stats.mat <- as.data.frame(matrix(numeric(v.leng*h.leng), nrow=v.leng))
  for (i in 1:length(imagelist)) {
    matrow <- as.data.frame(numeric(h.leng))
    matrow[1] <- names(imagelist)[i]          # Likely unnecessary for training set
    matrow[2] <- as.character(substitute(imagelist))
    matrow[3] <- as.numeric(mean(imageData(imagelist[[i]])))
    matrow[4] <- var(as.numeric(imageData(imagelist[[i]])))
    matrow[5] <- dim(imageData(imagelist[[i]]))[1] / dim(imageData(imagelist[[i]]))[2]
    matrow[6] <- computeFeatures.moment(imagelist[[i]])[,1] / dim(imageData(imagelist[[i]]))[1]
    matrow[7] <- computeFeatures.moment(imagelist[[i]])[,2] / dim(imageData(imagelist[[i]]))[2]
    matrow[8] <- computeFeatures.moment(imagelist[[i]])[,4]
    matrow[9] <- computeFeatures.moment(imagelist[[i]])[,5]
    # ADD MORE STATISTICS HERE AS NECESSARY!
    
    stats.mat[i,] <- matrow
  }
  
  names(stats.mat) <- c("filename", "class", "mean ink", "ink variance", "aspect ratio", "std x moment",
                        "std y moment", "eccentricity", "theta")
  
  return(stats.mat)
}

# And now, we incorporate all summary statistics into one data frame for generation!

agg.stats <- NULL
for (i in folders.name) {
  agg.stats <- rbind(agg.stats, analyze.list(get(i)))
}

# And... this throws an error.  
  # Error in `[<-.data.frame`(`*tmp*`, 2, value = c("get", "i")) : 
  # replacement has 2 rows, data has 9 
# What's going on here?