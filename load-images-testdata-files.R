library(EBImage)
library(dplyr)

source("utility.R")



load_bits <- function(folders_path, n_start, n_end){
  #message(paste(n_start, " ", n_end))
  require(EBImage)

  n_images  <- n_end - n_start + 1;
  
  images <- vector("list", n_images)
  
  for (i in 1:n_images){
    images[[i]] <- as.matrix(readImage(files[n_start + i - 1]))
    #message(paste("reading: ",(n_start + i - 1), " ", files[n_start + i - 1]))
  }
  
  names(images) <- files[n_start:n_end]
  #message(paste(files[n_start:n_end], sep = ", "))
  
  return(images)
}

analyze.list <- function(imagelist) {
  v.leng <- length(imagelist); h.leng <- 14;   # EDIT h.leng AS MORE STATISTICS ARE FIGURED OUT!
  stats.mat <- as.data.frame(matrix(numeric(v.leng*h.leng), nrow=v.leng))
  names(stats.mat) <- c("filename", "ink.mean", "ink.var", "aspect.ratio", 
                        "d.center", "eccentricity", "theta",  "x.moment", "y.moment",
                        "R2", "Centrality","left_right","line","corners")  

  #test <- proc.time()
  for (i in 1:length(imagelist)) {
    #test <- proc.time()
    features <- (computeFeatures.moment(imagelist[[i]]))
    if(is.null(features)==T){
       features <- matrix(rep(0,5), nrow = 1)
    }
    
    img_dat <- imageData(imagelist[[i]])
    x <- dim(img_dat)[2]
    y <- dim(img_dat)[1]
    #stats.mat[i,1] <- names(imagelist)[i]          # Likely unnecessary for training set
    stats.mat[i,2] <- as.numeric(mean(img_dat))
    stats.mat[i,3] <- var(as.numeric(img_dat))
    stats.mat[i,4] <- dim(imageData(imagelist[[i]]))[1] / x
    stats.mat[i,5] <- sqrt((features[,1] / y)^2 + (features[,2] / x)^2)
    stats.mat[i,6] <- features[,4]
    stats.mat[i,7] <- features[,5]
    stats.mat[i,8] <- features[,1] / y
    stats.mat[i,9] <- features[,2] / x
    stats.mat[i,10] <- rvar.lm(imagelist[[i]])
    stats.mat[i,11] <- rvar.centrality(imagelist[[i]])
    stats.mat[i,12] <- rvar.left_right(imagelist[[i]])
    stats.mat[i,13] <- rvar.line(imagelist[[i]])
    stats.mat[i,14] <- rvar.corners(imagelist[[i]])    
    #proc.time() - test
    # ADD MORE STATISTICS HERE AS NECESSARY!
  }  
  #proc.time() - test

  return(stats.mat)
}

#  folders_path  <- "Z:/ST599/Project2/test/test"
folders_path <- "D:/Media/Documents/School/STAT599-1/ST559-2/test"
files <- list.files(path = folders_path, full.names = T)

# test <- proc.time();
# test2 <- load_bits(folders_path, 1, 2500);
# proc.time() - test
#
# Runtime: 7.37
#
# test <- proc.time();
# test2 <- load_bits(folders_path, 1, 5000);
# proc.time() - test
#
# Runtime: 12.06
#
# test <- proc.time();
# test2 <- load_bits(folders_path, 1, 10000);
# proc.time() - test
#
# Runtime: 22.58, including all the time it took to render a list of 10000 fully.
#
# Not bad, not bad! This version should help avoid nasty RAM issues.
# So from here, we want to load in "the bits."
# Since each doubling of image size is corresponding to slightly *less* than a doubling
# of computation time, we set our sights on the 10000 mark (allowing for fewer iterations,
# but also clear knowledge if the process halts).
#
# While we're here, let's also makes sure the analysis works on an imagelist of size 10000:
#

test <- proc.time()
test4<- analyze.list(load_bits(folders_path, 5001, 6000))
proc.time() - test

test6 <- analyze.list(load_bits(folders_path, 6650, 6770))
#
# Runtime: 23 seconds
# PERFECT- except not, actually.
# It's running an error in rotate.
# bg.col=white; what's going on here?








file_vector <- as.vector(files)
no_images <- length(files)
#no_images <- 105
# 130400, in our case.
lsize <- 10000
#lsize <- 100
# We set each list load to be 10000.

agg.test <- NULL

agg.time <- proc.time()
#for (i in 1:ceiling(no_images/lsize)) {
for (i in 1:ceiling(no_images/lsize)) {
  agg.test <- NULL
  # Worklist holds images.
  worklist <- load_bits(folders_path, ((i-1)*lsize)+1, min(i*lsize,no_images))
  # Bindlist holds summaries of each element in worklist.
  bindlist <- analyze.list(worklist)
  
  #rbind(agg.test, worklist)
  agg.test <- rbind(agg.test, bindlist)
  write.csv(agg.test, paste("aggstats_part",i,".csv", sep = ""), na="NA", row.names=FALSE)
  
}
proc.time() - agg.time

agg.test <- NULL

#  Read back the data.
for (i in 1:ceiling(no_images/lsize)){
  read_file <- read.csv(paste("aggstats_part",i,".csv", sep = ""))#[,-1]
  agg.test <- rbind(agg.test, read_file)
}

#  combine all the data into one saved file.
#dim(agg.test)
#write.csv(agg.test, "aggstats_combined.csv", na="NA", row.names=FALSE)


# add the filenames.
test_stats <- agg.test
test_stats[,1] <- files
#head(test_stats)
write.csv(test_stats, "aggstats_full.csv", na="NA", row.names=FALSE)
