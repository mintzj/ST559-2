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
  v.leng <- length(imagelist); h.leng <- 9;   # EDIT h.leng AS MORE STATISTICS ARE FIGURED OUT!
  stats.mat <- as.data.frame(matrix(numeric(v.leng*h.leng), nrow=v.leng))
  names(stats.mat) <- c("filename", "ink.mean", "ink.var", "aspect.ratio", 
                        "R2", "Centrality","left_right","line","corners")
  #  names(stats.mat) <- c("filename", "class", "ink.mean", "ink.var", "aspect.ratio", 
  #                      "d.center", "eccentricity", "theta",  "x.moment", "y.moment")
  
  for (i in 1:length(imagelist)) {
    stats.mat[i,1] <- names(imagelist)[i]          # Likely unnecessary for training set
    stats.mat[i,2] <- as.numeric(mean(imageData(imagelist[[i]])))
    stats.mat[i,3] <- var(as.numeric(imageData(imagelist[[i]])))
    stats.mat[i,4] <- dim(imageData(imagelist[[i]]))[1] / dim(imageData(imagelist[[i]]))[2]
    <<<<<<< HEAD
    stats.mat[i,5] <- sqrt((computeFeatures.moment(imagelist[[i]])[,1] / dim(imageData(imagelist[[i]]))[1])^2 +
                             (computeFeatures.moment(imagelist[[i]])[,2] / dim(imageData(imagelist[[i]]))[2])^2)
    stats.mat[i,6] <- computeFeatures.moment(imagelist[[i]])[,4]
    stats.mat[i,7] <- computeFeatures.moment(imagelist[[i]])[,5]
    stats.mat[i,8] <- computeFeatures.moment(imagelist[[i]])[,1] / dim(imageData(imagelist[[i]]))[1]
    stats.mat[i,9] <- computeFeatures.moment(imagelist[[i]])[,2] / dim(imageData(imagelist[[i]]))[2]
    stats.mat[i,10] <- rvar.lm(imagelist[[i]])
    stats.mat[i,11] <- rvar.centrality(imagelist[[i]])
    stats.mat[i,12] <- rvar.left_right(imagelist[[i]])
    stats.mat[i,13] <- rvar.line(imagelist[[i]])
    stats.mat[i,14] <- rvar.corners(imagelist[[i]])    
    =======
      stats.mat[i,5] <- rvar.lm(imagelist[[i]])
    stats.mat[i,6] <- rvar.centrality(imagelist[[i]])
    stats.mat[i,7] <- log(rvar.left_right(imagelist[[i]]))
    stats.mat[i,8] <- rvar.line(imagelist[[i]])
    stats.mat[i,9] <- rvar.corners(imagelist[[i]])    
    >>>>>>> 0a440e08492fa07d51464e290cbf92b0793897f8
    
    # ADD MORE STATISTICS HERE AS NECESSARY!
  }  
  return(stats.mat)
}

#  folders_path  <- "Z:/ST599/Project2/test/test"
folders_path <- "D:/Media/Documents/School/STAT599-1/ST559-2/test"
<<<<<<< HEAD
files <- list.files(path = folders_path, full.names = T)
=======
  robust = F
>>>>>>> 0a440e08492fa07d51464e290cbf92b0793897f8

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
<<<<<<< HEAD
test4<- analyze.list(load_bits(folders_path, 5001, 6000))
=======
  test2 <- analyze.list(load_bits(folders_path, 1, 2))
>>>>>>> 0a440e08492fa07d51464e290cbf92b0793897f8
proc.time() - test

test6 <- analyze.list(load_bits(folders_path, 6650, 6770))
#
# Runtime: 23 seconds
# PERFECT- except not, actually.
# It's running an error in rotate.
# bg.col=white; what's going on here?








file_vector <- as.vector(files)
#no_images <- length(files)
no_images <- 105
# 130400, in our case.
#lsize <- 10000
lsize <- 100
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
  <<<<<<< HEAD
  
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
View(agg.test)




=======
  path <- paste("test",(i-1)*10000 + 1,sep='')
assign(path,bindlist)
}
proc.time() - agg.time
# Takes 9018s, but it's done!
# We can now export the files, through write.csv -

# Or we would be able to. It bricked R last night.
# Splitting and trying again.
# With the split functional, we write our data:

write.time <- proc.time()
for (i in 1:ceiling(no_images/lsize)) {
  path <- paste("test",(i-1)*10000 + 1,sep='')
  write.csv(get(path), paste(path, ".csv",sep=''), na="NA", row.names=FALSE)
}
proc.time() - write.time

# 20 seconds - much more reasonable.
# And with that, our data is created!
>>>>>>> 0a440e08492fa07d51464e290cbf92b0793897f8
