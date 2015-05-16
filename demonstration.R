library(EBImage)
# store your plots setup
def.par <- par(no.readonly = TRUE) 

#  Choose your folder path for test and training images.
#  test_folders_path <- "D:/Media/Documents/School/STAT599-1/ST559-2/test"
  test_folders_path  <- "Z:/ST599/Project2/test/test"

#  training_folders_path <- "D:/Media/Documents/School/STAT599-1/ST559-2/train"
  training_folders_path  <- "Z:/ST599/Project2/train"

test_files <- list.files(path = test_folders_path, full.names = T)
training_folders <- list.files(path = training_folders_path, full.names = T)

show_n <- 10  # number of test images to show.
plankto_pred <- as.vector(rf.pred)
sample_pred <- sample.int(n = length(rf.pred), size = show_n,  replace = F, )
plankto_pred <- cbind(n = sample_pred, type = plankto_pred[sample_pred], filname = test_files[sample_pred])

#  Layout:  one plot, whole left side.
layout(matrix(c(1,1,2,3,1,1,4,5), nrow = 2, ncol = 4, byrow = TRUE))
# show the regions that have been allocated to each plot
#layout.show(5)
par(oma=c(0,0,2,0));

for(i in 1:show_n){
  image(readImage(plankto_pred[i,3]))
  training_files <- list.files(path = paste(training_folders_path,plankto_pred[i,2], sep = "/"), full.names = T)
  folder_size  <-length(training_files)
  #  Pick 4 files from the folder.
  pick_files <- sample.int(n = folder_size, size = min(4, folder_size), replace = F)
  training_sample <- training_files[pick_files]
  for(f in training_sample)
    image(readImage(files = f))
  mtext(plankto_pred[i,2],outer=TRUE,line=-1, cex = 2)
}


par(def.par)  #- reset to default

