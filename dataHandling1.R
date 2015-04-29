# Version of image training ---------------------------
# (modified from Sharm's example)

#install.packages('jpeg')
library(jpeg)

#  Set the location of the 'working directory'... 
#  (your training images folder)
training <- "Z:/ST599/Project2/train"
setwd(training)

train.class.folders = substring(list.dirs(),3)[-1]
train.data = NULL
response = NULL
for(i in 1:length(train.class.folders)) {
	setwd(paste(training, train.class.folders[i], sep = ""))
	data.file.names = list.files()
	train.data = c(train.data, sapply(data.file.names, function(x) {readJPEG(x, native=T)}))
	response = c(response, rep(train.class.folders[i], length(data.file.names)))
}

setwd(training)
test.data = NULL
data.file.names = list.files()
test.data = sapply(data.file.names, function(x) {readJPEG(x, native=T)})

# Reading an image
#  This is just a first attempt to read an image.
#  Eventually we need to read the number of files and index through them.
first_img <- readJPEG(paste(training,"/",data.file.names[1],"/64.jpg",sep = ""), native=T)
plot(1, type="n", xlim=c(0, dim(train.data[[i]])[1]), ylim=c(0, dim(train.data[[i]])[2]))
dim(first_img)
plot(1, type = "n", xlim = c(0, dim(first_img)[1]), ylim = c(0, dim(first_img)[2]))
rasterImage(image = first_img,xleft = 0, ybottom = 0, xright = dim(first_img)[1], ytop = dim(first_img)[2])
#rasterImage(first_img, 0, 0, first_img[1],first_img[2])

#Working!
i=100
plot(1, type="n", xlim=c(0, dim(train.data[[i]])[1]), ylim=c(0, dim(train.data[[i]])[2]))
rasterImage(train.data[[i]], 0, 0, dim(train.data[[i]])[1], dim(train.data[[i]])[2])
#data.train1a = (train.data[[1]] - min(train.data[[i]]))/min(train.data[[1]])



# Based on example by Sharmodeep ---------------------------


library(jpeg)
#setwd("/Users/sharmodeep/Documents/study/Teaching/Stat599/Kaggle/train")

train.class.folders = substring(list.dirs(),3)[-1]
train.data = NULL
response = NULL
for(i in 1:length(train.class.folders)) {
  setwd(paste("/Users/sharmodeep/Documents/study/Teaching/Stat599/Kaggle/train/", train.class.folders[i], sep = ""))
  data.file.names = list.files()
  train.data = c(train.data, sapply(data.file.names, function(x) {readJPEG(x, native=T)}))
  response = c(response, rep(train.class.folders[i], length(data.file.names)))
}

setwd("/Users/sharmodeep/Documents/study/Teaching/Stat599/Kaggle/test")
test.data = NULL
data.file.names = list.files()
test.data = sapply(data.file.names, function(x) {readJPEG(x, native=T)})

i=100
plot(1, type="n", xlim=c(0, dim(train.data[[i]])[1]), ylim=c(0, dim(train.data[[i]])[2]))
rasterImage(train.data[[i]], 0, 0, dim(train.data[[i]])[1], dim(train.data[[i]])[2])
#data.train1a = (train.data[[1]] - min(train.data[[i]]))/min(train.data[[1]])

i=100
plot(1, type="n", xlim=c(0, dim(test.data[[i]])[1]), ylim=c(0, dim(test.data[[i]])[2]))
rasterImage(test.data[[i]], 0, 0, dim(test.data[[i]])[1], dim(test.data[[i]])[2])
#data.test1a = (data.test1 - min(data.test1))/min(data.test1)
