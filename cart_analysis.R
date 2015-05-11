library(dplyr)
library(rpart)
library(adabag)

# Make sure your working directory is set to the source file location!

# Read in the data:
agg.stats <- read.csv("aggstats.csv")[,-1]

# So, from here, let's divide the data into a training set and 
# a cross-validation set. (We could just run on the test set,
# and see what sort of answers that gives us, but we still aren't
# sure what format our submissions need to be in.)

xval <- sample_frac(agg.stats, 0.2)
xtrain.pre <- rbind(agg.stats, xval)
xtrain <- xtrain.pre[!duplicated(xtrain.pre,fromLAST = FALSE)&!duplicated(xtrain.pre,fromLast = TRUE),]

# Done. We now have an 80-20 split, with 80% training set and 20% cross-validation!
# From here, we run our bagging algorithm on the training set:

bag.time <- proc.time()
bag.init <- bagging(class~.,
                    data=xtrain,
                    mfinal=50,
                    control=rpart.control(xval=10,cp=0.005))
proc.time() - bag.time

# how to look at it?
names(bag.init)
bag.init$importance

bag.0.pred <- predict(bag.init, newdata=xval,
                      newmfinal=length(bag.init$trees))
confus <- bag.0.pred$confusion
write.csv(confus, "bag_confusion.csv", na="NA")

mc.error <- 1-(sum(diag(confus))/sum(confus))
# Not proper MC error; need to make sure all row names are included
# It's not sorting all rows!
# I can add this in after my 2pm class
