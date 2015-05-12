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

# mc.error <- 1-(sum(diag(confus))/sum(confus))
# Not proper MC error; need to make sure all row names are included
# It's not sorting all rows!

confus.f <- as.data.frame(matrix(numeric(121*121), nrow=121))
colnames(confus.f) <- colnames(confus)
rownames(confus.f) <- t(colnames(confus))

confus.f[1,] <- confus[1,]
confus.f[11,] <- confus[2,]
confus.f[12,] <- confus[3,]
confus.f[26,] <- confus[4,]
confus.f[33,] <- confus[5,]
confus.f[37,] <- confus[6,]
confus.f[73,] <- confus[7,]
confus.f[86,] <- confus[8,]
confus.f[109,] <- confus[9,]
confus.f[111,] <- confus[10,]
confus.f[116,] <- confus[11,]

c.rate <- sum(diag(as.matrix(confus.f))) / sum(confus.f)
1 - c.rate  # Misclassifcation rate is 0.7902; this is pretty awful! AS for why.....

rownames(confus)
# All cross-validation

# confus.f gives us the confusion matrix for our first algorithm.
image(as.matrix(confus.f), col=gray((0:32)/32),
      main="Confusion matrix (black)", ylab="Observed",
      xlab="Predicted", x=1:121, y=1:121)

image(as.matrix(confus.f), col=gray((32:0)/32),
      main="Confusion matrix (white)", ylab="Observed",
      xlab="Predicted", x=1:121, y=1:121)