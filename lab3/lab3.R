# You can write R code here and then click "Run" to run it on our platform

library(readr)
install.packages("NeuralNetTools")
library(NeuralNetTools)

train <- read.csv("./train.csv")
test <- read.csv("./test.csv")

# Write to the log:
cat(sprintf("Training set has %d rows and %d columns\n", nrow(train), ncol(train)))
cat(sprintf("Test set has %d rows and %d columns\n", nrow(test), ncol(test)))

X <- train[,-1]
Y <- train[,1]
trainlabel <- train[,1]
#Reducing Train using PCA
Xreduced <- X/255
Xcov <- cov(Xreduced)
pcaX <- prcomp(Xcov)
# Creating a datatable to store and plot the
# No of Principal Components vs Cumulative Variance Explained
vexplained <- as.data.frame(pcaX$sdev^2/sum(pcaX$sdev^2))
vexplained <- cbind(c(1:784),vexplained,cumsum(vexplained[,1]))
colnames(vexplained) <- c("No_of_Principal_Components","Individual_Variance_Explained","Cumulative_Variance_Explained")
#Plotting the curve using the datatable obtained
plot(vexplained$No_of_Principal_Components,vexplained$Cumulative_Variance_Explained, xlim = c(0,100),type='b',pch=16,xlab = "Principal Componets",ylab = "Cumulative Variance Explained",main = 'Principal Components vs Cumulative Variance Explained')
#Datatable to store the summary of the datatable obtained
vexplainedsummary <- vexplained[seq(0,100,5),]
vexplainedsummary
#Storing the vexplainedsummary datatable in png format for future reference.
library(gridExtra)
png("datatablevaraince explained.png",height = 800,width =1000)
p <-tableGrob(vexplainedsummary)
grid.arrange(p)
dev.off()
Xfinal <- as.matrix(Xreduced) %*% pcaX$rotation[,1:45]

#Making training labels as factors
trainlabel <- as.factor(trainlabel)
library(nnet)
Y <- class.ind(Y)
print(X[1:5,1:5])
print(Y[1:5,])

#We choose no_of_nodes=150 and maxiter=100 (change it as a trade-off between running time and accuracy)

#Training the nnet on totat_training_set
finalseed <- 150
set.seed(finalseed)
model_final <- nnet(Xfinal,Y,size=150,softmax=TRUE,maxit=130,MaxNWts = 80000)

plotnet(model_final)

#Load test to reduced and normalize it for predictions
testlabel <- as.factor(test[,1])

#Applying PCA to test set
testreduced <- test/255
testfinal <- as.matrix(testreduced) %*%  pcaX$rotation[,1:45]

#Calculating Final Accuracies
prediction <- predict(model_final,testfinal,type="class")
prediction <- as.data.frame(prediction)
finalprediction<- cbind(as.data.frame(1:nrow(prediction)),prediction)
colnames(finalprediction) <- c("ImageId","Label")
write.csv(finalprediction,file="predictions.csv",row.names=FALSE)