library(e1071)
library(doParallel)
library(tictoc)

BaggingSVM_Lin <- function(train, test, seed, size, cost){

registerDoParallel(makeCluster(3) -> cpu) 
tic()
set.seed(seed)  # for reproducibility

predictions <- foreach(
	icount(4), 
	.packages = "e1071", 
	.combine = cbind
) %dopar% {
	# bootstrap copy of training data
	index<- sample.int(n=nrow(train), 
										 size = floor(size*nrow(train)),
										 replace = TRUE)
	sample <- train[index, ]  
	
	# fit model to bootstrap copy
	bagged_svm <- svm(factor(attrition)~., 
										data=sample, 
										kernel ="linear",
										probability=TRUE,
										cost =cost)
	as.data.frame(attr(
		predict(bagged_svm,
						newdata=test, 
						probability=TRUE), "prob"))$Yes
}
toc() #3 sec (4)
stopCluster(cpu)

return(rowMeans(predictions))

}
