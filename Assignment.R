setwd("C:\\Workspaces\\workspace_R\\predmachlearn")

library(ElemStatLearn, quietly=TRUE)
library(data.table, quietly=TRUE)
library(randomForest, quietly=TRUE)
library(caret, quietly=TRUE)
library(tree, quietly=TRUE)

set.seed(222)

training <- read.csv("pml-training.csv", na.strings=c("", "NA", "NULL"))
testingIn <- read.csv("pml-testing.csv", na.strings=c("", "NA", "NULL"))

trainingClean <- training[ , colSums(is.na(training)) == 0]
testingInClean <- testingIn[ , colSums(is.na(testingIn)) == 0]

dim(trainingClean)
dim(testingInClean)

removeCols = c('X', 'user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window', 'num_window')
trainingClean <- trainingClean[, -which(names(trainingClean) %in% removeCols)]
testingInClean <- testingInClean[, -which(names(testingInClean) %in% removeCols)]
dim(trainingClean)

resultsCorrelation <- cor(trainingClean[, names(trainingClean) != 'classe'])
palette <- colorRampPalette(c('blue', 'white', 'red'))(n = 199)
heatmap(resultsCorrelation, col = palette)

resultsCorrelation[(resultsCorrelation < -0.9 | resultsCorrelation > 0.9) & resultsCorrelation != 1]

partition <- createDataPartition(y=trainingClean$classe, p=0.6, list=FALSE)
trainingPart <- trainingClean[partition,]
testingPart <- trainingClean[-partition,]

trainingTree <- tree(classe~.,data=trainingPart)
summary(trainingTree)

plot(trainingTree)
text(trainingTree,pretty=0, cex = .8)

methodRF <- randomForest(classe ~ ., data = trainingPart, proximity=TRUE)

varImpPlot(methodRF,)

# Apply Model to Test Set
results <- predict(methodRF, newdata = testingInClean)
results

# Write Result For Submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(results)
