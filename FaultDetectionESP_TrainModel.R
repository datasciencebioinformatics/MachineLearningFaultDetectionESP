#########################################################################################################
# Split into trainning and testing
trainning<- as.vector(createDataPartition(features_signals$label,times = 1,p = 0.5,list = TRUE)[[1]])
testing <- which(!rownames(features_signals) %in% trainning)

# Store trainning and testing data
trainning_features<-features_signals[trainning,c("label","median.8.13.","rms.98.102.","median.98.102.","peak1x","peak2x","a","b")]
testing_featurea  <-features_signals[trainning,c("label","median.8.13.","rms.98.102.","median.98.102.","peak1x","peak2x","a","b")]
#########################################################################################################
# Basic Parameter Tuning
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# Traing svm dataset
svm_espset <- train(label ~ ., data = trainning_features, method = "svmRadial", trControl = fitControl, verbose = FALSE)
knn_espset <- train(label ~ ., data = trainning_features, method = "knn", trControl = fitControl, verbose = FALSE)
nb_espset <- train(label ~ ., data = trainning_features, method = "nb", trControl = fitControl, verbose = FALSE.)
dnn_espset <- train(label ~ ., data = trainning_features, method = "dnn", trControl = fitControl, verbose = FALSE)
mlp_espset <- train(label ~ ., data = trainning_features, method = "dnn", trControl = fitControl, verbose = FALSE)
#########################################################################################################





# To do:
- Take models directly from vibration dataset
