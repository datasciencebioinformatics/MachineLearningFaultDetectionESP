#########################################################################################################
# Support machine models (SVM)
https://rpubs.com/uky994/593668

# K-Nearest Neighbor (KNN)
https://rpubs.com/njvijay/16444

# Naive Bayes (NB)
https://rpubs.com/maulikpatel/224581

# Stacked AutoEncoder Deep Neural Network (method = 'dnn').
# Stacked AutoEncoder Deep Neural Network
https://topepo.github.io/caret/train-models-by-tag.html#support-vector-machines

# "Convolutional Neural Network" R caret
https://rpubs.com/rusdipermana/image-cl

# Multi-Layer Perceptron
# Stacked AutoEncoder Deep Neural Network
https://topepo.github.io/caret/train-models-by-tag.html#neural-network
#########################################################################################################
# Split into trainning and testing
trainning<- as.vector(createDataPartition(features_signals$label,times = 1,p = 0.5,list = TRUE)[[1]])
testing <- which(!rownames(features_signals) %in% trainning)

# Store trainning and testing data
trainning_featurea<-features_signals[trainning,c("label","median.8.13.","rms.98.102.","median.98.102.","peak1x","peak2x","a","b")]
testing_featurea  <-features_signals[trainning,c("label","median.8.13.","rms.98.102.","median.98.102.","peak1x","peak2x","a","b")]
#########################################################################################################
# Basic Parameter Tuning
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# Traing svm dataset
svm_espset <- train(label ~ ., data = trainning_featurea, method = "svm", trControl = fitControl, verbose = FALSE)
