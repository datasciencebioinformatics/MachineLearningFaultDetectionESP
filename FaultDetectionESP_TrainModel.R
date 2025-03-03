#########################################################################################################
# The spectrum and features tables were downloaded from https://github.com/NINFA-UFES/ESPset            #
#########################################################################################################
# Path to the feature file
features_file="/home/felipe/googledrive/MachineLearningFaultDetectionESP/ESPset_dataset/features.csv"

# Path to the spectrum file
spectrum_file="/home/felipe/googledrive/MachineLearningFaultDetectionESP/ESPset_dataset/spectrum.csv"
#########################################################################################################
# Split train and test set for caret
createDataPartition(features_file,times = 1,p = 0.5,list = TRUE,groups = features_file$labels)
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




#########################################################################################################
