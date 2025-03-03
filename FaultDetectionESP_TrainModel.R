#########################################################################################################
# Load the spectrum file
spectrum_signals=read.csv(spectrum_file, fill = TRUE, header = FALSE, sep=";")

# Load the features file
features_signals=read.csv(features_file, fill = TRUE, header = TRUE, sep=";")
#########################################################################################################
# Rename lables
features_signals[features_signals$label=="Faulty sensor","label"]<-"Faulty_Sensor"
features_signals[features_signals$label=="Misalignment","label"]<-"Misalignment"
features_signals[features_signals$label=="Normal","label"]<-"Normal"
features_signals[features_signals$label=="Rubbing","label"]<-"Rubbing"
features_signals[features_signals$label=="Unbalanced","label"]<-"Unbalanced"

# Convert labels to factopr
features_signals$label<-as.factor(features_signals$label)
#########################################################################################################
# Add collumns abnormal and normal
features_signals$Class<-"Abnormal"
features_signals[which(features_signals$label=="Normal"),"Class"]<-"Normal"

# Convert labels to factopr
features_signals$Class<-as.factor(features_signals$Class)
#########################################################################################################
# Split into trainning and testing
# Store trainning and testing data
trainingControl_features<-features_signals[trainning,c("Class","median.8.13.","rms.98.102.","median.98.102.","peak1x","peak2x","a","b")]

# Split into trainning and testing
trainning<- as.vector(createDataPartition(features_signals$label,times = 1,p = 0.5,list = TRUE)[[1]])
testing <- which(!rownames(features_signals) %in% trainning)

# Store trainning and testing data
trainning_features<-features_signals[trainning,c("Class","median.8.13.","rms.98.102.","median.98.102.","peak1x","peak2x","a","b")]
testing_features  <-features_signals[trainning,c("Class","median.8.13.","rms.98.102.","median.98.102.","peak1x","peak2x","a","b")]
#########################################################################################################
# Basic Parameter Tuning
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)


svm_1_espset  <- train(Class ~ ., data = trainning_features, method = "svmLinear", trControl = fitControl,metric="ROC")
svm_2_espset  <- train(Class ~ ., data = trainning_features, method = "svmRadial", trControl = fitControl,metric="ROC")
knn_espset    <- train(Class ~ ., data = trainning_features, method = "knn", trControl = fitControl,metric="ROC")
mlp_espset    <- train(Class ~ ., data = trainning_features, method = "mlp", trControl = fitControl,metric="ROC")
dnn_espset    <- train(Class ~ ., data = trainning_features, method = "dnn", trControl = fitControl,metric="ROC")
glm_espset    <- train(Class ~ ., data = trainning_features, method = "glm", trControl = fitControl,metric="ROC")
#########################################################################################################
resamps <- resamples(list(svmLinear = svm_1_espset, 
                          svmRadial = svm_2_espset,
                          knn = knn_espset,
                          mlp=mlp_espset,
                          dnn=dnn_espset,                 
                          glm=glm_espset))                          
#########################################################################################################
# Set up bwplot
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)

# bwplo               
png(filename=paste(output_dir,"Plot_bwplot_results.png",sep=""), width = 25, height = 12, res=600, units = "cm")  
  bwplot(resamps, layout = c(3, 1))
dev.off()
#########################################################################################################
svm_1_pred <- predict(svm_1_espset, testing_features) 
svm_2_pred <- predict(svm_2_espset, testing_features) 
knn_pred <- predict(knn_espset, testing_features) 
mlp_pred <- predict(mlp_espset, testing_features) 
dnn_pred <- predict(dnn_espset, testing_features) 
glm_pred <- predict(glm_espset, testing_features) 
#########################################################################################################
cm_svm_1  <-confusionMatrix(data = svm_1_pred, reference = testing_features$Class)
cm_svm_2  <-confusionMatrix(data = svm_2_pred, reference = testing_features$Class)
cm_knn    <-confusionMatrix(data = knn_pred, reference = testing_features$Class)
cm_mlp    <-confusionMatrix(data = mlp_pred, reference = testing_features$Class)
dnn_mlp   <-confusionMatrix(data = dnn_pred, reference = testing_features$Class)
glm_mlp   <-confusionMatrix(data = glm_pred, reference = testing_features$Class)


