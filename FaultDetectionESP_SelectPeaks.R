#########################################################################################################
# The spectrum and features tables were downloaded from https://github.com/NINFA-UFES/ESPset            #
#########################################################################################################
# Load the spectrum file
spectrum_signals=read.csv(spectrum_file, fill = TRUE, header = FALSE, sep=";")

# Load the features file
features_signals=read.csv(features_file, fill = TRUE, header = TRUE, sep=";")

# Re-set the colnames to numbers
colnames(spectrum_signals)<-1:length(colnames(spectrum_signals))

# Set spectrum signal according to feature signal/
spectrum_signals$id<-features_signals$id

# Spectrum and features merged
# In this table I have the signals and also the id, the esp_id and label.
spectrum_features_merged<-merge(spectrum_signals,features_signals[,c("id","esp_id","label")],by="id")
#########################################################################################################
# Averaging can be performed in the time domain or in the frequency domain. In this section, we will focus mainly on averaging in the frequency domain, which is the primary type of averaging used with FFT analyzers.
average_per_frequency<-colMeans(spectrum_signals[,1:length(colnames(spectrum_signals))-1])

# Adjust a data.frame with the average frequnecy
df_average_frequency<-data.frame(Average=average_per_frequency,Frequency=names(average_per_frequency))

# Calculate the peaks
# The first column gives the height, the second the position/index where the maximum is reached, the third and forth the indices of where the peak begins and ends — in the sense of where the pattern starts and ends.
# npeaks
npeaks=50
peak_average<-findpeaks(average_per_frequency,npeaks=npeaks, sortstr=FALSE,minpeakheight=0.01,minpeakdistance=10)
#########################################################################################################

# Take the position of all peaks
peaks_position<-sort(as.vector(peak_average[,2]))

# Filter out peaks positions position before position 1000
peaks_position<-peaks_position[(peaks_position>=100) &  (peaks_position <=6000)]

# Take value of the amplitude in each of the npeaks=100
# First start a data.frame with nrows = nsignals and ncols = npeaks
# A data.frame with frequency and esp
df_amplitude_in_peaks    <- data.frame(matrix(0,nrow = dim(spectrum_signals)[1], ncol = length(peaks_position)))

# set colanmes of df_amplitude_in_peaks as the peaks_position
colnames(df_amplitude_in_peaks)<-peaks_position

# For each signal take the amplitude in the peaks positions
for (singnal in rownames(spectrum_signals))
{
  # df_amplitude_in_peaks 
  df_amplitude_in_peaks[singnal,]<-spectrum_signals[singnal,peaks_position]
}
#########################################################################################################
# Set rownames
df_amplitude_in_peaks$id<-rownames(df_amplitude_in_peaks)

# Add label
df_amplitude_in_peaks<-merge(df_amplitude_in_peaks,features_signals[,c("label","id")],by="id")

# Add collumns abnormal and normal
df_amplitude_in_peaks$Class<-"Abnormal"
df_amplitude_in_peaks[which(df_amplitude_in_peaks$label=="Normal"),"Class"]<-"Normal"

# Convert labels to factopr
df_amplitude_in_peaks$Class<-as.factor(df_amplitude_in_peaks$Class)

# Remove collumn lavbel
df_amplitude_in_peaks <-df_amplitude_in_peaks[ ,-which(colnames(df_amplitude_in_peaks)=="label") ]
df_amplitude_in_peaks <-df_amplitude_in_peaks[ ,-which(colnames(df_amplitude_in_peaks)=="id") ]
#########################################################################################################
# Split into trainning and testing
trainning<- as.vector(createDataPartition(df_amplitude_in_peaks$Class,times = 1,p = 0.5,list = TRUE)[[1]])
testing <- which(!rownames(df_amplitude_in_peaks) %in% trainning)

# Store trainning and testing data
trainning_amplitude_in_peaks<-df_amplitude_in_peaks[trainning,]
testing_amplitude_in_peaks  <-df_amplitude_in_peaks[testing,]

# Filter out the trainign and test set
trainning_amplitude_in_peaks<-trainning_amplitude_in_peaks[,which(colnames(trainning_amplitude_in_peaks) %in% c(peaks_position,"Class"))]
testing_amplitude_in_peaks<-testing_amplitude_in_peaks[,which(colnames(testing_amplitude_in_peaks) %in% c(peaks_position,"Class"))]
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
#########################################################################################################
svm_1_espset  <- train(Class ~ ., data = trainning_amplitude_in_peaks, method = "svmLinear", trControl = fitControl,metric="ROC",na.action=na.omit)
svm_2_espset  <- train(Class ~ ., data = trainning_amplitude_in_peaks, method = "svmRadial", trControl = fitControl,metric="ROC",na.action=na.omit)
knn_espset    <- train(Class ~ ., data = trainning_amplitude_in_peaks, method = "knn", trControl = fitControl,metric="ROC",na.action=na.omit)
mlp_espset    <- train(Class ~ ., data = trainning_amplitude_in_peaks, method = "mlp", trControl = fitControl,metric="ROC",na.action=na.omit)
#dnn_espset    <- train(Class ~ ., data = trainning_amplitude_in_peaks, method = "dnn", trControl = fitControl,metric="ROC",na.action=na.omit)
glm_espset    <- train(Class ~ ., data = trainning_amplitude_in_peaks, method = "glm", trControl = fitControl,metric="ROC",na.action=na.omit)
#########################################################################################################
svm_1_pred <- predict(svm_1_espset, testing_amplitude_in_peaks) 
svm_2_pred <- predict(svm_2_espset, testing_amplitude_in_peaks) 
knn_pred <- predict(knn_espset, testing_amplitude_in_peaks) 
mlp_pred <- predict(mlp_espset, testing_amplitude_in_peaks) 
#dnn_pred <- predict(dnn_espset, testing_amplitude_in_peaks) 
glm_pred <- predict(glm_espset, testing_amplitude_in_peaks) 
#########################################################################################################
varImp_svm_1_espset <- varImp(svm_1_espset, scale = FALSE)
varImp_svm_2_espset <- varImp(svm_2_espset, scale = FALSE)
varImp_knn_espset   <- varImp(knn_espset, scale = FALSE)
varImp_mlp_espset   <- varImp(mlp_espset, scale = FALSE)
varImp_glm_espset   <- varImp(glm_espset, scale = FALSE)
#varImp_dnn_espset   <- varImp(dnn_espset, scale = FALSE)

plot_varImp_svm_1_espset<-plot(varImp_svm_1_espset, main = "svmLinear") 
plot_varImp_svm_2_espset<-plot(varImp_svm_2_espset, main = "svmRadial") 
plot_varImp_mlp_espset<-plot(varImp_mlp_espset, main = "mlp") 
plot_varImp_knn_espset<-plot(varImp_knn_espset, main = "knn") 
plot_varImp_glm_espset<-plot(varImp_glm_espset, main = "glm") 
#plot_varImp_dnn_espset<-plot(varImp_dnn_espset, main = "dnn") 

# bwplo               
png(filename=paste(output_dir,"Variable_Importance_results_Peaks.png",sep=""), width = 25, height = 35, res=600, units = "cm")  
  #grid.arrange(plot_varImp_svm_1_espset,plot_varImp_svm_2_espset,plot_varImp_mlp_espset,plot_varImp_knn_espset,plot_varImp_glm_espset,plot_varImp_dnn_espset)
  grid.arrange(plot_varImp_svm_1_espset,plot_varImp_svm_2_espset,plot_varImp_mlp_espset,plot_varImp_knn_espset,plot_varImp_glm_espset)
dev.off()
#########################################################################################################
resamps <- resamples(list(svmLinear = svm_1_espset))                          

# svm_1_pred
svm_2_pred<-predict(svm_2_espset, newdata = testing_amplitude_in_peaks)

# Predictions for each 
svm_1_pred <- predict(svm_1_espset, testing_amplitude_in_peaks) 
svm_2_pred <- predict(svm_2_espset, testing_amplitude_in_peaks) 
knn_pred <- predict(knn_espset, testing_amplitude_in_peaks) 
mlp_pred <- predict(mlp_espset, testing_amplitude_in_peaks) 
dnn_pred <- predict(dnn_espset, testing_amplitude_in_peaks) 
glm_pred <- predict(glm_espset, testing_amplitude_in_peaks) 

knn_confusionMatrix<-confusionMatrix(knn_pred, testing_amplitude_in_peaks$Class)
#########################################################################################################
# Generate plot
plot_average<-ggplot(data = df_average_frequency, aes(x = as.integer(Frequency), y = Average))+ geom_line()  + theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  + ylim(0,0.1) 

# Generate plot
plot_average<-plot_average +  geom_segment(aes(x = 3003, y = 0.1, xend =  3003, yend = 0.025, colour = "green"), arrow = arrow(length = unit(0.50, "cm"))) 
plot_average<-plot_average +  geom_segment(aes(x = 169,  y = 0.1, xend = 169, yend = 0.025, colour = "blue"), arrow = arrow(length = unit(0.50, "cm")))
plot_average<-plot_average +  geom_segment(aes(x = 1490, y = 0.1, xend =  1490, yend = 0.025, colour = "red"), arrow = arrow(length = unit(0.50, "cm")))  

# FindClusters_resolution               
png(filename=paste(output_dir,"Plot_Average_SelectPeaks.png",sep=""), width = 15, height = 10, res=600, units = "cm")  
  plot_average
dev.off()
