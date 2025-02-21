#########################################################################################################
# Load the spectrum file
spectrum_signals=read.csv(spectrum_file, fill = TRUE, header = TRUE, sep=";")

# Load the features file
features_signals=read.csv(features_file, fill = TRUE, header = TRUE, sep=";")
#########################################################################################################
# Initiate a data.frame for the results of all signals
df_feature_extraction=data.frame(signal=c(),RMS=c(),peak=c(),peak_to_peak=c(),median=c(),a=c(),b=c(),)

# Vector to store the frequencies_id
frequency_id<-colnames(spectrum_features_merged[,-which(colnames(spectrum_features_merged) %in% c("id","esp_id","label","esp_id_label","esp_id_str"))])

# For each signal, the amplitude is taken for all frequency_id
for (signal_id in rownames(spectrum_features_merged))
{
  print(signal_id)
  
  # Add also the peak          (maximum value)
  peak<-max(as.vector(unlist(spectrum_features_merged[signal_id,frequency_id])))

  # Add also the peak-to-peak (maximum-minimum value)
  peak_to_peak<-max(as.vector(unlist(spectrum_features_merged[signal_id,frequency_id])))-min(as.vector(unlist(spectrum_features_merged[signal_id,frequency_id])))

  # Calculate the rms
  rms<-rms(as.vector(unlist(spectrum_features_merged[signal_id,frequency_id])))

  # Calculate the median
  median<-median(as.vector(unlist(spectrum_features_merged[signal_id,frequency_id])))

  # frequency and signal 
  frequency   <-as.integer(colnames(spectrum_features_merged[signal_id,frequency_id]))
  signal      <-unlist(as.vector(spectrum_features_merged[signal_id,frequency_id]))
  
  # Compose dataset with signal and frequency
  data<-data.frame(x=signal,y=frequency)

  # exponential regression 1
  fit_er = lm(signal~frequency, data = data) 
  #fit_er = lm(log(signal, base = exp(1))~frequency, data = data) 

  # Store cofficientes
  a=summary(fit_er)$coefficients[1,1]
  b=summary(fit_er)$coefficients[1,2]

  # Add the results for the signal
  df_feature_extraction<-rbind(df_feature_extraction,data.frame(signal=signal_id,RMS=rms,peak=peak,peak_to_peak=peak_to_peak,median=median,a=a,b=b))  
}
#########################################################################################################
write.csv(df_feature_extraction,"/home/felipe/Downloads/df_feature_extraction.csv", row.names = FALSE)
#########################################################################################################
# Field withe esp_id as _str
spectrum_features_merged$esp_id_str<-paste(spectrum_features_merged$esp_id,sep="")
spectrum_features_merged$label_esp_id<-paste(spectrum_features_merged$label,spectrum_features_merged$esp_id,sep="_")

# Calculate and plot pca
model.features     <- prcomp(df_feature_extraction[,c("RMS","peak","peak_to_peak","median","a","b")],center = FALSE, scale =FALSE, na.action = na.omit, rank. = 4)

# Plot pca's
PCA_of_spectral_data_label          <-autoplot(model.features, data =spectrum_features_merged, colour = 'label') + theme_bw()
PCA_of_spectral_data_esp_id         <-autoplot(model.features, data =spectrum_features_merged, colour = 'esp_id_str') + theme_bw()
PCA_of_spectral_data_esp_id_label   <-autoplot(model.features, data =spectrum_features_merged, colour = 'label_esp_id') + theme_bw()

# FindClusters_resolution               
png(filename=paste(output_dir,"Plot_summary_Features_data.png",sep=""), width = 40, height = 25, res=600, units = "cm")  
  grid.arrange(PCA_of_spectral_data_label, PCA_of_spectral_data_esp_id,PCA_of_spectral_data_esp_id_label, ncol = 3, nrow = 1, top = "PCA of empirical model decomposition") 
dev.off()
#########################################################################################################
