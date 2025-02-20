#########################################################################################################
Load the spectrum file
spectrum_signals=read.csv(spectrum_file, fill = TRUE, header = TRUE, sep=";")

# Load the features file
features_signals=read.csv(features_file, fill = TRUE, header = TRUE, sep=";")
#########################################################################################################
# Initiate a data.frame for the results of all signals
df_feature_extraction=data.frame(signal=c(),RMS=c(),peak=c(),peak_to_peak=c(),median=c())

# Vector to store the frequencies_id
frequency_id<-colnames(spectrum_features_merged[,-which(colnames(spectrum_features_merged) %in% c("id","esp_id","label","esp_id_label","esp_id_str"))])

# For each signal, the amplitude is taken for all frequency_id
for (signal in rownames(spectrum_features_merged))
{
  print(signal)

  # Add also the peak          (maximum value)
  peak<-max(as.vector(unlist(spectrum_features_merged[signal,frequency_id])))
  
  # Add also the peak-to-peak (maximum-minimum value)
  peak_to_peak<-max(as.vector(unlist(spectrum_features_merged[signal,frequency_id])))-min(as.vector(unlist(spectrum_features_merged[signal,frequency_id])))

  # Calculate the rms
  rms<-rms(as.vector(unlist(spectrum_features_merged[signal,frequency_id])))

  # Calculate the median
  median<-median(as.vector(unlist(spectrum_features_merged[signal,frequency_id])))

  # Add the results for the signal
  df_feature_extraction<-rbind(df_feature_extraction,data.frame(signal=signal,RMS=rms,peak=peak,peak_to_peak=peak_to_peak,median=median))  
}
#########################################################################################################
# Calculate and plot pca
model.features     <- prcomp(df_feature_extraction[,c("RMS","peak","peak_to_peak")],center = FALSE, scale =FALSE, na.action = na.omit, rank. = 4)
PCA_for_features   <-autoplot(model.features, data =spectrum_features_merged, colour = 'label') + theme_bw() + ggtitle("Amplitude")
#########################################################################################################
