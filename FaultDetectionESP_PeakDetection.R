#########################################################################################################
# Load the spectrum file
spectrum_signals=read.csv(spectrum_file, fill = TRUE, header = TRUE, sep=";")

# Load the features file
features_signals=read.csv(features_file, fill = TRUE, header = TRUE, sep=";")

# Re-set the colnames to numbers
colnames(spectrum_signals)<-1:length(colnames(spectrum_signals))

# Take the ids as the rownames
spectrum_signals$id<-as.integer(rownames(spectrum_signals))

# Spectrum and features merged
# In this table I have the signals and also the id, the esp_id and label.
spectrum_features_merged<-merge(spectrum_signals,features_signals[,c("id","esp_id","label")],by="id")
#########################################################################################################
print(signal_id)



#########################################################################################################
# Initiate a data.frame for the results of all signals
df_index_peaks    <-data.frame(peak1_index=c(),peak2_index=c(),peak3_index=c())
df_amplitude_peaks<-data.frame(peak1_ampl=c(),peak2_ampl=c(),peak3_ampl=c())

# Vector to store the frequencies_id
frequency_id<-colnames(spectrum_features_merged[,-which(colnames(spectrum_features_merged) %in% c("id","esp_id","label","esp_id_label","esp_id_str"))])

# For each signal, the amplitude is taken for all frequency_id
for (signal_id in rownames(spectrum_features_merged))
{
  print(signal_id)
  
  # Vector to store amplitude for the frequency vector
  amplitude_vector<-as.vector(unlist(spectrum_signals[signal_id,]))
  
  # Calculate the peaks
  peaks<-findpeaks(amplitude_vector,npeaks=3)

  # Take the indexes and the amplitude
  indexes<-data.frame(t(peaks[,4]))
  amplitude<-data.frame(t(peaks[,1]))

  # Set the colnames
  colnames(indexes)   <-c("peak1","peak2","peak3")
  colnames(amplitude) <-c("peak1","peak2","peak3")

  df_index_peaks<-rbind(df_index_peaks,indexes)
  df_amplitude_peaks<-rbind(df_amplitude_peaks,amplitude)
}

# calculation the components
model.pca.index      <- prcomp(df_index_peaks    ,center = FALSE, scale =FALSE, rank. = 4)
model.pca.amplitude  <- prcomp(df_amplitude_peaks,center = FALSE, scale =FALSE, rank. = 4)

# Plot pca's
PCA_of_index     <-autoplot(model.pca.index, data = spectrum_features_merged, colour = 'label') + theme_bw() + ggtitle("index")
PCA_of_amplitude <-autoplot(model.pca.amplitude, data = spectrum_features_merged, colour = 'label') + theme_bw()  + ggtitle("amplitude")
#########################################################################################################
# FindClusters_resolution               
png(filename=paste(output_dir,"Plot_summary_PCA_of_spectral_data.png",sep=""), width = 20, height = 10, res=600, units = "cm")  
  grid.arrange(PCA_of_index, PCA_of_amplitude, ncol = 2, nrow = 1, top = "Summary of vibration data") 
dev.off()

