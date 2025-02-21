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
for (signal_id in rownames(spectrum_features_merged[,frequency_id]))
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
# Initiate a data.frame for the results of all signals
df_feature_extraction_peaks=data.frame(signal=c(),peak1x=c(),peak2x=c())

# Vector to store the frequencies_id
frequency_id<-colnames(spectrum_features_merged[,-which(colnames(spectrum_features_merged) %in% c("id","esp_id","label","esp_id_label","esp_id_str"))])

# For each signal, the amplitude is taken for all frequency_id
for (signal_id in rownames(spectrum_features_merged[,frequency_id]))
{
  print(signal_id)

  # CONSTANT VARIABLES
  STARTING_IDX_POS  = 100
  X1_IDX            = 3002 - STARTING_IDX_POS
  X2_IDX            = 6005 - STARTING_IDX_POS

  # MEDIAN (8,13) CONSTANT VARIABLES
  MEDIAN_8_13_START = 240 - STARTING_IDX_POS
  MEDIAN_8_13_END   = 390 - STARTING_IDX_POS

  # The median of the amplitude in the interval (8,13)
  # The median of a given signal in the interval starting in MEDIAN_8_13_START to MEDIAN_8_13_END
  # They were defined as constants, to be redefined in new data.
  median8_13   <-median(as.vector(unlist(spectrum_features_merged[signal_id,MEDIAN_8_13_START:MEDIAN_8_13_END])))
  
  # The median of the amplitude in the interval (98,102)
  # The sum is calulated by the somatory of the amplitude of given signal in the interval X1_IDX-61 to X2_IDX+61
  # Somatory(98_102) = (Amplitude(Signal X,X1_IDX-61 to X2_IDX+61))
  # After the square of the Somatory(98_102) is elevated to the 0.5 potency : Somatory(98_102)**0.5
  median98_102 <-sum(as.vector(unlist(spectrum_features_merged[signal_id,(X1_IDX-61):(X2_IDX+61)]))))**0.5

  # The peak1x of a given signal is given in the position defined by the constant X1_IDX.
  # This variable must be re-defedined with the new data.
  peak1x<-as.vector(unlist(spectrum_features_merged[signal_id,X1_IDX]))

  # The peak2x of a given signal is given in the position defined by the constant X2_IDX
  # This variable must be re-defedined with the new data.
  peak2x<-as.vector(unlist(spectrum_features_merged[signal_id,X2_IDX]))

  # CONSTANT VARIABLES FOR THE CALCULATION OF REGRESSION 
  IDXBEGIN  <-100
  IDXEND    <-1200
  
  xdata = np.log(X[signal_id, IDXBEGIN:IDXEND]+1e-10)

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

  new_feats['a'], new_feats['b'] = _extract_expregfeatures(X,100-starting_idx_pos, 1200-starting_idx_pos)
}

