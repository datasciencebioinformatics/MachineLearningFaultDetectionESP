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
# Initiate a data.frame for the results of all signals
df_feature_extraction=data.frame(signal=c(),RMS=c(),peak=c(),peak_to_peak=c(),median=c(),a=c(),b=c())

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
df_feature_extraction_peaks=data.frame(signal=c(),median8_13=c(),median98_102=c(),rms98_102=c(),peak1x=c(),peak2x=c(),a=c(),b=c())

# Vector to store the frequencies_id
frequency_id<-colnames(spectrum_features_merged[,-which(colnames(spectrum_features_merged) %in% c("id","esp_id","label","esp_id_label","esp_id_str"))])

# CONSTANT VARIABLES
STARTING_IDX_POS  = 0
ENDNG_IDX_POS     = 6100
X1_IDX            = 3002 - STARTING_IDX_POS
X2_IDX            = 6005 - STARTING_IDX_POS

# MEDIAN (8,13) CONSTANT VARIABLES
MEDIAN_8_13_START = 240 - STARTING_IDX_POS
MEDIAN_8_13_END   = 390 - STARTING_IDX_POS

# Vector to store the frequecies 
# CONSTANT STAR 
frequency_vector<-frequency_id[STARTING_IDX_POS:length(frequency_id)]
frequency_vector<-frequency_id[1:ENDNG_IDX_POS]

# For each signal, the amplitude is taken for all frequency_id
for (signal_id in rownames(spectrum_features_merged))
{
  print(signal_id)
  
  # Vector to store amplitude for the frequency vector
  amplitude_vector<-spectrum_features_merged[signal_id,frequency_vector]

  # The median of the amplitude in the interval (8,13)
  # The median of a given signal in the interval starting in MEDIAN_8_13_START to MEDIAN_8_13_END
  # They were defined as constants, to be redefined in new data.
  median8_13   <- median(as.vector(unlist(amplitude_vector[MEDIAN_8_13_START:MEDIAN_8_13_END])))

  # The median of the amplitude in the interval (8,13)
  # The median of a given signal in the interval starting in MEDIAN_8_13_START to MEDIAN_8_13_END
  median98_102   <-median(as.vector(unlist(amplitude_vector[(X1_IDX-61):(X1_IDX+61)])))
  
  # The root-mean-suqare of the amplitude in the interval (98,102)
  # The sum is calulated by the somatory of the amplitude of given signal in the interval X1_IDX-61 to X2_IDX+61
  # Somatory(98_102) = (Amplitude(Signal X,X1_IDX-61 to X1_IDX+61))
  # After the square of the Somatory(98_102) is elevated to the 0.5 potency : Somatory(98_102)**0.5
  rms98_102 <-rms(as.vector(unlist(amplitude_vector[(X1_IDX-61):(X1_IDX+61)])))
  
  # The peak1x of a given signal is given in the position defined by the constant X1_IDX.
  # This variable must be re-defedined with the new data.
  peak1x<-as.vector(unlist(amplitude_vector[X1_IDX]))

  # The peak2x of a given signal is given in the position defined by the constant X2_IDX
  # This variable must be re-defedined with the new data.
  peak2x<-as.vector(unlist(amplitude_vector[X2_IDX]))

  # CONSTANT VARIABLES FOR THE CALCULATION OF REGRESSION 
  IDXBEGIN  <-100-STARTING_IDX_POS
  IDXEND    <-1200-STARTING_IDX_POS

  # Take the log of the amplitude in the inverval from IDXBEGIN to IDXEND
  # These variable must be re-defedined with the new data.
  xdata = data.frame(Signal=as.vector(unlist(log(amplitude_vector[IDXBEGIN:(IDXEND)]+1e-10))),Interval=IDXBEGIN:IDXEND)
            
  # exponential regression 1
  fit_er = lm(xdata$Interval~xdata$Signal, data = xdata) 

  # Coefficient 
  cf <- coef(fit_er)
  
  # Store cofficientes
  intercept <-  data.frame(fit_er$coefficients)[1,1]
  slope     <-  data.frame(fit_er$coefficients)[2,1]
  
  # Add the results for the signal
  df_results<-data.frame(signal=signal_id,median8_13=median8_13,median98_102=median98_102,rms98_102=rms98_102,peak1x=peak1x,peak2x=peak2x,a=slope,b=intercept)

  # Merge data.frame
  df_feature_extraction_peaks<-rbind(df_feature_extraction_peaks,df_results)
}
# center and scale the data before
# calculation the components
plot_feature_extraction_peaks<-na.omit(df_feature_extraction_peaks[,c("median8_13","median98_102","rms98_102","peak1x","peak2x","a","b")])
plot_feature_extraction_peaks<- plot_feature_extraction_peaks[!is.infinite(rowSums(plot_feature_extraction_peaks)),]

# Calculate pca
model.pca <- prcomp(plot_feature_extraction_peaks,center = FALSE, scale =FALSE, rank. = 4)

# Add label label_esp_id
spectrum_features_merged$label_esp_id<-paste(spectrum_features_merged$label,spectrum_features_merged$esp_id,sep="_")
spectrum_features_merged$esp_id_str  <-paste(spectrum_features_merged$esp_id,sep="")

# Plot pca's
PCA_of_spectral_data_label       <-autoplot(model.pca, data = spectrum_features_merged, colour = 'label') + theme_bw() 
PCA_of_spectral_data_esp_id       <-autoplot(model.pca, data = spectrum_features_merged, colour = 'esp_id_str') + theme_bw()
PCA_of_spectral_data_esp_id_label <-autoplot(model.pca, data = spectrum_features_merged, colour = 'label_esp_id') + theme_bw()

# FindClusters_resolution               
png(filename=paste(output_dir,"Plot_summary_PCA_of_spectral_data.png",sep=""), width = 40, height = 25, res=600, units = "cm")  
  grid.arrange(PCA_of_spectral_data_label, PCA_of_spectral_data_esp_id,PCA_of_spectral_data_esp_id_label, ncol = 3, nrow = 1, top = "Summary of vibration data") 
dev.off()
############################################################################################################################
# To DO: on Weekend
# Check the consitency
teste2<-features_signals
teste1<-df_feature_extraction_peaks


cor(teste1$a,teste2$a)
cor(teste1$b,teste2$b)

cor(teste1$median8_13,teste2$median.8.13.) # OK
cor(teste1$median98_102,teste2$median.98.102.) #OK
cor(teste1$rms98_102,teste2$rms.98.102.) # OK
cor(teste1$peak1x,teste2$peak1x) 
cor(teste1$peak2x,teste2$peak2x)


