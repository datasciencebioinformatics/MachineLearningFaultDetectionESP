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

# Constant to define end position
ENDNG_IDX_POS     = 6100

# Frequency_id
frequency_id<-frequency_id[1:6100]

# For each signal, the amplitude is taken for all frequency_id
for (signal_id in rownames(spectrum_features_merged))
{
  print(signal_id)
  
  # Vector to store amplitude for the frequency vector
  amplitude_vector<-spectrum_features_merged[signal_id,frequency_vector]

  # Add the results for the signal

  # It takes the amplitude vector as input (time-series) and calculate w, min, max, mean, median, sd and stat
  # this for an interval of size w
  # "w"        "min"      "max"      "mean"     "median"   "sd"       "skewness" "kurtosis"
  # For each signal, there will be a time    
  SlidingWindows<-descritive.SlidingWindows(as.vector(unlist(amplitude_vector)), w = 100, skewness = "moment", kurtosis = "moment")
  

}
