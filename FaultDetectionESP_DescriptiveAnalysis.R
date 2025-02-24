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
all_signal_statistical_indicators=data.frame(id=c(),min=c(),max=c(),median=c(),mean=c(),sd=c(),skewness=c(),kurtosis=c())

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
  amplitude_vector<-spectrum_features_merged[signal_id,frequency_id]

  # Add the results for the signal
  # It takes the amplitude vector as input (time-series) and calculate w, min, max, mean, median, sd and stat
  # this for an interval of size w
  SlidingWindows<-descritive.SlidingWindows(as.vector(unlist(amplitude_vector)), w = 100, skewness = "moment", kurtosis = "moment")
  
  # statistical indicators
  # "w"        "min"      "max"      "mean"     "median"   "sd"       "skewness" "kurtosis"  
  # For each signal, there will be a vector containing results
  # the resulting vector has the same size of the input amplitude_vector
  # df_signa_statistical_indicators of the signal
  df_signa_statistical_indicators<-rbind(all_signal_statistical_indicators,data.frame(id=signal_id,min=as.vector(SlidingWindows$min),max=as.vector(SlidingWindows$max),median=as.vector(SlidingWindows$median),mean=as.vector(SlidingWindows$mean),sd=as.vector(SlidingWindows$sd),skewness=as.vector(SlidingWindows$skewness),kurtosis=as.vector(SlidingWindows$kurtosis)))
}
# For each signal, I have all the frequency_ids as collumns.
# and in each collumn I have the folllowing information for each slidding window:
# min      : mimuim amplitude value wihing the slidding windows
# max      : maximum amplitude value wihing the slidding windows
# mean     : mean amplitude value wihing the slidding windows
# median   : mean amplitude value wihing the slidding windows
# skewness : skewness amplitude value wihing the slidding windows
# kurtosis : kurtosis amplitude value wihing the slidding windows






