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
# Vector to store the frequencies_id
frequency_id<-colnames(spectrum_features_merged[,-which(colnames(spectrum_features_merged) %in% c("id","esp_id","label","esp_id_label","esp_id_str"))])

# Constant to define end position
ENDNG_IDX_POS     = 6100

# Frequency_id
frequency_id<-frequency_id[1:6100]

#####################################################################################################################################
# Start a template data.frame
# Vector to store amplitude for the frequency vector
amplitude_vector<-spectrum_features_merged[1,frequency_id]

# Add the results for the signal
# It takes the amplitude vector as input (time-series) and calculate w, min, max, mean, median, sd and stat
# this for an interval of size w
SlidingWindows<-descritive.SlidingWindows(as.vector(unlist(amplitude_vector)), w = 100, skewness = "moment", kurtosis = "moment")

# Template data.frame
df_min       <- matrix(0, ncol = length(SlidingWindows$min), nrow = length(unique(all_signal_statistical_indicators$id)))
df_max       <- matrix(0, ncol = length(SlidingWindows$min), nrow = length(unique(all_signal_statistical_indicators$id)))
df_mean      <- matrix(0, ncol = length(SlidingWindows$min), nrow = length(unique(all_signal_statistical_indicators$id)))
df_median    <- matrix(0, ncol = length(SlidingWindows$min), nrow = length(unique(all_signal_statistical_indicators$id)))
df_sd        <- matrix(0, ncol = length(SlidingWindows$min), nrow = length(unique(all_signal_statistical_indicators$id)))
df_skewness  <- matrix(0, ncol = length(SlidingWindows$min), nrow = length(unique(all_signal_statistical_indicators$id)))
df_kurtosis  <- matrix(0, ncol = length(SlidingWindows$min), nrow = length(unique(all_signal_statistical_indicators$id)))
#####################################################################################################################################
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
  df_min      <-rbind(df_min,as.vector(SlidingWindows$min))
  df_max      <-rbind(df_max,as.vector(SlidingWindows$max))
  df_mean     <-rbind(df_mean,as.vector(SlidingWindows$mean))
  df_median   <-rbind(df_median,as.vector(SlidingWindows$median))
  df_sd       <-rbind(df_sd,as.vector(SlidingWindows$sd))
  df_skewness <-rbind(df_skewness,as.vector(SlidingWindows$skewness))
  df_kurtosis <-rbind(df_kurtosis,as.vector(SlidingWindows$kurtosis))   
}
# Set rownames
rownames(df_min)       <-signal_id
rownames(df_max)       <-signal_id
rownames(df_mean)      <-signal_id
rownames(df_median)    <-signal_id
rownames(df_sd)        <-signal_id
rownames(df_skewness)  <-signal_id
rownames(df_kurtosis)  <-signal_id

# For each signal, I have all the frequency_ids as collumns.
# and in each collumn I have the folllowing information for each slidding window:
# min      : mimuim amplitude value wihing the slidding windows
# max      : maximum amplitude value wihing the slidding windows
# mean     : mean amplitude value wihing the slidding windows
# median   : mean amplitude value wihing the slidding windows
# skewness : skewness amplitude value wihing the slidding windows
# kurtosis : kurtosis amplitude value wihing the slidding windows
# First, I must split the df_signa_statistical_indicators into different data.frames.
# Start a template data.frame





