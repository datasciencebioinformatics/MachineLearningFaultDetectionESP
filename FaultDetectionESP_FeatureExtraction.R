#########################################################################################################
Load the spectrum file
spectrum_signals=read.csv(spectrum_file, fill = TRUE, header = TRUE, sep=";")

# Load the features file
features_signals=read.csv(features_file, fill = TRUE, header = TRUE, sep=";")
#########################################################################################################
# Initiate a data.frame for the results of all signals
df_root_mean_square_deviation=data.frame(signal=c(),RMS=c())

# Vector to store the frequencies_id
frequency_id<-colnames(spectrum_features_merged[,-which(colnames(spectrum_features_merged) %in% c("id","esp_id","label","esp_id_label","esp_id_str"))])

# For each signal, the amplitude is taken for all frequency_id
for (signal in rownames(spectrum_features_merged))
{
  print(signal)

  # Add the results for the signal
  df_root_mean_square_deviation<-rbind(df_root_mean_square_deviation,data.frame(signal=signal,RMS=rms(as.vector(unlist(spectrum_features_merged[1,frequency_id])), MARGIN = 2)))  
}
#########################################################################################################
