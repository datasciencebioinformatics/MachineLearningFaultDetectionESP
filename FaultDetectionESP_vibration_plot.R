#########################################################################################################
# Load the spectrum file
spectrum_signals=read.csv(spectrum_file, fill = TRUE, header = FALSE, sep=";")

# Load the features file
features_signals=read.csv(features_file, fill = TRUE, header = TRUE, sep=";")

# Take the number of collumns of the spectrum_signals table
nCollumns_spectrum<-length(colnames(spectrum_signals))
#########################################################################################################
# Re-set the colnames to numbers
colnames(spectrum_signals)<-1:nCollumns_spectrum

# Take the ids as the rownames
spectrum_signals$id<-as.integer(rownames(spectrum_signals))

# Spectrum and features merged
# In this table I have the signals and also the id, the esp_id and label.
spectrum_features_merged<-merge(spectrum_signals,features_signals[,c("id","esp_id","label")],by="id")
#########################################################################################################
# When needed, a procedure to subset Normal samples to reduce computational complexity
# Split normal samples from the other samples
spectrum_features_merged_normal_samples <-spectrum_features_merged[spectrum_features_merged$label=="Normal",]
spectrum_features_merged_except_samples <-spectrum_features_merged[spectrum_features_merged$label!="Normal",]

## Merge back the two data.frames
spectrum_features_merged<-rbind(sample_n(spectrum_features_merged_normal_samples, 100),spectrum_features_merged_except_samples)
#########################################################################################################
# The spectrum_signals table must be melt. 
# The id must be kept to identity each signal.
# Melt by multiple ids
melt_spectrum_signals<-melt(spectrum_features_merged,id=c("id","esp_id","label"))

# Rename collumn
colnames(melt_spectrum_signals)<-c("id","esp_id","label","frequency_id","amplitude")

# Convert collumn to numeric
melt_spectrum_signals$frequency_id<-as.numeric(as.vector(melt_spectrum_signals$frequency_id))

# Plot the average data data
ggplot2_raw_data<-ggplot(data = melt_spectrum_signals, aes(x = frequency_id, y = amplitude,colour = factor(label)))+ geom_line(aes(group=id))+ facet_grid(vars(label),scales="free") + theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())   + ggtitle("Raw data")   +  ylim(0, 0.3) + xlab("x rotation")+ ylab("inches/s")+ scale_x_continuous(breaks=seq(0,3,0.25))

# Plot_raw_vibration_data.png               
png(filename=paste(output_dir,"Plot_raw_vibration_convert.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  ggplot2_raw_data
dev.off()
#########################################################################################################
# Procedure for averaging per condition
# For each condition, the average amplitude will be calculated

# First the frequency_id in the colnames
frequency_ids_vector<- rotating_X

# Third the esp_id combined with the label
esp_label      <- sort(unique(spectrum_features_merged$label))

# A data.frame with frequency and esp
df_esp_frequency    <- data.frame(matrix(0,nrow = length(esp_label), ncol = length(frequency_ids_vector)))

# Set the rownames with frtequency names
colnames(df_esp_frequency)<-frequency_ids_vector

# Set the colnames with frtequency names
rownames(df_esp_frequency)<-esp_label

# For each esp_ids take the average amplitude
for (label in esp_label)
{
  # Take all signals from a specific esp
  signals_from_esp_label<-spectrum_features_merged[spectrum_features_merged$label %in% label,]

  # Take only the amplitude
  signals_from_esp_label<-signals_from_esp_label[,which(colnames(signals_from_esp_label) %in% rotating_X)]

  # For each frequency_id
  for (frequency_id in frequency_ids_vector)
  {
    # Then, take the average of the amplitude for each frequency_id
    mean_of_amplitude<-mean(signals_from_esp_label[,toString(frequency_id)])

    # Mean of ampplitude for the equipment
    df_esp_frequency[label,toString(frequency_id)]<-mean_of_amplitude    
  }    
}
# Set the label
df_esp_frequency$label<-rownames(df_esp_frequency)

#################################################################################################################
# The spectrum_signals table must be melt.
# The id must be kept to identity each signal.
# Melt by multiple ids
melt_spectrum_signals<-melt(df_esp_frequency,id=c("label"))

# Rename collumn
colnames(melt_spectrum_signals)<-c("label","rotating_x","amplitude")

# Convert collumn to numeric
melt_spectrum_signals$rotating_x<-as.numeric(as.vector(melt_spectrum_signals$rotating_x))
                               
# Each line represents a signal.
# For each the 6032 vibration signals , there are 12103 collumns. Each collumn represents the amplitude.
# Therefore, two collumns are needed, x for the singal and y for the amplitude.

# Plot the average data data
ggplot2_raw_data<-ggplot(data = melt_spectrum_signals, aes(x = rotating_x, y = amplitude,colour = factor(label)))+ geom_line(aes(group=label))+ facet_grid(vars(label),scales="free") + theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())   + ggtitle("Averaged data")   +  ylim(0, 0.1) + xlab("x rotation")+ ylab("inches/s")+ scale_x_continuous(breaks=seq(0,3,0.25))

# Plot_raw_vibration_data.png               
png(filename=paste(output_dir,"Plot_raw_vibration_convert.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  ggplot2_raw_data
dev.off()
#########################################################################################################

