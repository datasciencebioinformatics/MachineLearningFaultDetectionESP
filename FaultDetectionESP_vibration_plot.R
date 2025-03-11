#########################################################################################################
# Load the spectrum file
spectrum_signals=read.csv(spectrum_file, fill = TRUE, header = FALSE, sep=";")

# Load the features file
features_signals=read.csv(features_file, fill = TRUE, header = TRUE, sep=";")

# Take the number of collumns of the spectrum_signals table
nCollumns_spectrum<-length(colnames(spectrum_signals))
#########################################################################################################
# Re-set the colnames to numbers
rotating_X<-1:nCollumns_spectrum*0

# Max rate rotating X
max_rate<-length(rotating_X)/4096

# Rotating X conversion rate
convertion_rate<-max_rate/length(rotating_X)

# For each posiition, add increment the rotation rate
for (index in 2:length(rotating_X))
{
  # Add convertion_rate to the position
  rotating_X[index]<-rotating_X[index-1]+convertion_rate
}
#########################################################################################################
# Re-set the colnames to numbers
colnames(spectrum_signals)<-rotating_X

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
# Set the esp_id
df_esp_frequency$esp_with_label_id<-rownames(df_esp_frequency)

# Take list with the paired  id
esp_with_label_id_list<-strsplit(df_esp_frequency$esp_with_label_id,"_",fixed=T)

# Add also the field esp_id
df_esp_frequency$esp_id<-0

# And the field esp_id
df_esp_frequency$label<-""

# For each paired id in the list
for (esp_with_label_id in esp_with_label_id_list)
{
  # Add the esp_id and the label
  df_esp_frequency[paste(esp_with_label_id[1],esp_with_label_id[2],sep="_"),"esp_id"]<-esp_with_label_id[2]
  df_esp_frequency[paste(esp_with_label_id[1],esp_with_label_id[2],sep="_"),"label"]<-esp_with_label_id[1]
}
#################################################################################################################
# The spectrum_signals table must be melt.
# The id must be kept to identity each signal.
# Melt by multiple ids
melt_spectrum_signals<-melt(df_esp_frequency,id=c("esp_with_label_id","esp_id","label"))

# Rename collumn
colnames(melt_spectrum_signals)<-c("esp_with_label_id","esp_id","label","frequency_id","amplitude")
                               
# Each line represents a signal.
# For each the 6032 vibration signals , there are 12103 collumns. Each collumn represents the amplitude.
# Therefore, two collumns are needed, x for the singal and y for the amplitude.

# Plot the raw data
ggplot2_raw_data<-ggplot(data = melt_spectrum_signals, aes(x = as.integer(frequency_id), y = amplitude,colour = factor(label)))+ geom_line(aes(group=esp_with_label_id))+ theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  + ylim(min(melt_spectrum_signals$amplitude), 100) + ggtitle("Average data (per esp and condition)") + xlim(min(as.integer(melt_spectrum_signals$frequency_id)), max(as.integer(melt_spectrum_signals$frequency_id))) + facet_grid(vars(label), scales="free")

# Plot_raw_vibration_data.png              
png(filename=paste(output_dir,"Plot_average_vibration_data.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  ggplot2_raw_data
dev.off()
#########################################################################################################

