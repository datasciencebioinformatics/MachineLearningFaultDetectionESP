# X1 position X1_IDX
# X2 position X2_IDX

# CONSTANT VARIABLES
# STARTING_IDX_POS  = 100
# ENDNG_IDX_POS     = 6200
# X1_IDX            = 3002 - STARTING_IDX_POS
# X2_IDX            = 6005 - STARTING_IDX_POS

# MEDIAN (8,13) CONSTANT VARIABLES
# MEDIAN_8_13_START = 240 - STARTING_IDX_POS
# MEDIAN_8_13_END   = 390 - STARTING_IDX_POS

# MEDIAN (98,102) CONSTANT VARIABLES
# MEDIAN_98_102_START = X1_IDX-61
# MEDIAN_98_102_END   = X1_IDX+61

# CONSTANT VARIABLES FOR THE CALCULATION OF REGRESSION 
# IDXBEGIN  <-100-STARTING_IDX_POS
# IDXEND    <-1200-STARTING_IDX_POS
#########################################################################################################
# Load the spectrum file
spectrum_signals=read.csv(spectrum_file, fill = TRUE, header = FALSE, sep=";")

# Load the features file
features_signals=read.csv(features_file, fill = TRUE, header = TRUE, sep=";")

# Take the number of collumns of the spectrum_signals table
nCollumns_spectrum<-length(colnames(spectrum_signals))

# Re-set the colnames to numbers
colnames(spectrum_signals)<-1:nCollumns_spectrum
#########################################################################################################
# Constants are same as the github.com/NINFA-UFES/ESPset
STARTING_IDX_POS  = 101
ENDING_IDX_POS     = 6100+1
X1_IDX            = 3002 - STARTING_IDX_POS
X2_IDX            = 6005 - STARTING_IDX_POS
#########################################################################################################
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
#########################################################################################################
# Plot the average data data
ggplot2_raw_data_limits<-ggplot(data = melt_spectrum_signals, aes(x = frequency_id, y = amplitude,colour = factor(label)))+ geom_line(aes(group=id)) + theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())   + ggtitle("Raw data")   +  ylim(0, 1) + xlab("spectrum")+ ylab("inches/s") + scale_x_continuous(breaks=c(STARTING_IDX_POS,ENDING_IDX_POS,X1_IDX,X2_IDX), limits = c(0, max(melt_spectrum_signals$frequency_id)) )+ theme(axis.text.x = element_text(angle=90,size=9))

# Plot_raw_vibration_data.png               
png(filename=paste(output_dir,"Plot_raw_vibration_limits.png",sep=""), width = 25, height = 10, res=600, units = "cm")  
  ggplot2_raw_data_limits
dev.off()
#########################################################################################################







#########################################################################################################
# 1) First, convert the frequencies ids to rotation x
# Convert the frequencies ids to rotation x
# x unit
x_unit<-1/X1_IDX

# Convert the frequency_id vector to rotation x vector
rotation_x<-(0:(nCollumns_spectrum-1))*x_unit

# Convert the frequency_id vector to rotation x vector
colnames(spectrum_signals)[1:nCollumns_spectrum]<-rotation_x
#colnames(spectrum_signals)[1:nCollumns_spectrum]<-1:nCollumns_spectrum

# Spectrum and features merged
# In this table I have the signals and also the id, the esp_id and label.
spectrum_features_merged<-merge(spectrum_signals,features_signals[,c("id","esp_id","label")],by="id")
#########################################################################################################
# Re-sample normal samples
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

# Plot_raw_vibration_data.png               
png(filename=paste(output_dir,"Plot_raw_vibration_convert.png",sep=""), width = 20, height = 30, res=600, units = "cm")  
ggplot(data = melt_spectrum_signals, aes(x = frequency_id, y = amplitude,colour = factor(label)))+ geom_line(aes(group=id))+ facet_grid(vars(label),scales="free") + theme_bw()  + ylim(0,0.1) + scale_x_continuous(name="X rotation", limits=c(0, max(melt_spectrum_signals$frequency_id)),breaks=seq(0,max(melt_spectrum_signals$frequency_id),by=0.25))
dev.off()
#########################################################################################################
# 2) Second, plot also the trimmed version from STARTING_IDX_POS:ENDING_IDX_POS
# Plot also the trimmed version
# Load the spectrum file
# Re-set the colnames to numbers
spectrum_signals_filtered<-spectrum_signals[,c(STARTING_IDX_POS:ENDING_IDX_POS)]

# Take the ids as the rownames
spectrum_signals_filtered$id<-as.integer(rownames(spectrum_signals_filtered))

# Trimmed from STARTING_IDX_POS till ENDING_IDX_POS 
# Spectrum and features merged
# In this table I have the signals and also the id, the esp_id and label.
spectrum_features_merged<-merge(spectrum_signals_filtered,features_signals[,c("id","esp_id","label")],by="id")
#########################################################################################################
# Re-sample normal samples
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

# Plot_raw_vibration_data.png               
png(filename=paste(output_dir,"Plot_raw_vibration_trimmed.png",sep=""), width = 20, height = 30, res=600, units = "cm")  
ggplot(data = melt_spectrum_signals, aes(x = frequency_id, y = amplitude,colour = factor(label)))+ geom_line(aes(group=id))+ facet_grid(vars(label),scales="free") + theme_bw()  + ylim(0,0.1) + scale_x_continuous(name="X rotation", limits=c(0, max(melt_spectrum_signals$frequency_id)),breaks=seq(0,max(melt_spectrum_signals$frequency_id),by=0.25))
dev.off()



















# Select only equipment four
melt_spectrum_signals_signal<-melt_spectrum_signals[which(melt_spectrum_signals$esp_id=="4"),]

# Plot_raw_vibration_data.png               
png(filename=paste(output_dir,"Plot_raw_vibration_eqp_4.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  ggplot(data = melt_spectrum_signals_signal, aes(x = frequency_id, y = amplitude,colour = factor(label)))+ geom_line(aes(group=id))+ facet_grid(vars(label),scales="free") + theme_bw()  + ylim(0,0.1) + scale_x_continuous(name="X rotation", limits=c(0, max(melt_spectrum_signals$frequency_id)),breaks=seq(0,max(melt_spectrum_signals$frequency_id),by=0.25))
dev.off()



#########################################################################################################
# Procedure for averaging per condition
# For each condition, the average amplitude will be calculated

# First the frequency_id in the colnames
frequency_ids_vector<- 1:nCollumns_spectrum

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
  signals_from_esp_label<-signals_from_esp_label[,which(colnames(signals_from_esp_label) %in% frequency_ids_vector)]

  # For each frequency_id
  for (frequency_id in frequency_ids_vector)
  {
    # Then, take the average of the amplitude for each frequency_id
    mean_of_amplitude<-mean(signals_from_esp_label[,frequency_id])

    # Mean of ampplitude for the equipment
    df_esp_frequency[label,frequency_id]<-mean_of_amplitude    
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
colnames(melt_spectrum_signals)<-c("label","frequency_id","amplitude")

# Convert collumn to numeric
melt_spectrum_signals$frequency_id<-as.numeric(as.vector(melt_spectrum_signals$frequency_id))
                               
# Each line represents a signal.
# For each the 6032 vibration signals , there are 12103 collumns. Each collumn represents the amplitude.
# Therefore, two collumns are needed, x for the singal and y for the amplitude.

# Plot the average data data
ggplot2_raw_data<-ggplot(data = melt_spectrum_signals, aes(x = frequency_id, y = amplitude,colour = factor(label)))+ geom_line(aes(group=label))+ facet_grid(vars(label),scales="free") + theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())   + ggtitle("Averaged data")   +  ylim(0, 0.1) + xlab("spectrum")+ ylab("inches/s") + xlim(101,6201)

# Plot_raw_vibration_data.png               
png(filename=paste(output_dir,"Plot_raw_vibration_average.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  ggplot2_raw_data
dev.off()
#########################################################################################################
# Add a collumn with the esp_id combined with the label
spectrum_features_merged$esp_id_label<-paste(spectrum_features_merged$label,spectrum_features_merged$esp_id,sep="_")

# First the frequency_id in the colnames
frequency_ids_vector<- 1:nCollumns_spectrum

# Second the esp_id in the colnames
esp_ids_vector      <- sort(unique(spectrum_features_merged$esp_id))

# Third the esp_id combined with the label
esp_with_label      <- sort(unique(spectrum_features_merged$esp_id_label))

# A data.frame with frequency and esp
df_esp_frequency    <- data.frame(matrix(nrow = length(esp_with_label), ncol = length(frequency_ids_vector)))

# Set the rownames with frtequency names
colnames(df_esp_frequency)<-frequency_ids_vector

# Set the colnames with frtequency names
rownames(df_esp_frequency)<-esp_with_label

# For each esp_ids take the average amplitude
for (esp_with_label_id in esp_with_label)
{
  # Take all signals from a specific esp
  signals_from_esp_label<-spectrum_features_merged[spectrum_features_merged$esp_id_label %in% esp_with_label_id,frequency_ids_vector]
 
  # For each frequency_id
  for (frequency_id in frequency_ids_vector)
  {
    # Then, take the average of the amplitude for each frequency_id
    mean_of_amplitude<-mean(signals_from_esp_label[,frequency_id])

    # Mean of ampplitude for the equipment
    df_esp_frequency[esp_with_label_id,frequency_id]<-mean_of_amplitude    
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
ggplot2_raw_data<-ggplot(data = melt_spectrum_signals, aes(x = as.integer(frequency_id), y = amplitude,colour = factor(esp_id)))+ geom_line(aes(group=esp_id))+ theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  + ylim(min(melt_spectrum_signals$amplitude), 100) + ggtitle("Average data (per esp)") + xlim(min(as.integer(melt_spectrum_signals$frequency_id)), max(as.integer(melt_spectrum_signals$frequency_id))) + facet_grid(vars(label), scales="free")  +  ylim(0, 0.1) + xlab("spectrum")+ ylab("inches/s") + xlim(101,6201)

# Plot_raw_vibration_data.png              
png(filename=paste(output_dir,"Plot_raw_vibration_average_esp.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  ggplot2_raw_data
dev.off()
#################################################################################################################
