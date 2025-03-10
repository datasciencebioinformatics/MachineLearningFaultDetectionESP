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
ggplot2_raw_data<-ggplot(data = melt_spectrum_signals, aes(x = as.integer(frequency_id), y = amplitude,colour = factor(label)))+ geom_line()+ facet_grid(vars(label),scales="free") + theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  + ggtitle("Raw data")   +  ylim(0, 1) 

# Plot_raw_vibration_data.png               
png(filename=paste(output_dir,"Plot_raw_vibration_convert.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  ggplot2_raw_data
dev.off()
