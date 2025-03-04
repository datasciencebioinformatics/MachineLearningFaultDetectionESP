;#########################################################################################################
# The spectrum and features tables were downloaded from https://github.com/NINFA-UFES/ESPset            #
#########################################################################################################
# Load the spectrum file
spectrum_signals=read.csv(spectrum_file, fill = TRUE, header = FALSE, sep=";")

# Load the features file
features_signals=read.csv(features_file, fill = TRUE, header = TRUE, sep=";")

# Re-set the colnames to numbers
colnames(spectrum_signals)<-1:length(colnames(spectrum_signals))

# Set spectrum signal according to feature signal/
spectrum_signals$id<-features_signals$id

# Spectrum and features merged
# In this table I have the signals and also the id, the esp_id and label.
spectrum_features_merged<-merge(spectrum_signals,features_signals[,c("id","esp_id","label")],by="id")
#########################################################################################################
# Averaging can be performed in the time domain or in the frequency domain. In this section, we will focus mainly on averaging in the frequency domain, which is the primary type of averaging used with FFT analyzers.
average_per_frequency<-colMeans(spectrum_signals[,1:length(colnames(spectrum_signals))-1])

# Adjust a data.frame with the average frequnecy
df_average_frequency<-data.frame(Average=average_per_frequency,Frequency=names(average_per_frequency))

# Calculate the peaks
# The first column gives the height, the second the position/index where the maximum is reached, the third and forth the indices of where the peak begins and ends — in the sense of where the pattern starts and ends.
# npeaks
npeaks=100
peak_average<-findpeaks(average_per_frequency,npeaks=npeaks, sortstr=FALSE,minpeakheight=0,minpeakdistance=5)

# Generate plot
plot_average<-ggplot(data = df_average_frequency, aes(x = as.integer(Frequency), y = Average))+ geom_line()  + theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  + ylim(0,0.1) 

# FindClusters_resolution               
png(filename=paste(output_dir,"Plot_Average_Peaks.png",sep=""), width = 15, height = 20, res=600, units = "cm")  
  plot_average
dev.off()
#########################################################################################################
# Take the position of all peaks
peaks_position<-sort(as.vector(peak_average[,2]))

# Filter out peaks positions position before position 1000
peaks_position<-peaks_position[(peaks_position>=100) &  (peaks_position <=6000)]

# Take value of the amplitude in each of the npeaks=100
# First start a data.frame with nrows = nsignals and ncols = npeaks
# A data.frame with frequency and esp
df_amplitude_in_peaks    <- data.frame(matrix(0,nrow = dim(spectrum_signals)[1], ncol = length(peaks_position)))

# set colanmes of df_amplitude_in_peaks as the peaks_position
colnames(df_amplitude_in_peaks)<-peaks_position

# For each signal take the amplitude in the peaks positions
for (singnal in rownames(spectrum_signals))
{
  # df_amplitude_in_peaks 
  df_amplitude_in_peaks[singnal,]<-spectrum_signals[singnal,peaks_position]
}

