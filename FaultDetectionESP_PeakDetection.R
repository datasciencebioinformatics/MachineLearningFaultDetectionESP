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
# Constant to define end position
STARTING_IDX_POS     = 101
ENDING_IDX_POS     = 6000
#########################################################################################################
# Initiate a data.frame for the results of all signals
df_index_peaks    <-data.frame(peak1_index=c(),peak2_index=c(),peak3_index=c())
df_amplitude_peaks<-data.frame(peak1_ampl=c(),peak2_ampl=c(),peak3_ampl=c())

# Vector to store the frequencies_id
frequency_id<-colnames(spectrum_features_merged[,-which(colnames(spectrum_features_merged) %in% c("id","esp_id","label","esp_id_label","esp_id_str"))])

# For each signal, the amplitude is taken for all frequency_id
for (signal_id in rownames(spectrum_features_merged))
{
  print(signal_id)
  
  # Vector to store amplitude for the frequency vector
  amplitude_vector<-as.vector(unlist(spectrum_signals[signal_id,STARTING_IDX_POS:ENDING_IDX_POS]))
  
  # Calculate the peaks
  peaks<-findpeaks(amplitude_vector,npeaks=3, sortstr=FALSE,minpeakheight=0.01,minpeakdistance=20)

  #indexes<-data.frame(t(which.maxn(amplitude_vector, 3)))
  #amplitude<-data.frame(t(amplitude_vector[as.vector(unlist(indexes))]))
  
  # If functrion returned null
  if (!is.logical(peaks))
  { 
    # peaks
    peaks<-rbind(peaks,c(NA,NA,NA,NA))    
    peaks<-rbind(peaks,c(NA,NA,NA,NA))    
    peaks<-rbind(peaks,c(NA,NA,NA,NA))    
  }
  # If number of dimensions shows less than three rows
  if((dim(peaks)[1]<3))
  {
    # peaks
    peaks<-rbind(peaks,c(NA,NA,NA,NA))    
    peaks<-rbind(peaks,c(NA,NA,NA,NA))    
  }    
  
  # Take the indexes and the amplitude
  indexes<-data.frame(t(peaks[1:3,2]))
  amplitude<-data.frame(t(peaks[1:3,1]))

  # Set the colnames
  colnames(indexes)   <-c("peak1","peak2","peak3")
  colnames(amplitude) <-c("peak1","peak2","peak3")

  df_index_peaks<-rbind(df_index_peaks,indexes)
  df_amplitude_peaks<-rbind(df_amplitude_peaks,amplitude)
}
#########################################################################################################
filtered_amplitude_peaks <- df_amplitude_peaks[df_amplitude_peaks$peak2 >= 0, ]
filtered_amplitude_peaks <- na.omit(filtered_amplitude_peaks[filtered_amplitude_peaks$peak3 >= 0, ])

# calculation the components
model.pca.amplitude  <- prcomp(filtered_amplitude_peaks,center = FALSE, scale =FALSE, rank. = 4)

# Plot pca's
PCA_of_amplitude <-autoplot(model.pca.amplitude, data = spectrum_features_merged[rownames(filtered_amplitude_peaks),], colour = 'label') + theme_bw()
#########################################################################################################
# FindClusters_resolution               
png(filename=paste(output_dir,"Plot_summary_PCA_of_peaks_data.png",sep=""), width = 10, height = 10, res=600, units = "cm")  
  PCA_of_amplitude
dev.off()
#########################################################################################################
# Add id to df_index_peak
df_index_peaks$id<-rownames(df_index_peaks)

# Subselect spectrum_signals
spectrum_signals_subselection<-spectrum_signals[,c(STARTING_IDX_POS:ENDING_IDX_POS,"id")]

# Merge spectrum_selected
spectrum_selected_merged<-merge(spectrum_signals_subselection,df_index_peaks,by="id")

# Merge spectrum_selected
spectrum_selected_merged_2<-merge(spectrum_selected_merged,features_signals,by="id")

# Melt data.frame
spectrum_selected_melt<-melt(spectrum_selected_merged_2,id.vars =c("id","peak1","peak2","peak3","esp_id","label"))  

#########################################################################################################
# Re-set the colnames
colnames(spectrum_selected_melt)[7]<-"Frequency_id"

# Plot the raw data
index=5

# Generate plot
plot2<-ggplot(data = spectrum_selected_melt[spectrum_selected_melt$id==selected_signals[index],], aes(x = as.integer(Frequency_id), y = value))+ geom_line(aes(group=id))+ facet_grid(vars(id), scales="free") + theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + ggtitle(paste("Peak detection for signal ",selected_signals[index])) + ylim(0,0.05) 
plot2<-plot2 +  geom_segment(aes(x = df_index_peaks[which(rownames(df_index_peaks)==selected_signals[index]),"peak1"], y = 0.050, xend = df_index_peaks[which(rownames(df_index_peaks)==selected_signals[index]),"peak1"], yend = 0.025, colour = "green"), arrow = arrow(length = unit(0.25, "cm"))) 
plot2<-plot2 +  geom_segment(aes(x = df_index_peaks[which(rownames(df_index_peaks)==selected_signals[index]),"peak2"], y = 0.050, xend = df_index_peaks[which(rownames(df_index_peaks)==selected_signals[index]),"peak2"], yend = 0.025, colour = "blue"), arrow = arrow(length = unit(0.25, "cm")))
plot2<-plot2 +  geom_segment(aes(x = df_index_peaks[which(rownames(df_index_peaks)==selected_signals[index]),"peak3"], y = 0.050, xend = df_index_peaks[which(rownames(df_index_peaks)==selected_signals[index]),"peak3"], yend = 0.025, colour = "blue"), arrow = arrow(length = unit(0.25, "cm")))  

# FindClusters_resolution               
png(filename=paste(output_dir,"Plot_Peak_Detection_Example.png",sep=""), width = 15, height = 15, res=600, units = "cm")  
  plot2
dev.off()


# Generate plot
plot2<-ggplot(data = spectrum_selected_melt, aes(x = as.integer(Frequency_id), y = value))+ geom_line(aes(group=id))+ facet_grid(vars(label), scales="free") + theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + ggtitle(paste("Peak detection for signal ",selected_signals[index])) + ylim(0,1) 


# FindClusters_resolution               
png(filename=paste(output_dir,"Plot_Peak_Detection_Example_2.png",sep=""), width = 15, height = 20, res=600, units = "cm")  
  plot2
dev.off()
