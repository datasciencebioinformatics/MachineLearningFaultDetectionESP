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
  peaks<-findpeaks(amplitude_vector,npeaks=3,minpeakheight=0.01,minpeakdistance=50)

  # If functrion returned null
  if (!is.logical(peaks))
  { 
    # peaks
    peaks<-rbind(peaks,c(-1,-1,-1,-1))    
    peaks<-rbind(peaks,c(-1,-1,-1,-1))    
    peaks<-rbind(peaks,c(-1,-1,-1,-1))    
  }
  # If number of dimensions shows less than three rows
  if((dim(peaks)[1]<3))
  {
    # peaks
    peaks<-rbind(peaks,c(-1,-1,-1,-1))    
    peaks<-rbind(peaks,c(-1,-1,-1,-1))    
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
# calculation the components
model.pca.index      <- prcomp(df_index_peaks    ,center = FALSE, scale =FALSE, rank. = 4)
model.pca.amplitude  <- prcomp(df_amplitude_peaks,center = FALSE, scale =FALSE, rank. = 4)

# Plot pca's
PCA_of_index     <-autoplot(model.pca.index, data = spectrum_features_merged, colour = 'label') + theme_bw() + ggtitle("index")
PCA_of_amplitude <-autoplot(model.pca.amplitude, data = spectrum_features_merged, colour = 'label') + theme_bw()  + ggtitle("amplitude")
#########################################################################################################
# FindClusters_resolution               
png(filename=paste(output_dir,"Plot_summary_PCA_of_peaks_data.png",sep=""), width = 20, height = 10, res=600, units = "cm")  
  grid.arrange(PCA_of_index, PCA_of_amplitude, ncol = 2, nrow = 1, top = "PCA for 3 peaks per signal") 
dev.off()

#########################################################################################################
# Add id to df_index_peak
df_index_peaks$id<-rownames(df_index_peaks)

# Subselect spectrum_signals
spectrum_signals_subselection<-spectrum_signals[,STARTING_IDX_POS:ENDING_IDX_POS]

#  Plot the first 1000 frequency positions.
# Printing three rows  
selected_signals<-sample(df_index_peaks$id, 10)

#  Selected 10 random signals.
# spectrum_selected_signals[
df_index_peaks<-df_index_peaks[df_index_peaks$id %in% selected_signals,]

# Set id
spectrum_signals_subselection$id<-rownames(spectrum_signals_subselection)

# spectrum_signals_subselection
spectrum_signals_subselection<-spectrum_signals_subselection[selected_signals,]

# Merge spectrum_selected
spectrum_selected_merged<-merge(spectrum_signals_subselection,df_index_peaks,by="id")

# Melt data.frame
spectrum_selected_melt<-melt(spectrum_selected_merged,id.vars =c("id","peak1","peak2","peak3"))  

#########################################################################################################
# Re-set the colnames
colnames(spectrum_selected_melt)[5]<-"Frequency_id"


# Plot the raw data
ggplot2_raw_data<-ggplot(data = spectrum_selected_melt[spectrum_selected_melt$id==135,], aes(x = as.integer(Frequency_id), y = value))+ geom_line(aes(group=id))+ facet_grid(vars(id), scales="free") + theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + ggtitle("Descritive statistics with sliding windows") + ylim(0,1) + geom_segment(aes(x = spectrum_selected_melt[spectrum_selected_melt$id==135,"peak1"], y = 0.5, xend = spectrum_selected_melt[spectrum_selected_melt$id==135,"peak1"], yend = 0), arrow = arrow(length = unit(0.25, "cm")))  + geom_segment(aes(x = spectrum_selected_melt[spectrum_selected_melt$id==135,"peak2"], y = 0.5, xend = spectrum_selected_melt[spectrum_selected_melt$id==135,"peak2"], yend = 0), arrow = arrow(length = unit(0.25, "cm")))  + geom_segment(aes(x = spectrum_selected_melt[spectrum_selected_melt$id==135,"peak3"], y = 0.5, xend = spectrum_selected_melt[spectrum_selected_melt$id==135,"peak3"], yend = 0), arrow = arrow(length = unit(0.25, "cm")))  








