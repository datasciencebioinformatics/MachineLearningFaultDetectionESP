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
STARTING_IDX_POS     = 001
ENDING_IDX_POS     = 500

# Frequency_id
frequency_id<-frequency_id[STARTING_IDX_POS:ENDING_IDX_POS]

#####################################################################################################################################
# Start a template data.frame
# Vector to store amplitude for the frequency vector
amplitude_vector<-spectrum_features_merged[1,frequency_id]

# Add the results for the signal
# It takes the amplitude vector as input (time-series) and calculate w, min, max, mean, median, sd and stat
# this for an interval of size w
SlidingWindows<-descritive.SlidingWindows(as.vector(unlist(amplitude_vector)), w = 50, skewness = "moment", kurtosis = "moment")

# Template data.frame
df_min       <- matrix(0, ncol = length(SlidingWindows$min), nrow = length(unique(spectrum_features_merged$id)))
df_max       <- matrix(0, ncol = length(SlidingWindows$min), nrow = length(unique(spectrum_features_merged$id)))
df_mean      <- matrix(0, ncol = length(SlidingWindows$min), nrow = length(unique(spectrum_features_merged$id)))
df_median    <- matrix(0, ncol = length(SlidingWindows$min), nrow = length(unique(spectrum_features_merged$id)))
df_sd        <- matrix(0, ncol = length(SlidingWindows$min), nrow = length(unique(spectrum_features_merged$id)))
df_skewness  <- matrix(0, ncol = length(SlidingWindows$min), nrow = length(unique(spectrum_features_merged$id)))
df_kurtosis  <- matrix(0, ncol = length(SlidingWindows$min), nrow = length(unique(spectrum_features_merged$id)))
#####################################################################################################################################
# For each signal, the amplitude is taken for all frequency_id
for (signal_id in rownames(spectrum_features_merged))
{
  print(as.integer(signal_id))
  
  # Vector to store amplitude for the frequency vector
  amplitude_vector<-spectrum_features_merged[signal_id,frequency_id]

  # Add the results for the signal
  # It takes the amplitude vector as input (time-series) and calculate w, min, max, mean, median, sd and stat
  # this for an interval of size w
  SlidingWindows<-descritive.SlidingWindows(as.vector(unlist(amplitude_vector)), w = 50, skewness = "moment", kurtosis = "moment")

  # statistical indicators  
  df_min[as.integer(signal_id),]<-as.vector(SlidingWindows$min)
  df_max[as.integer(signal_id),]<-as.vector(SlidingWindows$max)
  df_mean[as.integer(signal_id),]<-as.vector(SlidingWindows$mean)
  df_median[as.integer(signal_id),]<-as.vector(SlidingWindows$median)
  df_sd[as.integer(signal_id),]<-as.vector(SlidingWindows$sd)
  df_skewness[as.integer(signal_id),]<-as.vector(SlidingWindows$skewness)
  df_kurtosis[as.integer(signal_id),]<-as.vector(SlidingWindows$kurtosis)
  
}
df_min<-data.frame(df_min)
df_max<-data.frame(df_max)
df_mean<-data.frame(df_mean)
df_median<-data.frame(df_median)
df_sd<-data.frame(df_sd)
df_skewness<-data.frame(df_skewness)
df_kurtosis<-data.frame(df_kurtosis)

# Preparat1ion of data.frame with the statistical indicators
# Set rownames
rownames(df_min)       <-spectrum_features_merged$signal_id[1:length(rownames(df_min))]
rownames(df_max)       <-spectrum_features_merged$signal_id[1:length(rownames(df_max))]
rownames(df_mean)      <-spectrum_features_merged$signal_id[1:length(rownames(df_mean))]
rownames(df_median)    <-spectrum_features_merged$signal_id[1:length(rownames(df_median))]
rownames(df_sd)        <-spectrum_features_merged$signal_id[1:length(rownames(df_sd))]
rownames(df_skewness)  <-spectrum_features_merged$signal_id[1:length(rownames(df_skewness))]
rownames(df_kurtosis)  <-spectrum_features_merged$signal_id[1:length(rownames(df_kurtosis))]

# Set colnames
colnames(df_min)       <- 1:dim(df_min)[2]
colnames(df_max)       <- 1:dim(df_max)[2]
colnames(df_mean)      <- 1:dim(df_mean)[2]
colnames(df_median)    <- 1:dim(df_median)[2]
colnames(df_sd)        <- 1:dim(df_sd)[2]
colnames(df_skewness)  <- 1:dim(df_skewness)[2]
colnames(df_kurtosis)  <- 1:dim(df_min)[2]


# Add collumns data.frame 
df_min          <-cbind(df_min,data.frame(id=rownames(df_min)))
df_max          <-cbind(df_max,data.frame(id=rownames(df_max)))
df_mean         <-cbind(df_mean,data.frame(id=rownames(df_mean)))
df_median       <-cbind(df_median,data.frame(id=rownames(df_median)))
df_sd           <-cbind(df_sd,data.frame(id=rownames(df_sd)))
df_skewness     <-cbind(df_skewness,data.frame(id=rownames(df_skewness)))
df_kurtosis     <-cbind(df_kurtosis,data.frame(id=rownames(df_kurtosis)))

                    
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

# One painel with the line plots for all the measures min,max,mean,median,skewness,kurtosis
# one measure per plot in the panel.
melt_df_min     <-melt(df_min)
melt_df_max     <-melt(df_max)
melt_df_sd      <-melt(df_sd)
melt_df_mean    <-melt(df_mean)
melt_df_median  <-melt(df_median)
melt_df_skewness<-melt(df_skewness)
melt_df_kurtosis<-melt(df_kurtosis)

# Set the colnames
colnames(melt_df_min)      <-c("id","sw_id","value")
colnames(melt_df_max)      <-c("id","sw_id","value")
colnames(melt_df_sd)       <-c("id","sw_id","value")
colnames(melt_df_mean)     <-c("id","sw_id","value")
colnames(melt_df_median)   <-c("id","sw_id","value")
colnames(melt_df_skewness) <-c("id","sw_id","value")
colnames(melt_df_kurtosis) <-c("id","sw_id","value")

# Add a collumn to set the type of metric
# Add a collumn to set the type of metric
melt_df_min$metric        <-"min"
melt_df_max$metric        <-"max"
melt_df_mean$metric       <-"mean"
melt_df_median$metric     <-"median"
melt_df_sd$metric         <-"sd"
melt_df_skewness$metric   <-"skewness"
melt_df_kurtosis$metric   <-"kurtosis"

# Melt all the metric in one
melt_df_metrics<-rbind(melt_df_min,melt_df_max,melt_df_mean,melt_df_median,melt_df_sd,melt_df_skewness,melt_df_kurtosis)
#########################################################################################################
# One painel with the pca plots for all the measures min,max,mean,median,skewness,kurtosis
# one measure per plot in the panel.

#########################################################################################################
# Plot the raw data
ggplot2_raw_data<-ggplot(data = melt_df_metrics, aes(x = as.integer(sw_id), y = value))+ geom_line(aes(group=id))+ facet_grid(vars(metric), scales="free") + theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + ggtitle("Descritive statistics with sliding windows")

# Plot_raw_vibration_data.png               
png(filename=paste(output_dir,"Plot_raw_statistical_indicators.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  ggplot2_raw_data
dev.off()

#########################################################################################################
id_min<-unique(intersect(features_signals$id,na.omit(df_min$id)))
id_max<-unique(intersect(features_signals$id,na.omit(df_max$id)))
id_median<-unique(intersect(features_signals$id,na.omit(df_median$id)))
id_mean<-unique(intersect(features_signals$id,na.omit(df_mean$id)))
id_sd<-unique(intersect(features_signals$id,na.omit(df_sd$id)))
id_skewness<-unique(intersect(features_signals$id,na.omit(df_skewness$id)))
id_kurtosis<-unique(intersect(features_signals$id,na.omit(df_kurtosis$id)))

features_signals_min<-features_signals[features_signals$id %in% id_min,]
features_signals_max<-features_signals[features_signals$id %in% id_max,]
features_signals_median<-features_signals[features_signals$id %in% id_median,]
features_signals_mean<-features_signals[features_signals$id %in% id_mean,]
features_signals_sd<-features_signals[features_signals$id %in% id_sd,]
features_signals_skewness<-features_signals[features_signals$id %in% id_skewness,]
features_signals_kurtosis<-features_signals[features_signals$id %in% id_kurtosis,]

# center and scale the data before
df_min<-na.omit(df_min[,1:length(SlidingWindows$min)])
df_max<-na.omit(df_max[,1:length(SlidingWindows$max)])
df_median<-na.omit(df_median[,1:length(SlidingWindows$median)])
df_mean<-na.omit(df_mean[,1:length(SlidingWindows$mean)])
df_sd<-na.omit(df_sd[,1:length(SlidingWindows$sd)])
df_skewness<-na.omit(df_skewness[,1:length(SlidingWindows$skewness)])
df_kurtosis<-na.omit(df_kurtosis[,1:length(SlidingWindows$kurtosis)])


# calculation the components
model.pca.min          <- prcomp(df_min,center = FALSE, scale =FALSE, rank. = 4)
model.pca.max          <- prcomp(df_max,center = FALSE, scale =FALSE, rank. = 4)
model.pca.median       <- prcomp(df_median,center = FALSE, scale =FALSE, rank. = 4)
model.pca.mean         <- prcomp(df_mean,center = FALSE, scale =FALSE, rank. = 4)
model.pca.sd           <- prcomp(df_sd,center = FALSE, scale =FALSE, rank. = 4)
model.pca.skewness     <- prcomp(df_skewness,center = FALSE, scale =FALSE, rank. = 4)
model.pca.kurtosis     <- prcomp(df_kurtosis,center = FALSE, scale =FALSE, rank. = 4)

# Plot pca's
PCA_of_min        <-autoplot(model.pca.min, data = features_signals_min, colour = 'label') + theme_bw() + ggtitle("min")
PCA_of_max        <-autoplot(model.pca.max, data = features_signals_max, colour = 'label') + theme_bw()  + ggtitle("max")
PCA_of_median     <-autoplot(model.pca.median, data = features_signals_median, colour = 'label') + theme_bw() + ggtitle("median")
PCA_of_mean       <-autoplot(model.pca.mean, data = features_signals_mean, colour = 'label') + theme_bw()  + ggtitle("mean")
PCA_of_sd         <-autoplot(model.pca.sd, data = features_signals_sd, colour = 'label') + theme_bw()  + ggtitle("sd")
PCA_of_skewness   <-autoplot(model.pca.skewness, data = features_signals_skewness, colour = 'label') + theme_bw()  + ggtitle("skewness")
PCA_of_kurtosis   <-autoplot(model.pca.kurtosis, data = features_signals_kurtosis, colour = 'label') + theme_bw()  + ggtitle("kurtosis")


# Plot_raw_vibration_data.png               
png(filename=paste(output_dir,"Plot_pca_statistical indicators.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  grid.arrange(PCA_of_min,PCA_of_max,PCA_of_median,PCA_of_mean,PCA_of_sd,PCA_of_skewness,PCA_of_kurtosis, ncol = 3, nrow = 3, top = "Descritive statistics with sliding windows")
dev.off()
#########################################################################################################



