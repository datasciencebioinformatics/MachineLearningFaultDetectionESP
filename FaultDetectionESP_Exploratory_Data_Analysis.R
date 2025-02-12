#########################################################################################################
# The spectrum and features tables were downloaded from https://github.com/NINFA-UFES/ESPset            #
#########################################################################################################
# 1.1    Exploratory Data Analysis
# 1.1.1  PCA example: analysis of spectral data
# Ouptput 1 : Frequency vs. Amplitude : Plot_raw_vibration_data.png
# Ouptput 2 : PCA data per conditions : Plot_PCA_of_spectral_data.png
#########################################################################################################
# Add a collumn with the esp_id combined with the label
spectrum_features_merged$esp_id_label<-paste(spectrum_features_merged$label,spectrum_features_merged$esp_id,sep="_")

# The spectrum_signals table must be melt. 
# The id must be kept to identity each signal.
# Melt by multiple ids
melt_spectrum_signals<-melt(spectrum_features_merged,id=c("id","esp_id","label","esp_id_label"))

# Rename collumn
colnames(melt_spectrum_signals)<-c("id","esp_id","label","esp_id_label","frequency_id","amplitude")

# Each line represents a signal.
# For each the 6032 vibration signals , there are 12103 collumns. Each collumn represents the amplitude.
# Therefore, two collumns are needed, x for the singal and y for the amplitude.

# Plot the raw data
ggplot2_raw_data<-ggplot(data = melt_spectrum_signals, aes(x = as.integer(frequency_id), y = amplitude,colour = factor(esp_id)))+ geom_line(aes(group=id))+ facet_grid(vars(label)) + theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  + ylim(min(melt_spectrum_signals$amplitude), 100) + ggtitle("Raw data") + xlim(min(as.integer(melt_spectrum_signals$frequency_id)), max(as.integer(melt_spectrum_signals$frequency_id)))

# Plot_raw_vibration_data.png               
png(filename=paste(output_dir,"Plot_raw_vibration_data.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  ggplot2_raw_data
dev.off()

# Subset spectrum data
spectrum_features_data<-spectrum_features_merged

# Rename collumns of spectrum data
spectrum_features_data<-spectrum_features_data[,-which(colnames(spectrum_features_data) %in% c("id","esp_id","label","esp_id_label","esp_id_str"))]

# center and scale the data before
# calculation the components
model.pca <- prcomp(spectrum_features_data,center = FALSE, scale =FALSE, rank. = 4)

# Display summary of
summary(model.pca)

# Add collumns to esp_id as string
spectrum_features_merged$esp_id_str<-paste(spectrum_features_merged$esp_id)

# Plot pca's
PCA_of_spectral_data_label        <-autoplot(model.pca, data = spectrum_features_merged, colour = 'label') + theme_bw() 
PCA_of_spectral_data_esp_id       <-autoplot(model.pca, data = spectrum_features_merged, colour = 'esp_id_str') + theme_bw()
PCA_of_spectral_data_esp_id_label <-autoplot(model.pca, data = spectrum_features_merged, colour = 'esp_id_label') + theme_bw()

# FindClusters_resolution               
png(filename=paste(output_dir,"Plot_raw_PCA_of_spectral_data.png",sep=""), width = 40, height = 25, res=600, units = "cm")  
  grid.arrange(PCA_of_spectral_data_label, PCA_of_spectral_data_esp_id,PCA_of_spectral_data_esp_id_label, ncol = 3, nrow = 1, top = "Raw data") 
dev.off()
###########################################################################
# 1.1.2  PCA example: analysis of spectral data averaged per equipment
# The amplitude of each equipment will be averaged per equipment.
# Expected to find a more clear peaks, as shown in Figure 1 of:
# https://onlinelibrary.wiley.com/doi/pdf/10.1002/%28SICI%291522-2594%28199903%2941%3A3%3C450%3A%3AAID-MRM4%3E3.0.CO%3B2-9
# For each equipment, the average of amplitude will be calculated per frequency_id
# From the spectrum_features_data, take the frequency_id and the esp_id

# First the frequency_id in the colnames
frequency_ids_vector<- sort(colnames(spectrum_features_data))

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
ggplot2_raw_data<-ggplot(data = melt_spectrum_signals, aes(x = as.integer(frequency_id), y = amplitude,colour = factor(label)))+ geom_line(aes(group=esp_with_label_id))+ theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  + ylim(min(melt_spectrum_signals$amplitude), 100) + ggtitle("Average data (per esp and condition)") + xlim(min(as.integer(melt_spectrum_signals$frequency_id)), max(as.integer(melt_spectrum_signals$frequency_id))) + facet_grid(vars(label))

# Plot_raw_vibration_data.png              
png(filename=paste(output_dir,"Plot_average_vibration_data.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  ggplot2_raw_data
dev.off()
######################################################################################################
# 1.1.4  PCA example: analysis of spectral data after empiric mode decomposition
# Several papers point to empirical model decomposition.
# Expected to find reduction in the complexity of the singals (averaging, smothing, etc, etc).
# Os dados podem ser tratados com  o método decomposição em modo empírico (emp) [https://www.mdpi.com/2071-1050/14/16/9870, https://www.sciencedirect.com/science/article/abs/pii/S2949891024006493, https://www.mdpi.com/2071-1050/14/16/9870, https://www.mdpi.com/2071-1050/14/16/9870]. Um pacote R já foi selecionado para aplicar a emp no ESPSet [emd: Decomposição em modo empírico]. Eu vou estudar e testar esse pacote R (emp), e reporto progresso,
# https://rdrr.io/cran/EMD/man/semd.html
# To do: study empirical model decompositions
# Tesday work from home : 1.1.2 and 1.1.4 and take study questions.
# Wednesday : library horto in the afternoon.
# xt2 observation or signal observed at time tt
# The amplitude will be used. 
frequency_id<-colnames(spectrum_features_merged[,-which(colnames(spectrum_features_merged) %in% c("id","esp_id","label","esp_id_label","esp_id_str"))])

# Instance a table with zero 
df_results_imf_emd     <-spectrum_features_merged[rownames(spectrum_features_merged),frequency_id]*0
df_results_residue_emd <-spectrum_features_merged[rownames(spectrum_features_merged),frequency_id]*0
df_results_residue_nimf<-spectrum_features_merged[rownames(spectrum_features_merged),frequency_id]*0

# For each signal, the amplitude is taken for all frequency_id
for (signal in rownames(spectrum_features_merged))
{
  # Take all the 
  emd_signal_x<-as.vector(emd(as.numeric(spectrum_features_merged[signal,frequency_id]),as.integer(frequency_id), boundary="wave"))

  # Store results in the data.frame
  df_results_imf_emd[signal,frequency_id]<-emd_signal_x$imf
  df_results_residue_emd[signal,frequency_id]<-emd_signal_x$residue
  df_results_residue_nimf[signal,frequency_id]<-emd_signal_x$nimf
}
# tt2, observation index or time index.
# The frenquecy index will be used.

# specifies boundary condition from ``none", ``wave", ``symmetric", ``periodic" or ``evenodd". 
# See Zeng and He (2004) for evenodd boundary condition.
# The boundary ``none"      will be used.
# The boundary ``wave"      will be tested.
# The boundary ``symmetric"  will be tested.

# Load the spectrum file
imf_emd_signals=df_results_imf_emd
residue_emd_signals=df_results_residue_emd
residue_nimf_signals=df_results_residue_nimf

# Re-set the colnames to numbers
colnames(imf_emd_signals)<-frequency_id
colnames(residue_emd_signals)<-frequency_id
colnames(residue_nimf_signals)<-frequency_id

# Take the ids as the rownames
imf_emd_signals$id<-as.integer(rownames(imf_emd_signals))
residue_emd_signals$id<-as.integer(rownames(residue_emd_signals))
residue_nimf_signals$id<-as.integer(rownames(residue_nimf_signals))

# Spectrum and features merged
# In this table I have the signals and also the id, the esp_id and label
imf_emd_features_merged      <-merge(imf_emd_signals,features_signals[,c("id","esp_id","label")],by="id")
residue_emd_features_merged  <-merge(residue_emd_signals,features_signals[,c("id","esp_id","label")],by="id")
residue_nimf_features_merged <-merge(residue_nimf_signals,features_signals[,c("id","esp_id","label")],by="id")

# Add a collumn with the esp_id combined with the label
imf_emd_features_merged$esp_id_label<-paste(imf_emd_features_merged$label,imf_emd_features_merged$esp_id,sep="_")
residue_emd_features_merged$esp_id_label<-paste(residue_emd_features_merged$label,residue_emd_features_merged$esp_id,sep="_")
residue_nimf_features_merged$esp_id_label<-paste(residue_nimf_features_merged$label,residue_nimf_features_merged$esp_id,sep="_") 
######################################################################################################
# Questions to follow about features.csv:
# peak1x and peak2x, median(8,13) and median(98,102), rms(98,102), coefficients a and Coefficients b are shown. 
# Can we use the summary variables to descriminate conditions or cluster the groups?
# To do : 
# PCA using all these variables accross conditions, ESPs and combinations of them
# Take the ids as the rownames
features_signals$esp_id_str<-paste(features_signals$label,features_signals$esp_id,sep="_")

# Subset spectrum data
spectrum_features_data<-features_signals

# Rename collumns of spectrum data
spectrum_features_data<-spectrum_features_data[,-which(colnames(spectrum_features_data) %in% c("id","esp_id","label","esp_id_label","esp_id_str"))]

# center and scale the data before
# calculation the components
model.pca <- prcomp(spectrum_features_data,center = FALSE, scale =FALSE, rank. = 4)

# Display summary of
summary(model.pca)

# Add collumns to esp_id as string
features_signals$esp_id<-paste(features_signals$esp_id)

# Plot pca's
PCA_of_spectral_data_label        <-autoplot(model.pca, data = features_signals, colour = 'label') + theme_bw() 
PCA_of_spectral_data_esp_id       <-autoplot(model.pca, data = features_signals, colour = 'esp_id') + theme_bw()
PCA_of_spectral_data_esp_id_label <-autoplot(model.pca, data = features_signals, colour = 'esp_id_str') + theme_bw()

# FindClusters_resolution               
png(filename=paste(output_dir,"Plot_summary_PCA_of_spectral_data.png",sep=""), width = 40, height = 25, res=600, units = "cm")  
  grid.arrange(PCA_of_spectral_data_label, PCA_of_spectral_data_esp_id,PCA_of_spectral_data_esp_id_label, ncol = 3, nrow = 1, top = "Summary of vibration data") 
dev.off()
