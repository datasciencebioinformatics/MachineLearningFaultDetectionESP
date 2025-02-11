#########################################################################################################
# The spectrum and features tables were downloaded from https://github.com/NINFA-UFES/ESPset            #
#########################################################################################################
# 1.1    Exploratory Data Analysis
# 1.1.1  PCA example: analysis of spectral data
# Ouptput 1 : Frequency vs. Amplitude : Plot_raw_vibration_data.png
# Ouptput 2 : PCA data per conditions : Plot_PCA_of_spectral_data.png
#########################################################################################################
# The spectrum_signals table must be melt. 
# The id must be kept to identity each signal.
# Melt by multiple ids
melt_spectrum_signals<-melt(spectrum_features_merged,id=c("id","esp_id","label"))

# Rename collumn
colnames(melt_spectrum_signals)<-c("id","esp_id","label","frequency_id","amplitude")
                               
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
spectrum_features_data<-spectrum_features_data[,-which(colnames(spectrum_features_data) %in% c("id","esp_id","label"))]

# center and scale the data before
# calculation the components
model.pca <- prcomp(spectrum_features_data,center = FALSE, scale =FALSE, rank. = 4)

# Display summary of
summary(model.pca)

# Plot pca's
PCA_of_spectral_data_label<-autoplot(model.pca, data = spectrum_features_merged, colour = 'label') + theme_bw() 
PCA_of_spectral_data_esp_id<-autoplot(model.pca, data = spectrum_features_merged, colour = 'esp_id') + theme_bw()

# FindClusters_resolution               
png(filename=paste(output_dir,"Plot_raw_PCA_of_spectral_data.png",sep=""), width = 20, height = 10, res=600, units = "cm")  
  grid.arrange(PCA_of_spectral_data_label, PCA_of_spectral_data_esp_id, ncol = 2, nrow = 1, top = "Raw data") 
dev.off()
######################################################################################################
# 1.1.2  PCA example: analysis of spectral data averaged per equipment
# The amplitude of each equipment will be averaged per equipment.
# Expected to find a more clear peaks, as shown in Figure 1 of:
# https://onlinelibrary.wiley.com/doi/pdf/10.1002/%28SICI%291522-2594%28199903%2941%3A3%3C450%3A%3AAID-MRM4%3E3.0.CO%3B2-9
# For each equipment, the average of amplitude will be calculated per frequency_id
# From the spectrum_features_data, take the frequency_id and the esp_id

# Add a collumn with the esp_id combined with the label
spectrum_features_merged$esp_id_label<-paste(spectrum_features_merged$esp_id,spectrum_features_merged$label,sep="_")

# First the frequency_id in the colnames
frequency_ids_vector<- sort(colnames(spectrum_features_data))

# Second the esp_id in the colnames
esp_ids_vector      <- sort(unique(spectrum_features_merged$esp_id))

# Third the esp_id combined with the label
esp_with_label      <- sort(unique(spectrum_features_merged$esp_id_label))

# A data.frame with frequency and esp
df_esp_frequency    <- data.frame(matrix(nrow = length(esp_ids_vector), ncol = length(frequency_ids_vector))) 

# Set the rownames with frtequency names
colnames(df_esp_frequency)<-frequency_ids_vector

# Set the colnames with frtequency names
rownames(df_esp_frequency)<-esp_ids_vector

# For each esp_ids take the average amplitude
for (esp_id in esp_ids_vector)
{ 
  # Take all signals from a specific esp
  signals_from_esp<-spectrum_features_merged[spectrum_features_merged$esp_id %in% esp_id,frequency_ids_vector]
  
  # For each frequency_id
  for (frequency_id in frequency_ids_vector)
  { 
    # Then, take the average of the amplitude for each frequency_id
    mean_of_amplitude<-mean(signals_from_esp[,frequency_id])

    # Mean of ampplitude for the equipment 
    df_esp_frequency[esp_id,frequency_id]<-mean_of_amplitude    
  }    
}

# Set the esp_id
df_esp_frequency$esp_id<-rownames(df_esp_frequency)

# The spectrum_signals table must be melt. 
# The id must be kept to identity each signal.
# Melt by multiple ids
melt_spectrum_signals<-melt(df_esp_frequency,id=c("esp_id"))

# Rename collumn
colnames(melt_spectrum_signals)<-c("esp_id","frequency_id","amplitude")
                               
# Each line represents a signal.
# For each the 6032 vibration signals , there are 12103 collumns. Each collumn represents the amplitude.
# Therefore, two collumns are needed, x for the singal and y for the amplitude.

# Plot the raw data
ggplot2_raw_data<-ggplot(data = melt_spectrum_signals, aes(x = as.integer(frequency_id), y = amplitude,colour = factor(esp_id)))+ geom_line()+ theme_bw() +   theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),    panel.background = element_blank())  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  + ylim(min(melt_spectrum_signals$amplitude), 100) + ggtitle("Average data (per esp)") + xlim(min(as.integer(melt_spectrum_signals$frequency_id)), 50)

# Plot_raw_vibration_data.png               
png(filename=paste(output_dir,"Plot_average_vibration_data.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  ggplot2_raw_data
dev.off()

# Subset spectrum data
spectrum_features_data<-df_esp_frequency

# Rename collumns of spectrum data
spectrum_features_data<-spectrum_features_data[,-which(colnames(spectrum_features_data) %in% c("id","esp_id","label"))]

# center and scale the data before
# calculation the components
model.pca <- prcomp(spectrum_features_data,center = FALSE, scale =FALSE, rank. = 4)

# Display summary of
summary(model.pca)

# Plot pca's
PCA_of_spectral_data_label<-autoplot(model.pca, data = spectrum_features_merged, colour = 'label') + theme_bw() 
PCA_of_spectral_data_esp_id<-autoplot(model.pca, data = spectrum_features_merged, colour = 'esp_id') + theme_bw()

# FindClusters_resolution               
png(filename=paste(output_dir,"Plot_average_PCA_of_spectral_data.png",sep=""), width = 20, height = 10, res=600, units = "cm")  
  grid.arrange(PCA_of_spectral_data_label, PCA_of_spectral_data_esp_id, ncol = 2, nrow = 1, top = "Raw data") 
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
######################################################################################################
