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
png(filename=paste(output_dir,"Plot_PCA_of_spectral_data.png",sep=""), width = 20, height = 10, res=600, units = "cm")  
  grid.arrange(PCA_of_spectral_data_label, PCA_of_spectral_data_esp_id, ncol = 2, nrow = 1, top = "Raw data") 
dev.off()
######################################################################################################
# 1.1.2  PCA example: analysis of spectral data averaged per equipment
# The amplitude of each equipment will be averaged per equipment.
# Expected to find a more clear peaks, as shown in Figure 1 of:
# https://onlinelibrary.wiley.com/doi/pdf/10.1002/%28SICI%291522-2594%28199903%2941%3A3%3C450%3A%3AAID-MRM4%3E3.0.CO%3B2-9
# For each equipment, the average of amplitude will be calculated per frequency_id
######################################################################################################
# 1.1.3  PCA example: analysis of spectral data after eigen decomposition
# This can be tried following this simple tutorial: https://www.geeksforgeeks.org/ml-spectral-clustering/
# Question is is I can use eigen values on the spectral data or if I use it on the time series.
######################################################################################################
# 1.1.4  PCA example: analysis of spectral data after empiric mode decomposition
# Several papers point to empirical model decomposition.
# Expected to find reduction in the complexity of the singals (averaging, smothing, etc, etc).
# Os dados podem ser tratados com  o método decomposição em modo empírico (emp) [https://www.mdpi.com/2071-1050/14/16/9870, https://www.sciencedirect.com/science/article/abs/pii/S2949891024006493, https://www.mdpi.com/2071-1050/14/16/9870, https://www.mdpi.com/2071-1050/14/16/9870]. Um pacote R já foi selecionado para aplicar a emp no ESPSet [emd: Decomposição em modo empírico]. Eu vou estudar e testar esse pacote R (emp), e reporto progresso,
######################################################################################################

