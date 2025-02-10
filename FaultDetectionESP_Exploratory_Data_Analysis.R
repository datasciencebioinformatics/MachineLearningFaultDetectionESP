#########################################################################################################
# The spectrum and features tables were downloaded from https://github.com/NINFA-UFES/ESPset            #
#########################################################################################################
# 1.1    Exploratory Data Analysis
# 1.1.1  PCA example: analysis of spectral data
#########################################################################################################
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

