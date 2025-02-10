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
######################################################################################################
# 1.1.3  PCA example: analysis of spectral data after eigen decomposition
######################################################################################################
# 1.1.4  PCA example: analysis of spectral data after empiric mode decomposition
######################################################################################################

