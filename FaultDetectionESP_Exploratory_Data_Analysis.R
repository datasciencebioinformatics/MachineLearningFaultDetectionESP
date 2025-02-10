#########################################################################################################
# The spectrum and features tables were downloaded from https://github.com/NINFA-UFES/ESPset            #
######################################################################################################
# Subset spectrum data
spectrum_features_data<-spectrum_features_merged #[,c(1:100)]
# 1.1 Exploratory Data Analysis
# PCA example: analysis of spectral data
spectrum_features_data<-spectrum_features_data[,-which(colnames(spectrum_features_data) %in% c("id","esp_id","label"))]

# center and scale the data before
# calculation the components
model.pca <- prcomp(spectrum_features_data,center = TRUE, scale =TRUE, rank. = 4)

# Display summary of
summary(model.pca)

# Plot pca's
PCA_of_spectral_data_label<-autoplot(model.pca, data = spectrum_features_merged, colour = 'label') + theme_bw() 
PCA_of_spectral_data_esp_id<-autoplot(model.pca, data = spectrum_features_merged, colour = 'esp_id') + theme_bw()

# FindClusters_resolution               
png(filename=paste(output_dir,"Plot_PCA_of_spectral_data.png",sep=""), width = 20, height = 20, res=600, units = "cm")  
  grid.arrange(PCA_of_spectral_data_label, PCA_of_spectral_data_esp_id, P3, ncol = 2, nrow = 1)
dev.off()

