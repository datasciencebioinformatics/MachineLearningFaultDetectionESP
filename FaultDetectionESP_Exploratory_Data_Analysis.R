#########################################################################################################
# The spectrum and features tables were downloaded from https://github.com/NINFA-UFES/ESPset            #
######################################################################################################
spectrum_features_data<-spectrum_features_merged[,c(1:100)]

# 1.1 Exploratory Data Analysis
# PCA example: analysis of spectral data
spectrum_features_data<-spectrum_features_data[,-which(colnames(spectrum_features_data) %in% c("id","esp_id","label"))]

# center and scale the data before
# calculation the components
model.pca <- prcomp(spectrum_features_data,center = TRUE, scale =TRUE, rank. = 4)

# Display summary of
summary(model.pca)

# Plot pca's
autoplot(model.pca, data = spectrum_features_merged, colour = 'label')
autoplot(model.pca, data = spectrum_features_merged, colour = 'esp_id')
