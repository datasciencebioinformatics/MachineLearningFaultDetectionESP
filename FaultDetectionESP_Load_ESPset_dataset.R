#########################################################################################################
# The spectrum and features tables were downloaded from https://github.com/NINFA-UFES/ESPset            #
#########################################################################################################
# Path to the feature file
features_file="/home/felipe/googledrive/MachineLearningFaultDetectionESP/ESPset_dataset/features.csv"

# Path to the spectrum file
spectrum_file="/home/felipe/googledrive/MachineLearningFaultDetectionESP/ESPset_dataset/spectrum.csv"
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
# Table with the number of signal collected per equipment and per condition
# table(spectrum_features_merged$esp_id,spectrum_features_merged$label)
#
#     Faulty sensor Misalignment Normal Rubbing Unbalance
#  0              0            0    201       0        51
#  1             23            0    644      46        34
#  2             10            0    277      12         1
#  3             11            0    347      56         6
#  4             60           52    578       7       103
#  5             79            0    614      79        58
#  6              4            0    272       0       102
#  7              0            0    611       0        72
#  8            108            0    309       0        69
#  9              0           18    308      22        73
#  10             0            0    639      68         7
#########################################################################################################
# When needed, a procedure to subset Normal samples to reduce computational complexity
## Split normal samples from the other samples
#spectrum_features_merged_normal_samples <-spectrum_features_merged[spectrum_features_merged$label=="Normal",]
#spectrum_features_merged_except_samples <-spectrum_features_merged[spectrum_features_merged$label!="Normal",]

## Merge back the two data.frames
#spectrum_features_merged<-rbind(sample_n(spectrum_features_merged_normal_samples, 100),spectrum_features_merged_except_samples)
#########################################################################################################




