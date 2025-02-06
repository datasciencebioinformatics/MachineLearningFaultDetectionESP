#########################################################################################################
# The spectrum and features tables were downloaded from https://github.com/NINFA-UFES/ESPset            #
#########################################################################################################
# Path to the feature file
features_file="/home/felipe/googledrive/MachineLearningFaultDetectionESP/ESPset_dataset/features.csv"

# Path to the spectrum file
spectrum_file="/home/felipe/googledrive/MachineLearningFaultDetectionESP/ESPset_dataset/spectrum.csv"
#########################################################################################################
# Load the spectrum file
spectrum_signals=read.csv(spectrum_file, fill = TRUE, header = FALSE, sep=";")

# Load the features file
features_signals=read.csv(features_file, fill = TRUE, header = TRUE, sep=";")

# Re-set the colnames to numbers
colnames(spectrum_signals)<-1:length(colnames(spectrum_signals))

# Take the ids as the rownames
spectrum_signals$id<-rownames(spectrum_signals)

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
# The spectrum_signals table must be melt. 
# The id must be kept to identity each signal.
# I stopped here: it is needed to melt the table and keep the three variables (id,esp_id, and label) 
melt_spectrum_signals<-melt(spectrum_features_merged,by=c("id","esp_id","label"))

colnames(melt_spectrum_signals)<-c("signal","label","amplitude")
                                   
# Each line represents a signal.
# For each the 6032 vibration signals , there are 12103 collumns. Each collumn represents the amplitude.
# Therefore, two collumns are needed, x for the singal and y for the amplitude.
df_singal_amplitude<-data.frame(signal=melt_spectrum_signals$ids,amplitude=melt_spectrum_signals$value,frequency=melt_spectrum_signals$variable)

ggplot(data = melt_spectrum_signals, aes(x = variable, y = value))+ geom_line(aes(group=ids))
