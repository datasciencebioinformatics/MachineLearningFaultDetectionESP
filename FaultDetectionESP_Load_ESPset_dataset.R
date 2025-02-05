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

# Take the ids as the rownames
spectrum_signals$ids<-rownames(spectrum_signals)
#########################################################################################################
# Each line represents a signal.
# For each the 6032 signal, there are 12103 collumns. Each collumn represents the amplitude.
# Therefore, two collumns are needed, x for the singal and y for the amplitude.

# The spectrum_signals table must be melt. 
# The id must be kept to identity each signal.
melt_spectrum_signals<-melt(spectrum_signals,by="ids")

spectrum_signals[which(spectrum_signals$ids==1),]

ggplot(data = melt_spectrum_signals, aes(x = variable, y = value))+ geom_line(aes(group=ids))

