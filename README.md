# MachineLearningFaultDetectionESP
Intelligent systems for fault detection and diagnosis of submersible equipments.

## ESPset dataset description [1]
The data is downloaded from https://github.com/NINFA-UFES/ESPset. The dataset is provided in two files: spectrum.csv and features.csv.

### features.csv
esp_id: The id of the ESP.
label: The classification label.
median(8,13): Median of the amplitudes in the interval (8% X, 13% X);
median(98,102): Median of the amplitudes in the interval (98% X, 102% X);
a: Coefficient a of the exponential regression of type e (a⋅A+b) where A is an array of equally separated relative frequencies up to 0.4
X, excluding zero. Example: A=(0.01, 0.02, ..., 0.39, 0.4).
b: Coefficient b of the exponential regression of type e(a⋅A+b) where A is an array of equally separated relative frequencies up to 0.4 X, excluding zero. Example: A =
(0.01, 0.02, ..., 0.39, 0.4).
peak1x: Amplitude in X;
peak2x: Amplitude in 2;
rms(98,102): Root mean square of the amplitudes in the interval (98% X, 102% X).

### spectrum.csv
This csv file is a matrix of 6032 lines and 12103 columns, whose values are float numbers seperated by a ';'. Each line of this file contains the spectrum of a single vibration signal collected from a sensor at a specific test condition of the ESP. Each value is the amplitude in inches per second (velocity) at a specific frequency. Each signal is normalized by the rotation frequency in which the ESP operates, in such a way that the amplitude with respect to the rotation frequency is always at the same position for all signal arrays.

**[1]** Varejão F. M., Mello L. H. S., Ribeiro M. P., Oliveira-Santos T. ,Rodrigues A. L.. An open source experimental framework and public dataset for vibration-based fault diagnosis of electrical submersible pumps used on offshore oil exploration. Volume 288, 15 March 2024, 111452.
