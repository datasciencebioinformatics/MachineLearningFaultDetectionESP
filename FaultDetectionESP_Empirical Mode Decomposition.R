######################################################################################################
# 1.1.4  PCA example: analysis of spectral data after empiric mode decomposition
# Several papers point to empirical model decomposition.
# Expected to find reduction in the complexity of the singals (averaging, smothing, etc, etc).
# Os dados podem ser tratados com  o método decomposição em modo empírico (emp) [https://www.mdpi.com/2071-1050/14/16/9870, https://www.sciencedirect.com/science/article/abs/pii/S2949891024006493, https://www.mdpi.com/2071-1050/14/16/9870, https://www.mdpi.com/2071-1050/14/16/9870]. Um pacote R já foi selecionado para aplicar a emp no ESPSet [emd: Decomposição em modo empírico]. Eu vou estudar e testar esse pacote R (emp), e reporto progresso,
# https://rdrr.io/cran/EMD/man/semd.html
# To do: study empirical model decompositions
# Tesday work from home : 1.1.2 and 1.1.4 and take study questions.
# Wednesday : library horto in the afternoon.
# xt2 observation or signal observed at time tt
# The amplitude will be used. 
# Emd function of one specific signal
frequency_id<-colnames(spectrum_features_merged[,-which(colnames(spectrum_features_merged) %in% c("id","esp_id","label","esp_id_label","esp_id_str"))])

# Instance a table with zero 
df_results_signal          <-spectrum_features_merged[rownames(spectrum_features_merged),frequency_id]*0
df_results_imf.1           <-spectrum_features_merged[rownames(spectrum_features_merged),frequency_id]*0
df_results_imf.2           <-spectrum_features_merged[rownames(spectrum_features_merged),frequency_id]*0
df_results_imf.3           <-spectrum_features_merged[rownames(spectrum_features_merged),frequency_id]*0
df_results_imf.4           <-spectrum_features_merged[rownames(spectrum_features_merged),frequency_id]*0
df_results_imf.residue     <-spectrum_features_merged[rownames(spectrum_features_merged),frequency_id]*0

# Initiate a data.frame for the results of all signals
df_results_imf_all_signals=data.frame(Signal=c(),Amplitude=c(),imf.1=c(),imf.2=c(),imf.3=c(),imf.4=c(),imf.5=c(),imf.6=c(),residue=c())

# For each signal, the amplitude is taken for all frequency_id
for (signal in rownames(spectrum_features_merged))
{
  # Take all the 
  emd_signal_x_none    <-as.vector(emd(as.numeric(spectrum_features_merged[signal,frequency_id]),as.integer(frequency_id), boundary="none",max.imf=4))
  
  # Set the vectors for numeric, imf, emd and residual
  df_results_signal         <-as.numeric(spectrum_features_merged[signal,frequency_id])
  df_results_imf.1          <-emd_signal_x_none$imf[,1]
  df_results_imf.2          <-emd_signal_x_none$imf[,2]
  df_results_imf.3          <-emd_signal_x_none$imf[,3]
  df_results_imf.4          <-emd_signal_x_none$imf[,4]
  df_results_imf.residue    <-emd_signal_x_none$residue

  # Compose a data.frame with the variabels  
  df_results_emd<-data.frame(Signal=signal,Amplitude=df_results_signal,imf.1=df_results_imf.1,imf.2=df_results_imf.2,imf.3=df_results_imf.3,imf.4=df_results_imf.4,residue=df_results_imf.residue)                             

  # Concatenate tables
  df_results_imf_all_signals<-rbind(df_results_imf_all_signals,df_results_emd)
}
# Add collumn frequency_id to the table

# Add collumn frequency_id to the table

emd_signal_x    <-as.vector(emd(as.numeric(spectrum_features_merged[signal,frequency_id]),as.integer(frequency_id), boundary="none",max.imf=8))

# Set the vectors for numeric, imf, emd and residual
raw_signal   <-as.numeric(spectrum_features_merged[signal,frequency_id])
imf_signal   <-emd_signal_x$imf
emd_signal   <-emd_signal_x$residue
residue_nimf <-emd_signal_x$nimf

# Compose a data.frame with the variabels
df_results_emd<-data.frame(Amplitude=raw_signal,imf=imf_signal,emd=emd_signal)

# Set the rownames of the data.frame
rownames(df_results_emd)<-frequency_id

# Add esp_id, esp_id, label
df_results_emd$id<-signal
df_results_emd$frequency_id<-frequency_id
df_results_emd$esp_id<-features_signals[signal,c("esp_id")] 
df_results_emd$label<-features_signals[signal,c("label")] 

#########################################################################################################
# The spectrum_signals table must be melt.
# The id must be kept to identity each signal.
# Melt by multiple ids

# The spectrum_signals table must be melt.
# The id must be kept to identity each signal.
# Melt by multiple ids
melt_df_results_emd<-melt(df_results_emd,id=c("id", "esp_id", "label","frequency_id"))

# Plot the emd  
ggplot2_imf_emd_data<-ggplot(data = melt_df_results_emd, aes(x = as.integer(frequency_id), y = value))+ facet_grid(vars(variable)) + theme_bw() + geom_line(aes(group=id)) + ggtitle(paste("emd on sigal", signal, "boundary set to none",sep=" ")) 

# Plot_raw_vibration_data.png              
png(filename=paste(output_dir,"Plot_imf_emd_data.png",sep=""), width = 20, height = 25, res=600, units = "cm")  
  ggplot2_imf_emd_data
dev.off()
