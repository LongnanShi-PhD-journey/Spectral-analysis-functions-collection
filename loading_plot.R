# Loading plot function
loading_plot <- function(df, PC_num, indicator){
  
  # Descriptions: This function is used to plot loading plot of select PC, and identify peaks and valleys
  #
  # Input: df: The dataframe of PCs loading, containing information about wavenumber
  #        PC_num: selected PC, in string, e.g. 'PC1'
  #        indicator: The dataframe including explained variance, obtained by following code 
  #                   indicator<- as.data.frame(summary(MIR_pca)[[6]][,1:10])
  #
  # Output: Loading plot
  
  library(ggplot2)
  library(photobiology)
  
  temp <- df[colnames(df) == PC_num] 
  peaks <- as.numeric(rownames(df)[find_peaks(unlist(c(temp[1])), span=99, ignore_threshold = 0.8)]) # find peaks
  valleys <- as.numeric(rownames(df)[find_peaks(unlist(c(-temp[1])), span=49, ignore_threshold = 0.5)]) # find valleys
  
  peaks_y = temp[rownames(df) %in% peaks,] # return wavenumber of peaks
  valleys_y = c(temp[rownames(df) %in% valleys,]) # return wavenumber of valleys

  annotation_peaks <- data.frame(x = peaks, y = peaks_y+0.01,label = paste(round(peaks))) # set peaks dataframe for plot
  annotation_valleys <- data.frame(x = valleys, y = valleys_y-0.01,label = paste(round(valleys))) # set valleys dataframe for plot

  # line plot with horizontal line(y=0)
  loading <- ggplot(data=df, aes_string(x=as.numeric(rownames(df)), y=PC_num))+
    geom_line()+
    labs(x = "Wavenumber /cm")+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())+
    geom_hline(yintercept=0, linetype="dashed", color = "red")+
    scale_x_reverse(limits = c(4000, 600))
  
  # add peaks on plot
  loading<- loading + 
    geom_text(data=annotation_peaks, aes(x=x, y=y, label=label),
              color="red", size=4)
  # add valleys on plot
  loading<- loading + 
    geom_text(data=annotation_valleys, aes(x=x, y=y, label=label),
              color="blue", size=4)
  
  # add ylab
  loading <- loading + 
    ylab(paste(PC_num, ':', scales::percent(indicator['Proportion of Variance',paste(PC_num)])))
  
  # set text size
  loading <- loading +
    theme(legend.text=element_text(size=12), legend.title = element_text(15),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12))
  
  return(loading)
}
