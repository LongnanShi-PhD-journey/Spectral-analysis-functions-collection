# Biplot of PCA
biplot_PCA <- function(df, a,b, cat, indicator){
  
  # Descriptions: This function is used to plot the biplot.
  #               To identify the variability of spectral data with specific soil 
  #               properties
  #
  # Input: df: The dataframe of chemical data and scores
  #        a: PC, in string. e.g. 'PC1'
  #        b: PC, in string. e.g. 'PC1'
  #        cat: Category column used for plot
  #        indicator: The dataframe including explained variance, obtained by following code 
  #                   indicator<- as.data.frame(summary(PCA_result)[[6]][,1:10])
  #
  # Output: Figures with biplots and Loading plot
  
  library(ggplot2)
  
  # plot scatter points, vertical(x=0) and horizontal(y=0) line
  biplot <- ggplot(df, aes_string(x=a, y=b, color = cat))+
    geom_point(size = 2.0, shape = 16)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())+
    geom_hline(yintercept=0, linetype="dashed", color = "red")+
    geom_vline(xintercept=0, linetype="dashed", color = "red")
  
  # add xlab and ylab with explained variance
  biplot <- biplot +
    xlab(paste(a, ':', scales::percent(indicator['Proportion of Variance',a]))) +
    ylab(paste(b, ':', scales::percent(indicator['Proportion of Variance',b])))
  
  # Set text size
  biplot <- biplot + 
    theme(legend.text=element_text(size=12), legend.title = element_text(15),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 12))
  
  return(biplot)
}
