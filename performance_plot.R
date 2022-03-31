# Plot measurments vs predictions after regression----
ggplotRegression <- function (df,  measurements, predictions, title, indicator) {
  
  # Descriptions: This function is used to plot the performance of regression methods. Measurements vs Predictions 
  #
  # Inputs: df: the dataframe conttains prediction values and measurement values
  #         measurements: the column name of measured values, should be in string e.g. 'OC'
  #         predictions: the column name of predicted values, should be in string e.g. 'OC_pred'
  #         title: the title of plot, shoule be in string e.g. 'OC vs OC_pred'
  #         indicator: the performance indicator listed on the plot
  #
  # Outputs: ggplot
  
  library(ggplot2)
  
  plotreg <- ggplot(df, aes_string(x = measurements, y = predictions)) + 
    geom_point()+
    geom_smooth(method='lm')+
    labs(x='Measurements', y='Predictions', title=title)
  annotations <- data.frame(
    X = c(-Inf),
    Y =  c(Inf),
    text = c(paste('','R2:',round(indicator[1],2), '\n', 'RMSEP:',round(indicator[2],2), '\n', 
                 'RPD:',round(indicator[3],2), '\n', 'RPIQ:',round(indicator[4],2), '\n')),
    x_adjust = c(0),
    y_adjust = c(1.2))

  plotreg <- plotreg + 
    geom_text(data=annotations, aes(
    x=X,y=Y,hjust=x_adjust,vjust=y_adjust,label=text), size=4, fontface ='bold')+
    theme_bw() +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
          plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  return(plotreg)
}
