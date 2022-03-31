# Calculate modeling performance ----
performance <- function(pred, actual, cat){
  
  # Descriptions: This function is used to show the performance of regression methods. Several performance indicators 
  #               are calculated: R2,RMSEP,RPIQ,RPD
  #
  # Inputs: pred: the prediction values
  #         actual: the observed/measured values
  #         cat: the name of regression method used, which will be used as row name in output
  #
  # Outputs: A dataframe contains all indicators
  
  library(chillR)
  
  R2 <- cor(pred, actual) ^ 2
  RMSEP <- sqrt(mean((actual - pred)^2))
  RPIQ <- RPIQ(pred, actual)
  RPD <- RPD(pred, actual)
  temp <- data.frame(R2, RMSEP, RPD, RPIQ)
  row.names(temp) <- c(cat)
  return(temp)
}
