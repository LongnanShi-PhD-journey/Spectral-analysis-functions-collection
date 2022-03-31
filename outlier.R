# Outliers detection via Mahalobinas distance----

outlier <- function (data, df, cutoff= 0.95, ifplot = FALSE){
  
  # Descriptions: This function is created to detect outliers based on selected PCs and Mahalobinas distance, The  
  #               Chi-square distribution is used to set the cut-off value and filer observations
  #
  # Inputs: data: the dataset contains selected PCs, should be dataframe
  #            df: degree of freedom for Chi-square distribution, should be integer depends on how many PCs are used
  #            cutoff: set the cut-off value to determine outlier, the default value is 0.95, which means keep 95% and left 5% is outlier
  #            ifplot: if plot scatter plot to show outliers, default value is FALSE
  #
  # Outputs: key: a vector contains sample codes for outlier

  library(stats)
  library(ggplot2)
  library(ggpubr)

  mean_pcaA <- (colMeans(data[,1:df])) # calculate the mean of each col for each PCs
  cov_pcaA <- cov(data[,1:df]) # calculate the covariance of selected PCs
  chiMat <- matrix(NA, ncol = 3, nrow = nrow(data)) # Create a new empty matrix, dimensional is (3*number of observations)
  chiMat[, 1] <- mahalanobis(data, mean_pcaA, cov_pcaA) # calculate mahalanobis distance and fill in the first column in created empty matrix
  
  # For dataset with large number of samples the observed Mahalanobis distances have an approximate chi-square distribution 
  
  chiMat[, 2] <- pchisq(c(chiMat[, 1]), df = 5) # calculate the percentile of each mahalanobis distance under chi-square distribution 
  # pcrit <- 1 - ((0.24 - 0.003*5)/sqrt(nrow(ISIS_chem$MIR_sg_mc_2))) # calculate the cut-off value
  pcrit <- qchisq(p = cutoff , df = 5) # calculate the cut-off value
  for (i in 1:nrow(chiMat)) {
    if (chiMat[i, 1] >= pcrit) {
      chiMat[i, 3] <- 1
    } else {
      chiMat[i, 3] <- 0
    }
  }
  
  chiMat <- as.data.frame(chiMat)
  colnames(chiMat) <- c('Maha_distance','Chi_percentile','if_outliers')
  chiMat$if_outliers <- as.factor(chiMat$if_outliers)
  
  if (ifplot == TRUE){
    
    temp_1 <- ggplot(chiMat, aes(x=Maha_distance))+
      geom_histogram(aes(y=..density..), colour="black", fill="deepskyblue3",alpha=0.5, bins=50)+
      geom_vline(aes(xintercept=pcrit), color="red", linetype="dashed",size=1)+
      geom_text(aes(x=pcrit, label=paste('Cut-off Value:', cutoff), y=0.1), colour="Red", angle=90, vjust = 1.2, size=5)+
      theme_bw() +
      theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
            axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    

    temp_2 <- ggplot(chiMat , aes(x = Maha_distance , y = Chi_percentile, color=if_outliers)) +
      geom_point(size = 2) +
      ylab("Chi-square distribution percentile") + 
      xlab("Mahalanobis Distance")+
      scale_color_manual(name = "If Outliers",labels = c('No', 'Yes'), values = c("green", "red"))+
      geom_vline(aes(xintercept=pcrit), color="red", linetype="dashed",size=1)+
      geom_text(aes(x=pcrit, label=paste('Cut-off Value:', cutoff), y=0.5), colour="Red", angle=90, vjust = 1.2, size=5)+
      theme_bw() +
      theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
            axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
            plot.title = element_text(size = 20, face = "bold", color = "darkgreen"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    temp <- temp_1+temp_2+plot_annotation(
      title = 'Outlier Detection',
      theme = theme(plot.title = element_text(size = 25))&theme(plot.subtitle = element_text(size = 15)),
      tag_levels = list(c('(a)', '(b)'), '1'))
    
    plot(temp)
  }

  return(which(chiMat$if_outliers==1))
  
  }
