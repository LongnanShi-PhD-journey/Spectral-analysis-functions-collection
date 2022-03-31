# Spectral ploting function ----
spec_plot <- function(x,y, cat=NULL, main=NULL, cat_name=NULL){
  
  # Descriptions: This function is used to plot spectral data
  # Input: x: Wavenumbers
  #        y: Spectral data
  #        cat: the category used to plot, the default value is NULL
  #        cat_name: the legend name showed on plot, the default value is NULL
  #        main: the title of plot 
  # 
  # Output: Spectral plot
  
  library(classInt)
  library(RColorBrewer)
  library(ggplot2)
  
  if(is.null(cat)==TRUE){  
    matplot(x, y, xlab = "Wavenumber cm-1",
            ylab = "log(1/R)",
            xlim = rev(range(x)),
            type = "l",
            lty = 1,
            col = rgb(red = 0.5, green = 0.5, blue = 0.5, alpha = 0.3),
            main=main)
  }else{
    
    # the color range is divided into n intervals (n=5) based on ascending increase of category
    # each observation will be assign a color code according to category value
    
    n=5
    pal <- rev(brewer.pal(n, "Spectral")) 
    int=classIntervals(cat,n)
    col=pal[findInterval(cat,int$brks,all.inside = T)] # assign color for each observations
    
    # plot spectral data
    
    par(family="serif") # set plot layout
    layout(matrix(c(1:2),1,2,byrow=T),width=c(5,1))
    matplot(x, y, 
            xlim = rev(range(x)),
            type = "l",
            lty = 1,
            col = col,
            main = main,
            xlab = "Wavenumber cm-1",
            ylab = "log(1/R)")
    
    # add legend as a second plot aside the spectral plot
    
    legend_image=as.raster(matrix(rev(pal),ncol=1)) # plot legend as a raster plot to erase the boundary
    plot(c(0,4),c(0,1.1),type = 'n', axes = F,xlab = '', ylab = '')
    text(x=1.8, y = seq(0,1,0.5), labels = format(round(int$brks[c(1,3,6)],1)),cex=1.25) # add values on legend plot
    text(x=2, y = 1.05, labels = cat_name,cex=1.5) # add title for legend plot
    rasterImage(legend_image, 0, 0, 1,1) # plot legend plot
    par(mfrow=c(1,1)) # recovery the plot layout
    
    }
}

# Example
# spec_plot(x= as.numeric(colnames(ISIS_chem$MIR)), y= t(ISIS_chem$MIR), 
#           main='Raw MIR Spectral', cat = ISIS_chem$OC,cat_name = 'OC content')
#
# spec_plot(x= as.numeric(colnames(ISIS_chem$MIR)), y= t(ISIS_chem$MIR), 
#           main='Raw MIR Spectral')
# 
# spec_plot(x= as.numeric(colnames(ISIS_chem$MIR)), y= t(ISIS_chem$MIR), 
#           main='Raw MIR Spectral', cat = ISIS_chem$Clay,cat_name = 'Clay content')


# tiff(file="Spectral_oc.tiff",
#     width=16, height=9, units="in", res=600,compression = "zip")
# spec_plot(x= as.numeric(colnames(ISIS_chem$MIR)), y= t(ISIS_chem$MIR), 
#           main='Raw MIR Spectral', cat = ISIS_chem$OC,cat_name = 'OC content')
# dev.off()
