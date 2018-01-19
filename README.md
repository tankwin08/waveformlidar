# waveformlidar
The *waveformlidar* package provides functions to process waveform LiDAR data with decomposition or deconvolution methods. For the decomposition, multiple functions such as Gaussian, adaptive Gaussian and Weibull functions are available for directly decomposing waveforms. There are two deconvolution methods included for waveform deconvolution in this package: Richardson-Lucy (RL) and Gold algorithms. In addition, there are several functions are available for extracting waveform-related variables from raw waveforms, which can be beneficial for characterizing the waveform for tree species identification, biomass estimation and vegetation characterization. 

**Install waveformlidar**
---
The current developmental version can be downloaded from github via  
```
if (!require("devtools")) {  

  install.packages("devtools")  
  
}  

devtools::install_github("tankwin08/waveformlidar", dependencies = TRUE)  
library(waveformlidar)
``` 
**How to use waveformlidar**
---
##import return waveform data. Here we just made one simple example of decomposing waveforms
data(return)  
lr<-nrow(return)  
ind<-c(1:lr)  ##create a index for each waveform  
plot (as.numeric(return[i,]),type="l")  ##you can check the raw waveform data  

##prepare the data  
return<-data.frame(ind,return)  

x<-return[182,] ###must be a dataset including intensity with index at the beginning.  
r1<-decom(x)  ##use default setup with Gaussian decomposition
r2<-decom(x,smooth="TRUE",width=3) ###you can assign different smooth width to process the data  

**What is the best way to ask a question or propose a new feature?**
---
To propose a new feature or report a bug, please open an issue on github (https://github.com/tankwin08/waveformlidar/issues). Of course, you can always write me an email (tankchow12@gmail.com).
