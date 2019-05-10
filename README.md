**Overview**
---
A wealth of Full Waveform (FW) LiDAR data are available to the public from different sources, which is poised to boost the extensive application of FW LiDAR data. However, we lack a handy and open source tool that can be used by potential users for processing and analyzing FW LiDAR data. To this end, we introduce *waveformlidar*, an R package dedicated to FW LiDAR processing, analysis and visualization as a solution to the constraint. Specifically, this package provides several commonly used waveform processing methods such as Gaussian, adaptive Gaussian and Weibull decompositions, and deconvolution approaches (Gold and Richard-Lucy (RL)) with usersâ€™ customized settings. In addition, we also develop some functions to derive commonly used waveform metrics for characterizing vegetation structure. Moreover, a new way to directly visualize FW LiDAR data is developed through converting waveforms into points to form the Hyper Point cloud (HPC), which can be easily adopted and subsequently analyzed with existing discrete-return LiDAR processing tools such as LAStools and FUSION. Basic explorations of the HPC such as 3D voxelization of the HPC and conversion from original waveforms to composite waveforms are also available in this package. All of these functions are developed based on small-footprint FW LiDAR data, but they can be easily transplanted to the large footprint FW LiDAR data such as Geoscience Laser Altimeter System (GLAS) and Global Ecosystem Dynamics Investigation (GEDI) data analysis. 


**Install waveformlidar**
---
The stable version can be found in CRAN and you can use the following code to install:
```
install.packages("waveformlidar")
```
The current developmental version can be downloaded from github via  
```
if (!require("devtools")) {  
  install.packages("devtools")  
}  
devtools::install_github("tankwin08/waveformlidar", dependencies = TRUE)  

##if you didn't install rPeaks, you may need to install it first at the beginning, but most of time, it works fine.
#if (!require("rPeaks")) {
#    devtools::install_github("jrminter/rPeaks")
#  }
``` 
**How to use waveformlidar**
---
As a brief introduction, we conduct Gaussian decomposition on several waveforms to extarct useful information from the wavefomrs. In addition, we also examplify the waveforms on decompostion method with GOLD and RL methods. The detailed description of these methods has been documented in our previous reserach (https://doi.org/10.1016/j.isprsjprs.2017.04.021). 
Furthermore, we also demonstrate how to generate Hyper Point Cloud (HPC) from a sample waveform dataset at a small region.
Some examples about using these functions can be found in vignettes.

***Decomposition***
---
We used two waveforms as an example to demonstrate how to conduct Gaussian, adaptive and Weibull decompositon with differnt preprocessing settings.

```R
library(data.table)
data(return)
wf<-data.table(index=c(1:nrow(return)),return)
###################################################################################################
#decomposition two examples: one is the simple waveform and another was more complex waveform
r1<- decom(wf[1,])    ##default, with smooth was applied
r2<- decom(wf[1,], smooth = FALSE)  ##use the raw waveform

##for more complicated waveform
r3<- decom(wf[182,])
##when the waveform is mixed with too much noise, we should consider use more filtering options
r4<- decom(wf[182,],smooth=TRUE,width=5)
###we also can fit the waveform with the other models such as adaptive Gaussian and Weibull
r5<-decom.adaptive(wf[182,])
r6<-decom.weibull(wf[182,])

```
We selected one of the results to demonstrate the structure of results with differnt prepocessing settings. As you can see, the r3 gave us NULL due to there is no solution when we used the Gaussian model to decompose waveform. However, with some filtering step, we can get the solution of the decompostion as shwon in r4. But this solution is not a reasonle solution since one of the As is negative. Consequently, the index number r4[[1]] and r4[[3]] return NA and NULL to indicate that the caution should be exercised on this result. 

```
r3
NULL

r4
[[1]]
[1] NA

[[2]]
       index   Estimate Std. Error   t value     Pr(>|t|)
A1       182 228.709231  7.3797916 30.991286 1.818462e-49
A2       182 -30.882612  8.3873988 -3.682025 3.964092e-04
A3       182  81.869094  5.8023865 14.109555 2.012387e-24
u1       182  41.640000  0.4578811 90.940635 1.379831e-89
u2       182  42.130641  0.8817503 47.780695 3.086295e-65
u3       182  71.680461  0.7279330 98.471233 1.243687e-92
sigma1   182  14.612510  0.4900214 29.820145 4.161742e-48
sigma2   182   3.522174  1.1290565  3.119573 2.441069e-03
sigma3   182   8.072803  0.6615389 12.203065 1.052106e-20

[[3]]
NULL

```

***Deconvolution***
---
Compared to the decomposition, the deconvolution requires more input data and additional processing steps. Generally, we should have three kinds of data as the input for the deconvolution: the return waveform (RW), corresponding outgoing pulse (OUT) and the system impulse response (SIR). The RW and OUT are directly provided by the vendor. Ideally, the SIR is obtained through the calibration process in the lab before the waveform data are collected. In our case, the NEON provided a return impulse response (RIR) which can be assumed as a prototype SIR. This system impulse was obtained through a return pulse of single laser shot from a hard ground target with a mirror angle close to nadir. Meanwhile, NEON also provided the corresponding outgoing pulse of this return impulse response (RIR_OUT). The "true" system impulse response can be obtained by deconvolving the RIR_OUT.
In this package, we provide two options for users to deal with the system impulse response (SIR). One is directly to assume the RIR as the SIR by assigning imp = RIR. Another is to obtain the SIR through deconvolving the OUT_RIR. In the function, the "true" SIR can be achieved by assigning imp = RIR and imp_out = OUT_RIR.

```R
data(return)
data(outg)  ###corresponding outgoing pulse of return
data(imp)  ##The impulse function is generally one for the whole study area or
data(imp_out) ##corresponding outgoing pulse of imp
i=1
re<-return[i,]
out<-outg[i,]
imp<-imp
imp_out<-imp_out

### option1: to obtain the true system impluse response using the return impluse repsonse (imp) and corresponding outgoing pulse (imp_out)
gold0<-deconvolution(re = re,out = out,imp = imp,imp_out = imp_out)
rl0<-deconvolution(re = re,out = out,imp = imp,imp_out = imp_out,method = "RL")

###option2: assume the return impluse repsonse RIP is the system impulse reponse (SIR)
gold1<-deconvolution(re = re,out = out,imp = imp)
rl1<-deconvolution(re = re,out = out,imp = imp,method="RL",small_paras = c(30,2,1.5,30,2,2))
plot(gold1,type="l")
lines(rl1,col="red")
```
***Method comparison***
---
To visually compare the decompostion and deconvolution results at the individual waveform level, we made a plot of three wavefroms with these three methods (Gaussian decomposition, Gold and RL deconvolution).

Individual waveform level:

![alt text](https://github.com/tankwin08/waveformlidar/blob/master/man/figures/README_decompostion%26deconvolution_example.png)

Point Cloud level:

![alt text](https://github.com/tankwin08/waveformlidar/blob/master/man/figures/README_point_cloud_comparison-min.png)

You also can find these results from our previous reserach: 
Tan Zhou*, Sorin C. Popescu, Keith Krause, Ryan D. Sheridan, and Eric Putman, 2017. Gold-A novel deconvolution algorithm with optimization for waveform LiDAR processing. ISPRS Journal of Photogrammetry and Remote Sensing 129 (2017): 131-150. https://doi.org/10.1016/j.isprsjprs.2017.04.021

**Hyper point cloud**
---
The routine for extracting FW LiDAR information is to convert part of waveform signals to discrete points with the decomposition or deconvolution methods, which has been proven useful for tree species identification, forest inventory, and biomass estimation. However, most of the intensity information inherent in waveforms is being ignored with the conventional methods that undoubtedly degrades the value of FW LiDAR data. Moreover, the complicated waveform processing steps perplex users and further hinder the extensive use of FW LiDAR data for vegetation characterization. To tackle these challenges, we directly convert all raw waveform signals into points to form a point cloud, named the HPC, for subsequent analysis. A HPC is a set of data points converted from all waveform signals along the pulse path by combing geo-reference information (black) with raw waveform data (blue). 

![alt text](https://github.com/tankwin08/waveformlidar/blob/master/man/figures/hyper_point_cloud_graphic_abstract-min.jpg)

```R
data(geo)
data(return)

geo$index<-NULL
colnames(geo)[1:8]<-c("x","y","z","dx","dy","dz","or","fr")
hpc<-hyperpointcloud(waveform=return,geo=geo)
```

**Individaul tree waveform voxelization**
---
The principle behind the voxel is that the neighborhood points shared the similar characteristic and the information within the homogenous unit can be represented by one quantity or one voxel.  The following example shows how to voxelize data from the HPC. The main parameter of this function is the voxel size (res) which require you to assign a vector containing three values to represent voxel size at the X, Y and Z directions. Analogous to the waveformgrid, we also can generate the quantile intensity in each voxel by adding quan argument.
```R
voxr<-waveformvoxel(hpc,res=c(1,1,0.15))
```
Here is one simple example to conduct waveform voxelization using one individual tree.
![alt text](https://github.com/tankwin08/waveformlidar/blob/master/man/figures/individual_tree_waveformvoxel_flat_tree1_60%25_maxi_0.8_0.8_0.15_filter.png)


**Citing waveformlidar and related software**

You are welcome to use the package. If you need more help on differnt dataset or cooperation, I'd love to contribute. Developing and maintaining open source software take authors a lot of time and effort yet often underappreciated contribution to scientific progress. Thus, whenever you are
using open source software (or software in general), please make sure to cite it
appropriately so that developers get credit for their work.

When using waveformlidar, please cite one or more of the following publications:

1. Tan Zhou *, Sorin Popescu, Lonesome Malambo, Kaiguang Zhao, Keith Krause. From LiDAR waveforms to Hyper Point Clouds: a novel data product to characterize vegetation structure. Remote Sensing 2018, 10(12), 1949; https://doi.org/10.3390/rs10121949

2. Tan Zhou*, Sorin C. Popescu, A. Michelle Lawing, Marian Eriksson, Bogdan M. Strimbu, and Paul C. Bürkner. Bayesian and Classical Machine Learning Methods: A Comparison for Tree Species Classification with LiDAR Waveform Signatures. Remote Sensing 10, no. 1 (2017): 39. doi:10.3390/rs10010039

3. Tan Zhou*, and S.C. Popescu, 2017. Bayesian decomposition of full waveform LiDAR data with uncertainty analysis. Remote Sensing of Environment 200 (2017): 43-62. http://dx.doi.org/10.1016/j.rse.2017.08.012

4. Tan Zhou*, Sorin C. Popescu, Keith Krause, Ryan D. Sheridan, and Eric Putman, 2017. Gold-A novel deconvolution algorithm with optimization for waveform LiDAR processing. ISPRS Journal of Photogrammetry and Remote Sensing 129 (2017): 131-150. https://doi.org/10.1016/j.isprsjprs.2017.04.021

**What is the best way to ask a question or propose a new feature?**
---
To propose a new feature or report a bug, please open an issue on github (https://github.com/tankwin08/waveformlidar/issues). Of course, you can always write me an email (tankchow12@gmail.com).
