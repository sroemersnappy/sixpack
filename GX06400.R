## Savannah's feedback loop project
#using R to create histograms similar to minitab
#8/24/2022
# Import packages
library(SixSigma)
library(readxl)
library(grid)
library(ggplot2)
FMax <- read_xlsx(file.choose(), col_names =FALSE, range = "C14:C68")
FMax= as.matrix(FMax)



# Type in measurements' values


# Get the Z (sigma score) of the process
ss.ca.z(FMax, LSL = 370, USL = 430)

# Get the Cp indices of the process
ss.ca.cp(FMax, LSL = 370, USL = 430)

# Get the Cp indices confidence intervals
ss.ca.cp(FMax, LSL = 370, USL = 430, ci = TRUE)

# Get the Cpk indices of the process
ss.ca.cpk(FMax, LSL = 370, USL = 430)

# Get the Cpk indices confidence intervals
ss.ca.cpk(FMax, LSL = 370, USL = 430, ci = TRUE)

# Perform process capability analysis
twopack(FMax,
        LSL = 370,
        USL = 430,
        Target = 400,
        alpha = 0.5,
        f.sub = "Process capability")


#hist(FMax, breaks=14)



#hist(FMax, breaks=14)
