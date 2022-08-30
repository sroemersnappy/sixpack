## Savannah's feedback loop project
#using R to create histograms similar to minitab
#8/24/2022
# Import packages
library(SixSigma)
library(readxl)
library(grid)
library(ggplot2)
library(stringr)


#choose the file
file_user <- file.choose()

#get the file path so that the PN and WO# can be extracted from it using basename
extract =basename(file_user)

my_string_split <- scan(text = extract, what = "")  # Apply scan function
my_string_split2 <- scan(text = my_string_split[3], what = "", sep = ".")

#concatenate substring one (part number) and substring two (WO#)
PN_WO1 <- paste(my_string_split[1], my_string_split[2])
PN_WO <- paste(PN_WO1, my_string_split2[1])


#read Fmax data only from the excel file that was chosen
FMax <- read_xlsx(file_user, col_names =FALSE, range = "C14:C68")
FMax <- na.omit(FMax)
FMax <- as.matrix(FMax)

#import LSL,USL, and target values from second excel file - downloaded from xT
PN_data_xT_deduped <- read_excel("P:/Quality/Savannah/PN trip data/PN data xT deduped.xlsx")
                        


# lookup of PN will fail if the PN includes something like "REV A"
#Delete all of these occurrences so that only the PN remains


withREV1 <- PN_data_xT_deduped[,1]
withREV <- as.matrix(withREV1)

withoutREV <- numeric()  

for (i in 1:length(withREV))
  {
  parsesubstring <- scan(text = withREV[i,1], what = "")
  newstring = parsesubstring[1]
  withoutREV <- c(withoutREV, newstring)
   }

withoutREV1 <- split(withoutREV,1)
worev <- unlist(withoutREV1)

PN_data_xT_deduped$PN <- worev

index_of_PN <- which(PN_data_xT_deduped == my_string_split[1])

#remove duplicates
index_of_PN <- index_of_PN[1]

#  USL, LSL and Target values from the PN_data_xT_deduped excel file

lsl <- PN_data_xT_deduped[[index_of_PN,4]]
target <- PN_data_xT_deduped[[index_of_PN,2]]
usl <-PN_data_xT_deduped[[index_of_PN,5]]

#Used this website to find how minitab calculates Cp and Cpk.
#Used calculated standard deviation, not within standard deviation
# https://support.minitab.com/en-us/minitab/20/help-and-how-to/quality-and-process-improvement/capability-analysis/how-to/
#capability-analysis/normal-capability-analysis/methods-and-formulas/potential-capability/

sigma<-sd(FMax)
mu <- mean(FMax)
specdiff=usl-lsl
#top <- sum((FMax-mu)^2)
#standard_deviation <- sqrt(top/(length(FMax)-1))
C_p <- (specdiff)/(6*sigma)
C_pL <- (mu-lsl)/(3*sigma)
C_pU <- (usl-mu)/(3*sigma)
C_pK <- min(C_pL,C_pU)
n_FMax <- length(FMax)-1


C_pm <- specdiff/(6*sqrt((sum((FMax-target)^2))/n_FMax))

# Get the Z (sigma score) of the process
ss.ca.z(FMax, LSL=lsl, USL= usl)

# Get the Cp indices of the process
ss.ca.cp(FMax, LSL=lsl, USL= usl)

# Get the Cp indices confidence intervals
ss.ca.cp(FMax, LSL=lsl, USL = usl, ci = TRUE)

# Get the Cpk indices of the process
ss.ca.cpk(FMax, LSL=lsl, USL= usl)

# Get the Cpk indices confidence intervals
ss.ca.cpk(FMax, LSL=lsl, USL= usl, ci = TRUE)

# Perform process capability analysis
twopack(FMax,
        LSL = lsl,
        USL = usl,
        Target = target,
        alpha = 0.5,
        f.sub = "Process capability")

