## Savannah's feedback loop project
#using R to create histograms similar to minitab
#8/24/2022
# Import packages
library(SixSigma)
library(readxl)
library(grid)
library(ggplot2)


#choose the file
file_user <- file.choose()

#get the file path so that the PN and WO# can be extracted from it using basename
extract=basename(file_user)

my_string_split <- scan(text = extract, what = "")  # Apply scan function

#concatenate substring one (part number) and substring two (WO#)
PN_WO <- paste(my_string_split[1], my_string_split[2])

#read Fmax data only from the excel file that was chosen
FMax <- read_xlsx(file_user, col_names =FALSE, range = "C14:C68")
FMax= as.matrix(FMax)

#import LSL,USL, and target values from second excel file - downloaded from xT
PN_data_xT_deduped <- read_excel("P:/Quality/Savannah/PN trip data/PN data xT deduped.xlsx")
                        
index_of_PN <- which(PN_data_xT_deduped == my_string_split[1])

#  USL, LSL and Target values from the PN_data_xT_deduped excel file

lsl <- PN_data_xT_deduped[[index_of_PN,4]]
target <- PN_data_xT_deduped[[index_of_PN,2]]
usl <-PN_data_xT_deduped[[index_of_PN,5]]


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
