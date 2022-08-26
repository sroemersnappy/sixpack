# sixpack
stat sixpack for quality

#This is a modification of ss.study.ca in R
#I wanted to be able to change the binwidth of the histogram
#I saved a modified ss.study.ca as function "twopack" 
#If you change the denominator (in this example... value 1.4) you will change the binwidth. 
#This is currently line 71 in "twopack"
#I could not find prepCanvas in CRAN so I basically copied it off the internet and saved it as a function. Didn't mess with it much otherwise.

 binwST <- diff(range(xST))/(sqrt(nST)*1.4) 
