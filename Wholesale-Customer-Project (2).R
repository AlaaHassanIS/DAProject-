#import data into Rstudio
WholeSCutmr<-read.csv(file.choose(),header = TRUE)
#
View(WholeSCutmr)
str(WholeSCutmr)

# Total missing values
sum(is.na(WholeSCutmr))  
# Missing values per column
colSums(is.na(WholeSCutmr))  

#
WholeSCutmr$Fresh[is.na(WholeSCutmr$Fresh)] <- mean(WholeSCutmr$Fresh, na.rm = TRUE)
