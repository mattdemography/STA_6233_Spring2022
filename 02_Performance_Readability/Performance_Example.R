#Setup
library(dplyr)

#Setting Up File Paths Online

#Bring in the Data Dictionary from the 'Data' folder

#The data dictionary file has a list of file names and years. It also contains variable names and the position of the variable in the file
#I want to rename each variable using this information. I want to rename variables in files beginning with 'campstud' and 'campothr'
#for years 1995 through 2000. I also want a new variable equal to the year to be added to each file.

#I want a new R object to be created for each year/file_name combination. But only the first 10 observations. 
#Complete the task however you like, but it may be wise (hint, hint) to utilize functions and/or loops.


#Hint to read in files use the read.delim function with the following options where the file= being the data file
read.delim(file=, sep=",", header = T)
