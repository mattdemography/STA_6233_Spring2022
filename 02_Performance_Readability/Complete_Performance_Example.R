#Setup
library(dplyr)

#Setting Up File Paths Online
main<-"https://raw.githubusercontent.com/mattdemography/STA_6233_Spring2021/master/Data/"

#Bring in the Data Dictionary from the 'Data' folder
dd <- read.csv("https://raw.githubusercontent.com/mattdemography/STA_6233_Spring2021/master/Data/Data_Dictionary_revised.csv", stringsAsFactors=FALSE)

#The data dictionary file has a list of file names and years. It also contains variable names and the position of the variable in the file
#I want to rename each variable using this information. I want to rename variables in files beginning with 'campstud' and 'campperf'
#for years 1995 through 2000. I also want a new variable equal to the year to be added to each file.

#I want a new R object to be created for each year/file_name combination. But only the first 10 observations. 
#Complete the task however you like, but it may be wise (hint, hint) to utilize functions and/or loops.

file_type<-dd$FILE_TYPE %>% unique() #Get Only Unique File Types
file_name<-dd$FILE_NAME_ALT %>% unique() #Get Only Unique File Names (Use FILE_NAME_ALT)
file_name<-file_name[c(1,3)]
years<-dd$VAR_YR %>% unique() #Get Only Unique Years
years<-years[2:7] #Keep Only 1995 through 2000

for(i in 1:length(file_name)){
  for(j in 1:length(years)){
    f<- read.delim(file= paste0(main, file_name[i], "/", file_name[i], "_", years[j], ".dat"), sep=",", header = T)
    
    #Subset Larger Dataset with File_Name and Year Combinations to get variable positions and attach variable names in dataset f
    dd_sub<-subset(dd, dd$FILE_NAME_ALT==file_name[i])
    dd_sub<-subset(dd_sub, dd_sub$VAR_YR==years[j])
    dd_sub<-dd_sub[order(dd_sub$VAR_NAME),]
    var_names<-dd_sub$VAR_NAME
    pos<-dd_sub$VAR_POS

    names(f)[pos]<- var_names #Rename columns based on position number
    f$data_year<-years[j]
    eval(parse(text=paste0(file_name[i],"_", years[j] , '<-f[1:10,]')))
  }
}

