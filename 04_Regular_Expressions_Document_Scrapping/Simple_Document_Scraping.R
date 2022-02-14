#Bring in Libraries and Functions
library("tm")
library("pdftools")
library("stringr")
library(plyr)

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

#### Bring in PDF ####
#download.file("https://github.com/mattdemography/STA_6233_Spring2020/raw/master/Data/army.pdf", "./Army.PDF")
a<-pdf_text('C:/Users/Matthew/Dropbox/Courses Taught/Advanced_R/Github_Projects/STA_6233_Spring2021/Data/army.pdf')
  

#Make data into a data frame. This will have to be done in a loop.
  #Breaks by line - the \n does this
  a2<-strsplit(a, "\n") 
  army_full<-data.frame(Name=character(), First=character(), Middle=character(), Last=character(), Suffix=character(),
                 Rank=character(), MOS=character(), ETS_Date=character(), Branch=character(), Page=numeric())

for(j in 1:length(a2)){
if (j==1){
  a3<-a2[[j]][6:43]
  atable<-data.frame(a2[[j]][6:43])
  
} else {
  atable<-data.frame(a2[j])
  }
  
#atable<-data.frame(a3[j])
  rnums<-nrow(atable) #Number of Observations in the sheet
  atable$Main<-as.character(atable[1:rnums,1])
  atable$Main<-trimws(atable$Main, which="left")

#Create Variable Splits
  name_split<-as.data.frame(ldply(strsplit(atable$Main, split="SPC")))
  date_split<-as.data.frame(ldply(strsplit(name_split$V2, split=" ")))
  
#Count the number of missings in a column to create a proportion. If Over 50% are missing then drop the column
  date_split[date_split==""]<-NA  
  cols_to_drop<-data.frame(col=numeric(), p=numeric())
  for(m in 1:ncol(date_split)){
    #Take the sum of NAs
    s<-sum(is.na(date_split[m]))
    
    #Create placeholder dataset
    t<-data.frame(col=numeric(), p=numeric())
    col<-m
    
    #Find proportion of missing values
    p<-(s/nrow(date_split))
    t<-as.data.frame(cbind(col, p)) #Bind together for all columns in dataset date_split
    
    cols_to_drop<-rbind(cols_to_drop, t)
  }
  
  #Keep only columns with non-missing data
  cols_to_drop<-subset(cols_to_drop, cols_to_drop$p>0.5)
  cols_to_drop<-cols_to_drop$col
  date_split<-date_split[ -c(cols_to_drop)] #Drop Columns

  #Create Placeholder for data
  army<-data.frame(Main=character(), Name=character(), First=character(), Middle=character(), Last=character(), Suffix=character(),
                 Rank=character(), MOS=character(), ETS_Date=character())
  #Add number of rows equal to rows in document
  army<-army[1:rnums,]
  
  #Bring Over Main text from initial scrape
  army$Main<-atable$Main
  
  #Bring in Name from Name_split
  army$Name<-name_split[,1]
  
  #Find First, Last, and Suffix using Regular Expressions
  army$First<-str_extract(army$Name, pattern="([^A-z])([A-z]+) ")
  army$Last<-str_extract(army$Name, pattern="^([A-z]+)")
  army$Suffix<-str_extract(army$Name, " JR| SR| III")
  
  #Get MOS and ETS Dates
  army$MOS<-date_split[,1]
  army$ETS_Date<-date_split[,2]

  #Work on Middle Name and Rank
  army$Middle<-army$Name
  name<-army$Name
  ets_date<-army$ETS_Date
  mos<-army$MOS
  first<-army$First
  last<-army$Last
  middle<-trim(army$Name)
  rank<-army$Main
  
for(i in 1:rnums){
  middle[i]<-eval(parse(text=paste0("sub(as.character(first[i]), \" \", as.character(middle[i]))")))
  middle[i]<-eval(parse(text=paste0("sub(as.character(last[i]), \" \", as.character(middle[i]))")))
  rank[i]<-eval(parse(text=paste0("sub(as.character(name[i]), \" \", as.character(rank[i]))")))
  rank[i]<-eval(parse(text=paste0("sub(as.character(ets_date[i]), \" \", as.character(rank[i]))")))
  rank[i]<-eval(parse(text=paste0("sub(as.character(mos[i]), \" \", as.character(rank[i]))")))
}

#Bring the clean list back to the data frame
  army$Middle<-middle
  army$Middle<-gsub("JR|SR|III", "", army$Middle)
  army$Middle<-gsub("II", "", army$Middle)
  army$Middle<-trim(army$Middle)
  army$First<-trim(army$First)
  army$Last<-trim(army$Last)
  army$Suffix<-ifelse(is.na(army$Suffix), "", army$Suffix)
  army$Name<-paste0(army$First, " ", army$Middle, " ", army$Last, " ", army$Suffix) %>% trim()
  
  army$Rank<-rank %>% trim()
  army$Branch<-"Army"
  army$Page<-j

  army<-subset(army[,2:11])
  army_full<-rbind(army_full, army)

}



