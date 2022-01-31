#Here we will bring in raw data. I like putting all my libraries at the start of my code. You can place them as you go as well.
library(data.table)

#If you don't have a library installed you will have to install them via the code below or through the 'Packages' tab
install.packages("data.table")
library(data.table) #Now you can reload the library and shouldn't get an error

#We create an object to hold the url. This can be done in one line, but this helps see what's happening.
#url<-"https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
url<-"https://raw.githubusercontent.com/mattdemography/STA_6233_Spring2021/main/Data/nic-cage.csv"

#### Importing Files ####
#Now download the file
download.file(url, destfile = "C:/Users/Matthew/Documents/cage.csv")
list.files()

#Where does the file go? It goes to a default directory since we didn't set a working directory
#getwd()
#setwd("/Users/")
#getwd()

#Now that the file has been downloaded we can read it into R
#Read.csv
dat_csv<-read.csv("C:/Users/Matthew/Documents/cage.csv")
?read.csv()
#dat_csv<-read.csv("./Documents/cameras.csv")
#Read.table
dat_table<-data.table("C:/Users/Matthew/Documents/cage.csv", sep=",")
#Read.xlsx
dat_xlsx<-read.xlsx("C:/Users/Matthew/Documents/cameras.csv", sheetIndex=1, header=T) #xlsx is my least favorite method

#Take cage data frame and make it data table
str(dat_csv)
dt<-data.table(dat_csv)
str(dt)

#Do you see a difference?
#How do we fix this?

#What are in our data?
head(dt)
tail(dt)
names(dt)

#Count Number of Rows/Observations
nrow(dt)

#Percent of Voice Actor Movies
(table(dt$Voice)[2]/nrow(dt))*100

#table(dt$Rating) #Look at table function

#Count Number of Columns
length(dt)

#Read Specific Rows
dt[1,]
#How Would I capture more than one row?
dt[1:5,]  
dt[c(90, 5, 8),]

#Read Specific Columns
dat_table[,1]
#How would I capture more than one column?
new_dt<-dt[c(50:60,3,6:8),c(6,3)]
head(new_dt)
names(new_dt)

#How Would I capture the third observation in the 5th column?
#Answer

#Now answer the following questions using the file 'SampleMattData' found in the course Github repository. 
#Note add ?raw=true to the end of the link to get the proper file
#How many rows?
#How many columns?
#What are the names of the variables in the file?
#What is the response to the 10th column by the 20th through 25th respondent?

url<-"https://raw.githubusercontent.com/mattdemography/STA_6233_Spring2021/main/Data/SampleMattData.csv"
download.file(url, destfile = "C:/Users/Matthew/Documents/SampleMattData.csv")
sampleMD<-read.csv("C:/Users/Matthew/Documents/SampleMattData.csv")

dt1 <- data.table(sampleMD)
nrow(dt1)
length(dt1)
names(dt1)
dt1[c(20:25),10]


#### Subsetting ####
#Let's limit our example data to the first three columns.
ex_sub<-dt1[,1:3]
names(ex_sub)
str(ex_sub)

#Now let's further limit this to only respondents who agree with the first question
table(ex_sub$My.district.supports.me.to.take.risks.and.try.new.things)
table(ex_sub[,2])

ex_sub1<-subset(ex_sub, ex_sub$My.district.supports.me.to.take.risks.and.try.new.things=="Agree")
nrow(ex_sub1)  

#Now Create a subset using a randomization funciton in dplyr
library(dplyr)
ex_random<-sample_n(dt1, 400, replace=F)

#What is the first row of this new dataset?
ex_random[1,]

#### Changing Variables ####
#I don't like those long column names - Let's change this
library(plyr)
library(dplyr)
ex_sub1<-plyr::rename(ex_sub1, c(My.district.supports.me.to.take.risks.and.try.new.things="new_things", 
                                 I.would.recommend.this.school.district.to.friends.as.a.great.place.to.work="recommend"))
names(ex_sub1)

#Another Way
ex_sub1<-subset(ex_sub, ex_sub$My.district.supports.me.to.take.risks.and.try.new.things=="Agree")
names(ex_sub1)
ex_sub1<-ex_sub1 %>% dplyr::rename(new_things=2, recommend=3)
names(ex_sub1)

#Yet Another Way
ex_sub1<-subset(ex_sub, ex_sub$My.district.supports.me.to.take.risks.and.try.new.things=="Agree")
names(ex_sub1)  
colnames(ex_sub1)[2:3]<-c("new_things", "recommend")
names(ex_sub1)  

#Okay Now I want to make two categories one that is agree and the other that is disagree for the 'recommend' variable
table(ex_sub1$recommend)

ex_sub1$newvar<-ifelse(ex_sub1$recommend=="Somewhat Agree", "Agree", ex_sub1$recommend)
table(ex_sub1$newvar)  

ex_sub1$newvar<-ifelse(ex_sub1$recommend=="Somewhat Agree" | ex_sub1$recommend=="Strongly Agree", "Agree", 
                       ifelse(ex_sub1$recommend=="Strongly Disagree" | ex_sub1$recommend=="Somewhat Disagree", "Disagree", ex_sub1$recommend))
table(ex_sub1$newvar)

ex_sub1[ , newvar_dt := ifelse(ex_sub1$recommend=="Somewhat Agree" | ex_sub1$recommend=="Strongly Agree", "Agree", 
                               ifelse(ex_sub1$recommend=="Strongly Disagree" | ex_sub1$recommend=="Somewhat Disagree", "Disagree", ex_sub1$recommend))]
table(ex_sub1$newvar)
table(ex_sub1$newvar_dt)

#What's Missing?
table(ex_sub1$recommend)  

#### Data Table ####

#Let's look at the system read times to see why one may want to use data.table and not read.csv
big_df <- data.frame(x=rnorm(1E6), y=rnorm(1E6)) 
file <- tempfile() 
write.table(big_df, file=file, row.names=FALSE, col.names=TRUE, 
            sep="\t", quote=FALSE) 
system.time(fread(file)) 
system.time(read.csv(file))

#Let's create some data to show the functionality of data.table
#Set size of dataset
set.seed(100)
size<-20000

#Years<-sample(c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019), size, replace=T)
Years<-sample(c("2011", "2012", "2013", "2014", 2015, 2016, 2017, 2018, 2019), size, replace=T)

Months<-sample(c("January", "February", "March", "April", "May", "June", "July",
                 "August", "September", "October", "November", "December"), size, replace=T)
Orgs<-sample(c("Tri West", "Tri South", "Texas Best", "Helping Here", "Silver and Black Give Back",
               "Mike's Tots", "Purple Cross"), size, replace=T)
Region<-sample(c("South", "West", "North", "East"), size, replace=T)
Cost<-sample(100:100000, size, replace=T)

df<-data.frame(Orgs=Orgs, Months=Months, Region=Region, Years=Years, Cost=Cost)
dt<-data.table(Orgs=Orgs, Months=Months, Region=Region, Years=Years, Cost=Cost)

#Subset the data table 
dtm<-dt[dt$Months=="March",]
table(dtm$Months)

#Rows
dt[c(2,3)]
#Columns
dt[,c(2,3)]

#Calculating values for variables with expressions
sumstats<-dt[,list(mean(Cost), sum(Cost))]
sumstats

#Creating New Columns
dt[,newvar:=Cost/2]
summary(dt$Cost)
summary(dt$newvar)    
head(dt)    

dplyr::mutate(dt, newvar_dplyr=Cost/2)

#Creating Columns with multiple operations
dt[,multi:= {tmp <- (Cost+newvar); log2(tmp+5)}] 
#summary(dt$multi)    
#head(dt)

#If/Else like operations
dt[,val:=Cost>3000]
table(dt$val)    

#### Summarise Data with Dplyr ####
dplyr::summarise(dt, avg=mean(Cost))
dplyr::summarise_each(dt[,c(4:6)], funs(mean))
