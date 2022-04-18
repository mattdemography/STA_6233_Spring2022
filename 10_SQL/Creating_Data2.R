library(dplyr)

#Set size of dataset
size_income<-20000

Years_i<-sample(c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019), size_cost, replace=T)
Months_i<-sample(c("January", "February", "March", "April", "May", "June", "July",
                 "August", "September", "October", "November", "December"), size_cost, replace=T)
Orgs_i<-sample(c("Tri West", "Tri South", "Texas Best", "Helping Here", "Silver and Black Give Back",
               "Mike's Tots", "Purple Cross"), size_cost, replace=T)
Region_i<-sample(c("South", "West", "North", "East"), size_cost, replace=T)
Income_i<-sample(100:100000, size_cost, replace=T)

Income<-data.frame(Orgs=character(), Months=character(), Region=character(), 
              Years=numeric(), Income=numeric())
  Income<-Income[1:size_income,]
  Income$Orgs<-Orgs_i
  Income$Months<-Months_i
  Income$Region<-Region_i
  Income$Years<-Years_i
  Income$Income<-Income_i
  Income$Group_ID <- Income %>% group_indices(Orgs)
  Income$Month_ID <- Income %>% group_indices(Months)
  Income$Region_ID <- Income %>% group_indices(Region)
  Income$Year_ID <- Income %>% group_indices(Years)
  Income$U_ID <- paste0(Income$Group_ID, Income$Month_ID, Income$Year_ID, Income$Region_ID)

saveRDS(Income, "C:/Users/Matthew/Dropbox/Courses Taught/Advanced_R/Github_Projects/STA_6233_Spring2021/Data/Income.RData")

#Now Create Expenditure Data that is identical for org/month/region/year combination
size_exp<-table(duplicated(Income$U_ID))[1]

Years_e<-sample(c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019), size_exp, replace=T)
Months_e<-sample(c("January", "February", "March", "April", "May", "June", "July",
                   "August", "September", "October", "November", "December"), size_exp, replace=T)
Orgs_e<-sample(c("Tri West", "Tri South", "Texas Best", "Helping Here", "Silver and Black Give Back",
                 "Mike's Tots", "Purple Cross"), size_exp, replace=T)
Region_e<-sample(c("South", "West", "North", "East"), size_exp, replace=T)
Expenses_e<-sample(100:100000, size_exp, replace=T)

Expenses<-data.frame(Orgs=character(), Months=character(), Region=character(), 
                     Years=numeric(), Expenses=numeric())
  Expenses<-Expenses[1:size_exp,]
  Expenses$Orgs<-Orgs_e
  Expenses$Months<-Months_e
  Expenses$Region<-Region_e
  Expenses$Years<-Years_e
  Expenses$Expenses<-Expenses_e
  Expenses$Group_ID <- Expenses %>% group_indices(Orgs)
  Expenses$Month_ID <- Expenses %>% group_indices(Months)
  Expenses$Region_ID <-Expenses %>% group_indices(Region)
  Expenses$Year_ID <- Expenses %>% group_indices(Years)
  Expenses$U_ID <- paste0(Expenses$Group_ID, Expenses$Month_ID, Expenses$Year_ID, Expenses$Region_ID)

saveRDS(Expenses, "C:/Users/Matthew/Dropbox/Courses Taught/Advanced_R/Github_Projects/STA_6233_Spring2021/Data/Expenses.RData")

#Save To Database for SQL Examples
  conn <- dbConnect(SQLite(),'C:/Users/Matthew/Dropbox/Courses Taught/Advanced_R/Github_Projects/STA_6233_Spring2021/Data/examples.db')
  dbGetQuery(conn, "DROP TABLE Income")
  dbGetQuery(conn, "DROP TABLE Expenses")
  dbWriteTable(conn, "Income", Income)
  dbWriteTable(conn, "Expenses", Expenses)
