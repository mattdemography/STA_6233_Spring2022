#Load Library DBI
library(DBI)
library(RSQLite)

#Establish Your Connection
  con <- dbConnect(RSQLite::SQLite(), "C:/Users/Matthew/Dropbox/Courses Taught/Advanced_R/Github_Projects/STA_6233_Spring2021/Data/examples.db")

#Show Tables in Database
  dbListTables(con)
  
#Show Variables in Database file
  dbListFields(con, "Income")
  
#Basic SQL Structure
  #SELECT statement
    #What do you want
    #On Variables
  #FROM statement
    #Where do you want the information from
  #WHERE
    #Any condition
    #On Values in columns
  #INNER JOIN
    #How do you want these datatables to connect
    #On Columns
  #ORDER BY
    #How do you want the resulting query to look
    #On Values in columns

#Show Unique Values in a Variable in Database file
  org<-dbGetQuery(con, "
            SELECT DISTINCT
            Orgs
            
            FROM Income")
  
  reg<-dbGetQuery(con, "
            SELECT DISTINCT
            Region
            
            FROM Income")
  
  both<-dbGetQuery(con, "
            SELECT DISTINCT
            Orgs,
            Region
            
            FROM Income")
  
  both_order <- dbGetQuery(con, "
            SELECT DISTINCT
            Orgs,
            Region
            
            FROM Income
             
            ORDER BY Region")

  
#### Do a Simple Query ####
  #If we want all the data
  income<-dbGetQuery(con, "
            SELECT * 
            FROM Income")
  
  #If we want to limit our data
  limit<-dbGetQuery(con, "
            SELECT * 
            FROM Income
            Limit 100;")
  
  #If we want only some variables
  some<-dbGetQuery(con, "
            SELECT 
            Orgs,
            Months,
            Year_ID
              
            FROM Income
            Limit 100;")
  
  #If we want to only bring back certain values
  some<-dbGetQuery(con, "
            SELECT 
            Orgs,
            Months,
            Year_ID
              
            FROM Income
              
            WHERE Orgs='Texas Best'")
  
#### Complete a Table Join ####
  # See what we have in our Expenses table?
  # What should we join on?
  # Bring in Expenses column from Expenses table
  dbListFields(con, "Expenses")
  
  exp1<-dbGetQuery(con, "
            SELECT *
            FROM Expenses
            Limit 100;")
  exp1<-dbGetQuery(con, "
            SELECT Expenses
            FROM Expenses")
  
  #Find Unique combinations of Orgs, Months, Regions, Years
  comb<-dbGetQuery(con, "
                   SELECT DISTINCT
                   Orgs,
                   Months,
                   Region,
                   Years
                   FROM expenses")
  Exp<-dbGetQuery(con, "
                  SELECT *
                  FROM expenses")
  
  #Inner Join
  join<- dbGetQuery(con, "
            SELECT
            i.*,
            e.Expenses as Money_Out
            
            FROM 
            Income i
            
            LEFT JOIN Expenses e
                ON i.U_ID = e.U_ID")
  
  join2<- dbGetQuery(con, "
            SELECT
            i.orgs,
            i.Months,
            i.Income,
            e.Expenses,
            e.U_ID
            
            FROM 
            Expenses e
            
            LEFT JOIN Income i
                ON i.U_ID = e.U_ID")
  #61291
  
  #Merge in R
  merge(x, y, all.x=T, all.y=T) #Inner
  merge(x, y, all.x=T, all.y=F) #Left
  merge(x, y, all.x=F, all.y=T) #Right
  merge(x, y, all.x=F, all.y=F) #Outer
  
#### Write Tables to Database ####
  dbGetQuery(con, "CREATE TABLE join2 AS
            SELECT
            i.orgs,
            i.Months,
            i.Income,
            e.Expenses as Money_Out,
            e.U_ID as Expense_ID
            
            FROM 
            Expenses e
            
            LEFT JOIN Income i
                ON i.U_ID = e.U_ID")
  
  #See if it wrote correctly
  dbListTables(con)
  test<-dbGetQuery(con, "
            SELECT *
            FROM join2")
  
  #Delete It
  dbGetQuery(con, "DROP TABLE join2")
  dbListTables(con)
  
#Interactions with R
  Orgs_e<-c("Tri West", "Tri South", "Texas Best", "Helping Here", "Silver and Black Give Back",
                   "Mikes Tots", "Purple Cross")
  #If we want to only bring back certain values
  for(i in 1:5){
   eval(parse(text=paste0("some_",i, "<-dbGetQuery(con, paste0(\"
            SELECT 
            Orgs,
            Months,
            Year_ID
              
            FROM Income
              
            WHERE Orgs='", Orgs_e[i], "'\"))")))
  }
  
rm(some)  
