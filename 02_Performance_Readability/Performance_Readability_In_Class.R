#### For Loops ####
  #Let's say I want to do an action like print a phrase.
  print("Number 1")
  
  #If I want to print two phrases in sequence I could write two print statements
  print("Number 1")
  print("Number 2")
  
  #OR.... Loop through a list to print 1 through 10 phrases in order.
  for(i in 1:10){
    print("Number")
  }
  
  #Why didn't an actual number print?
  #How can we change this?
  for(i in 1:10){
    print(paste0("Number ", i))
  }
  paste("Number", i, "Here", sep="------")
  paste0("Number"," ", i, " ", "Here")
  
  #We can also use an object instead of counting to 10
  nums<-1:30
  for(i in nums){
    print(paste0("Number ", i))
  }

#Now let's try loops with real data.
dat<-read.csv("https://raw.githubusercontent.com/mattdemography/STA_6233_Spring2021/master/Data/SampleMattData.csv", stringsAsFactors = F)

#Checking the Names of the variables
  names(dat)
  head(dat)
  str(dat)
  
  #What if I want to change the names of the variables to be var_1, var_2, etc. for all variables in the data set? - Do so with a loop
  for(i in 1:length(dat)){
    names(dat)[i] = paste0("var_", i)
  }
  names(dat)
  
  #Changing to One-Word Column names that are more useful
  new_names<-c("District", "Risks", "Recommend","WorkValued", "Tools", "Vision", "Retention", "Recognized",
               "LiveValues", "Information", "Growth", "Role")
  names(dat)<-new_names
  
  #Check that Names have been transformed
  names(dat)
  
  #Sort Data by District Name
  dat2<-dat[order(dat$District),]
  
  #See which variables have likert scale because I want to change these to numbers
  head(dat)
  
  #Now see what scale looks like
  table(dat$Risks)
  
  #Create Loop to convert to numeric values with 0 being strongly disagree and strongly agree being 5
  #First grab all the names of variables that will be transformed
  vars<-names(dat[2:11])
  
  for(i in 1:length(vars)){
    eval(parse(text=paste0('dat$', vars[i], '<- ifelse(dat$', vars[i], '=="Strongly Disagree", 0, 
              ifelse(dat$', vars[i], '=="Disagree", 1, 
              ifelse(dat$', vars[i], '=="Somewhat Disagree", 2, 
              ifelse(dat$', vars[i], '=="Somewhat Agree", 3,
              ifelse(dat$', vars[i], '=="Agree", 4, 
              ifelse(dat$', vars[i], '=="Strongly Agree", 5, NA))))))')))
  }
  
  #Before we run the for-loop let's break down the elements of it first.
  
  #Now I want to print out a table for each of the variables I just changed. How would I do this in a loop?
  for(i in 1:length(vars)){
    eval(parse(text=paste0("print(table(dat$", vars[i],"))")))
  }
  table(dat$Role)
  #Imagine you want to print three letters (A,B,C) for every five numbers (6-10) for every name on a list (Bob, Bill, Sue, Antetokounmpo). 
  #So the Result is Bob A6, Bob A7, Bob A8, Bob A9, Bob A10, Bob B6, Bob B7... 
  #How would we write this?

  test_name <- c("Bob", "Bill", "Sue", "Antetokounmpo")
  lets <- c("A","B","C")
  nums <- 6:10
  for(i in 1:length(test_name)){
    for (j in 1:length(lets)) {
      for (k in 1:length(nums)) {
        print(paste0(test_name[i]," ",lets[j],nums[k]))
      }    
    }
  }
  
#### While Loops ####
  #What of instead of a for loop, I want to loop while a condition is true?
  #I want to print while a calculation is below 10,000
  i=1
  while(i<10000){
    print(paste0("Still Below 10,000. I is starting at ", i))
    i=i+(i*i)
    print(paste0("I has been updated to ", i))
  }
  
  #How many times did the while loop print? Is this correct?
  
  #What if I wanted to add an iteration number and the value of i to the while loop above?
  
#### Functions ####
  #Let's say there is a process that I want to be repeatable and callable
  #I want to take the second power of a number then divide this by the total number or observations in the column. 
  #Let's do this to the data we brought in earlier (dat) although it wouldn't be methodologically appropriate
  
  first_fun<-function(x,y){
    z<-y*y
    n<-nrow(x)
    t<-z/n
    return(t)
  }

  table(dat$Risks)
  
  dat$Risks_z<-dat$Risks*dat$Risks
  head(dat[,c(2,13)])
  dat$Risks_t<-dat$Risks_z/nrow(dat)
  head(dat[,c(2,13:14)])
  
  dat$Risks_f<-first_fun(dat, dat$Risks)
  head(dat[,c(3,13:15)])
  
  sim_fun<-function(x){
    print(paste0("This is a word: ", x))
  }
  
  sim_fun("too")
  sim_fun("phrase")
  
  #How could I make the above function just one argument? Make it work
  sec_fun<-function(x){
    z<-x*x
    n<-length(x)
    t<-z/n
    return(t)
  }
  
  dat$Risks_f2<-sec_fun(dat$Risks)
  head(dat[,c(3,13:16)])
  
  #Another Example of a function
  #I'm Making a Fake Data Set that appoximates normality
  ex<-rnorm(10, mean=0, sd=1)
  summary(ex)
  h<-hist(ex)
  xfit<-seq(min(ex), max(ex), length=40)
  yfit<-dnorm(xfit, mean=mean(ex), sd=sd(ex))
  yfit<-yfit*diff(h$mids[1:2])*length(ex)
  lines(xfit, yfit)
  
  #Let's Look at our two variables. I'm creating a function for the steps above. You don't have to do this, but it can be helpful.
  Dist_lines<-function(x){
    h<-hist(x)
    xfit<-seq(min(x), max(x), length=40)
    yfit<-dnorm(xfit, mean=mean(x), sd=sd(x))
    yfit<-yfit*diff(h$mids[1:2])*length(x)
    lines(xfit, yfit)
    
  }
  
  #Test the Function
  Dist_lines(ex)
  
  #Read the source code for each of the following three functions, puzzle out what they do, and then brainstorm better names.
  f1 <- function(string, prefix) {
    substr(string, 1, nchar(prefix)) == prefix
  }
  f1("Here's a phrase", "Here's")
  f1("Mr. Person", "Mrs.")
  
  f2 <- function(x) {
    if (length(x) <= 1) return(NULL)
    x[-length(x)]
  }
  f2(300)
  
  f3 <- function(x, y) {
    rep(y, length.out = length(x))
  }
  
#Now You will resolve a data problem using loops and functions how you see fit.
