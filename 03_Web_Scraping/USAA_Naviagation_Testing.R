library(base64)
options(error=expression(NULL))

library(RSelenium)
library(rvest)
library(dplyr)
library(XML)

see_page<-function(x){
  remDr$maxWindowSize()
  remDr$screenshot(display = TRUE)
}

end_page<-function(x){
  element <- remDr$findElement("css", "body")
  element$sendKeysToElement(list(key = "end"))
}

#### Set Quote Variables ####
zip="78233"
branch="Army"
rank=2
drivers=1
sex="Female"  #Male | Female
marital="Married" #Single | Married
tickets=0 #0=None, 1=1, 2=2 or More
accidents=0 #0=None, 1=1, 2=2 or More
current_insurer="nine-to-eleven"   #Less than 1 (less-than-one); #1 to 2 (one-to-two); 3 to 5 (three-to-five); #6 to 8 (six-to-eight); 9 to 11 (nine-to-eleven); 12 or more ();

#Create standalone browser. Only have to do this once.
#shell('docker run -d -p 4445:4444 selenium/standalone-chrome')
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445, browserName = "chrome")

#Open Driver
remDr$open()
Sys.sleep(2)

#Navigate
remDr$navigate("https://www.usaa.com/insurance/car-insurance-quote")

#Find Text Box for Zip Code
#Send text to input box. don't forget to use `list()` when sending text
element <-NULL
while(is.null(element)){
  element <- tryCatch({
    element <-remDr$findElement(using = "css",'input[type="text"]')
    element$sendKeysToElement(list(zip))
    element <-remDr$findElement(using= "css", 'Button[type="submit"]')
    element$clickElement()
    print("Finished Zip Code")
  }, error = function(e){})
}

#### Military Service ####
#Enlisted - Goes to Branch Next
element <-NULL
while(is.null(element)){
  element <- tryCatch({
    suppressMessages(element <-remDr$findElement(using= "css", 'Button[id="enlisted"]'))
    suppressMessages(element$clickElement())
    print("Finished Service Type")
  }, error = function(e){})
}

#### Branch Page ####
#Army
element <-NULL
while(is.null(element) & branch=="Army"){
  element <- tryCatch({
  suppressMessages(element <-remDr$findElement(using= "css", 'Button[id="army"]'))
  suppressMessages(element$clickElement())
    print("Finished Branch - Army")
  }, error = function(e){})
}

#Marines
element <-NULL
while(is.null(element) & branch=="Marines"){
  element <- tryCatch({
    suppressMessages(element <-remDr$findElement(using= "css", 'Button[id="marines"]'))
    suppressMessages(element$clickElement())
    print("Finished Branch - Marines")
  }, error = function(e){})
}

#Navy
element <-NULL
while(is.null(element) & branch=="Navy"){
  element <- tryCatch({
    suppressMessages(element <-remDr$findElement(using= "css", 'Button[id="navy"]'))
    suppressMessages(element$clickElement())
    print("Finished Branch - Navy")
  }, error = function(e){})
}

#Air Force
element <-NULL
while(is.null(element) & branch=="Air Force"){
  element <- tryCatch({
    suppressMessages(element <-remDr$findElement(using= "css", 'Button[id="airForce"]'))
    suppressMessages(element$clickElement())
    print("Finished Branch - Air Force")
  }, error = function(e){})
}

#Coast Guard
element <-NULL
while(is.null(element) & branch=="Coast Guard"){
  element <- tryCatch({
    suppressMessages(element <-remDr$findElement(using= "css", 'Button[id="coastGuard"]'))
    suppressMessages(element$clickElement())
    print("Finished Branch - Coast Guard")
  }, error = function(e){})
}
  
  #Rank  #E1 - Private E1 (2); #E2 - Private E2 (3); #E3 - Private First Class (4); #E4 - Specialist (5); #E4 - Corporal (6); #E5 - Sergeant (7); #E6 - Staff Sergeant (8)
  element <-NULL
  while(is.null(element)){
    element <- tryCatch({
      suppressMessages(element <-remDr$findElement(using= "xpath", 
                                  eval(parse(text=paste0("\"/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/form/div[1]/div/div/div/div[1]/div[3]/select/option[", rank, "]\"")))))
      suppressMessages(element$clickElement())
      suppressMessages(element <-remDr$findElement(using= "css", 'Button[type="submit"]'))
      suppressMessages(element$clickElement())
      print("Finished Rank")
    }, error = function(e){})
  }


  #E1 - Seaman Recruit (2); #E1 - Airman Recruit (3); #E1 - Fireman Recruit (4); 
  #E2 - Seaman Apprentice (5);  #E2 - Airman Apprentice (6); #E2 - Fireman Apprentice (7);
  #E3 - Seaman (8); #E3 - Airman  (9);  #E3 - Fireman (10); #E4 - Petty Officer Third Class (11);
  #E5 - Petty Officer Second Class (12); #E6 - Petty Officer First Class (13); 

#### Are You Married to USAA Member ####
#True for Army E1 - E6
#Only to establish eligibility
element <-NULL
while(is.null(element)){
  element <- tryCatch({
    suppressMessages(element <-remDr$findElement(using= "css", 'Button[id="yes"]'))
    suppressMessages(element$clickElement())
    print("Finished Married to USAA Member")
  }, error = function(e){})
}
  
  #### How Many Drivers ####
  element <-NULL
  while(is.null(element)){
  element <- tryCatch({
  suppressMessages(element <-remDr$findElement(using= "xpath", 
                              eval(parse(text=paste0("\"/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/div/div[1]/button[", drivers, "]\"")))))
  suppressMessages(element$clickElement())
    }, error = function(e){})
  }
  
  #### Driver Demographics ####
  element <-NULL
  while(is.null(element)){
  element <- tryCatch({
  suppressMessages(element <-remDr$findElement(using= "css", eval(parse(text=paste0("'[name=\"gender\"][value=\"", sex,"\"]'")))))
  suppressMessages(element$clickElement())
  
  suppressMessages(element <-remDr$findElement(using= "css", eval(parse(text=paste0("'[name=\"maritalStatus\"][value=\"", marital,"\"]'")))))
  suppressMessages(element$clickElement())
  
  suppressMessages(element <-remDr$findElement(using= "css", eval(parse(text=paste0("'[name=\"tickets\"][value=\"", tickets,"\"]'")))))
  suppressMessages(element$clickElement())
  
  suppressMessages(element <-remDr$findElement(using= "css", eval(parse(text=paste0("'[name=\"accidents\"][value=\"", accidents,"\"]'")))))
  suppressMessages(element$clickElement())
  
  #Find Text Box for Date of Birth
  suppressMessages(element <-remDr$findElement(using = "css",'input[type="text"]'))
  #Send text to input box. don't forget to use `list()` when sending text
  suppressMessages(element$sendKeysToElement(list("09/30/1992")))
  suppressMessages(element <-remDr$findElement(using= "css", 'Button[type="submit"]'))
  suppressMessages(element$clickElement())
  }, error = function(e){})
}

  #### How Many years Have You Been With Current insurer? ####

  element <-NULL
  while(is.null(element)){
  element <- tryCatch({
  suppressMessages(element <-remDr$findElement(using = "css", eval(parse(text=paste0("'Button[id=\"",current_insurer, "\"]'")))))
  suppressMessages(element$clickElement())
  }, error = function(e){})
}
  