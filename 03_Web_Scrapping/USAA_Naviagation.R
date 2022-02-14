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
branch="Coast Guard"
rank=2

#Create standalone browser. Only have to do this once.
shell('docker run -d -p 4445:4444 selenium/standalone-chrome')
#shell('docker run -d -p 4445:4444 selenium/standalone-firefox')
#eCaps <- list(chromeOptions = list(args = c('--window-size=1200,1100', '--headless')))
#remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445, browserName = "chrome", extraCapabilities=eCaps)
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445, browserName = "chrome")

#Open Driver
remDr$open()

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
    }, error = function(e){cat(conditionMessage(e))})
  }

#### Military Service ####
  #Enlisted - Goes to Branch Next
  element <-NULL
  while(is.null(element)){
    element <- tryCatch({
      element <-remDr$findElement(using= "css", 'Button[id="enlisted"]')
      element$clickElement()
      print("Finished Branch")
    }, error = function(e){cat(conditionMessage(e))})
    #loop until element with name <value> is found in <webpage url>
  }
  
#### Branch Page ####
  if(branch=="Army"){
  #Army
    element <-NULL
    while(is.null(element)){
      element <- tryCatch({
        element <-remDr$findElement(using= "css", 'Button[id="army"]')
        element$clickElement()
        print("Finished Branch")
      }, error = function(e){cat(conditionMessage(e))})
    }

    #Rank  #E1 - Private E1 (2); #E2 - Private E2 (3); #E3 - Private First Class (4); #E4 - Specialist (5); #E4 - Corporal (6); #E5 - Sergeant (7); #E6 - Staff Sergeant (8)
    element <-NULL
    while(is.null(element)){
      element <- tryCatch({
        element <-remDr$findElement(using= "xpath", 
                                    eval(parse(text=paste0("\"/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/form/div[1]/div/div/div/div[1]/div[3]/select/option[", rank, "]\""))))
        element$clickElement()
        element <-remDr$findElement(using= "css", 'Button[type="submit"]')
        element$clickElement()
        print("Finished Rank")
      }, error = function(e){cat(conditionMessage(e))})
    }

  } else if(branch=="Marines"){
  #Marines
    element <-NULL
    while(is.null(element)){
      element <- tryCatch({
        element <-remDr$findElement(using= "css", 'Button[id="marines"]')
        element$clickElement()
        print("Finished Branch")
      }, error = function(e){cat(conditionMessage(e))})
    }
    
    #Rank  #E1 - Private (2); #E2 - Private First Class (3); #E3 - Lance Corporal (4); #E4 - Corporal (5); #E5 - Sergeant (6); #E6 - Staff Sergeant (7); 
    element <-NULL
    while(is.null(element)){
      element <- tryCatch({
        element <-remDr$findElement(using= "xpath", 
                                    eval(parse(text=paste0("\"/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/form/div[1]/div/div/div/div[1]/div[3]/select/option[", rank, "]\""))))
        element$clickElement()
        element <-remDr$findElement(using= "css", 'Button[type="submit"]')
        element$clickElement()
        print("Finished Rank")
      }, error = function(e){cat(conditionMessage(e))})
    }
  } else if(branch=="Navy"){
  #Navy
    element <-NULL
    while(is.null(element)){
      element <- tryCatch({
        element <-remDr$findElement(using= "css", 'Button[id="navy"]')
        element$clickElement()
        print("Finished Branch")
      }, error = function(e){cat(conditionMessage(e))})
    }
 
    #Rank #E1 - Seaman Recruit (2); #E1 - Airman Recruit (3); #E1 - Fireman Recruit (4); 
    #E1 - Dentalman Recruit (6); #E1 - Hospitalman Recruit (7); #E2 - Seaman Apprentice (8);
    #E2 - Airman Apprentice (9); #E2 - Fireman Apprentice (10); #E2 - Constructionman Apprentice (11);
    #E2 - Dentalman Apprentice (12); #E2 - Hospitalman Apprentice (13); #E3 - Seaman (14); #E3 - Airman  (15);
    #E3 - Fireman (16); #E3 - Constructionman (17); #E3 - Dentalman (18) - #E3 - Hospitalman (19); 
    #E4 - Petty Officer Third Class (20); #E5 - Petty Officer Second Class (21); #E6 - Petty Officer First Class (22);
    element <-NULL
    while(is.null(element)){
      element <- tryCatch({
        element <-remDr$findElement(using= "xpath", 
                                    eval(parse(text=paste0("\"/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/form/div[1]/div/div/div/div[1]/div[3]/select/option[", rank, "]\""))))
        element$clickElement()
        element <-remDr$findElement(using= "css", 'Button[type="submit"]')
        element$clickElement()
        print("Finished Rank")
      }, error = function(e){cat(conditionMessage(e))})
    }
  } else if(branch=="Air Force"){
  #Air Force
    element <-NULL
    while(is.null(element)){
      element <- tryCatch({
        element <-remDr$findElement(using= "css", 'Button[id="airForce"]')
        element$clickElement()
        print("Finished Branch")
      }, error = function(e){cat(conditionMessage(e))})
    }
    
    #E1 - Airman Basic (2); #E2 - Airman (3); #E3 - Airman First Class (4); #E4 - Senior Airman (5); #E5 - Staff Sergeant (6); #E6 - Technical Sergeant (7);
    element <-NULL
    while(is.null(element)){
      element <- tryCatch({
        element <-remDr$findElement(using= "xpath", 
                                    eval(parse(text=paste0("\"/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/form/div[1]/div/div/div/div[1]/div[3]/select/option[", rank, "]\""))))
        element$clickElement()
        element <-remDr$findElement(using= "css", 'Button[type="submit"]')
        element$clickElement()
        print("Finished Rank")
      }, error = function(e){cat(conditionMessage(e))})
    }  
  } else if (branch=="Coast Guard"){
  #Coast Guard
    element <-NULL
    while(is.null(element)){
      element <- tryCatch({
        element <-remDr$findElement(using= "css", 'Button[id="coastGuard"]')
        element$clickElement()
        print("Finished Branch")
      }, error = function(e){cat(conditionMessage(e))})
    }
    #E1 - Seaman Recruit (2); #E1 - Airman Recruit (3); #E1 - Fireman Recruit (4); 
    #E2 - Seaman Apprentice (5);  #E2 - Airman Apprentice (6); #E2 - Fireman Apprentice (7);
    #E3 - Seaman (8); #E3 - Airman  (9);  #E3 - Fireman (10); #E4 - Petty Officer Third Class (11);
    #E5 - Petty Officer Second Class (12); #E6 - Petty Officer First Class (13); 
     
    element <-NULL
    while(is.null(element)){
      element <- tryCatch({
        element <-remDr$findElement(using= "xpath", 
                                    eval(parse(text=paste0("\"/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/form/div[1]/div/div/div/div[1]/div[3]/select/option[", rank, "]\""))))
        element$clickElement()
        element <-remDr$findElement(using= "css", 'Button[type="submit"]')
        element$clickElement()
        print("Finished Rank")
      }, error = function(e){NULL})
    }
  }
  
  #Never Served
  element <-remDr$findElement(using= "css", 'Button[id="never-served"]')
  element$getElementText()[[1]]
  element$clickElement()
  
  #### Are You Married to USAA Member ####
    #True for Army E1 - E6
    #Only to establish eligibility
  element <-NULL
  while(is.null(element)){
    element <- tryCatch({
      element <-remDr$findElement(using= "css", 'Button[id="yes"]')
      element$clickElement()
      print("Finished Married to USAA Member")
    }, error = function(e){cat(conditionMessage(e))})
  }

  
  ### Officer - Goes to Number of Drivers Next ###
    element <-remDr$findElement(using= "css", 'Button[id="officer"]')
    element$clickElement()
  
  #Army E7 - Sergeant First Class (9); #Army E8 - Master Sergeant (10); #Army E9 - Sergeant Major (12); Army E9 - Command Sergeant Major (13)
    element <-remDr$findElement(using="xpath",
                                eval(parse(text=paste0("\"/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/form/div[1]/div/div/div/div[1]/div[3]/select/option[", rank, "]\""))))
    element$clickElement()
    element <-remDr$findElement(using= "css", 'Button[type="submit"]')
    element$clickElement()
    
  #Marines E7 - Gunnary Sergeant (8); #Marines E8 - Master Sergeant (9); #Marines E8 - First Sergeant (10); #Marines E9 - Master Gunnary Sergeant (11);
  #Marines E9 - Sergeant Major (12)
    element <-remDr$findElement(using= "xpath", 
                                eval(parse(text=paste0("\"/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/form/div[1]/div/div/div/div[1]/div[3]/select/option[", rank, "]\""))))
    element$clickElement()
    element <-remDr$findElement(using= "css", 'Button[type="submit"]')
    element$clickElement()
  
  #Navy E7 - Chief Petty Officer (23); #Navy E8 - Senior Chief Petty Officer (24); #Navy E9 - Master Chief Petty Officer (25);
  #Navy E9 - Command Master Chief (26); #Navy E9 - Force Master Chief (27); #Navy E9 - Fleet Master Chief (28)
    element <-remDr$findElement(using= "xpath", 
                                eval(parse(text=paste0("\"/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/form/div[1]/div/div/div/div[1]/div[3]/select/option[", rank, "]\""))))
    element$clickElement()
    element <-remDr$findElement(using= "css", 'Button[type="submit"]')
    element$clickElement()  
    
  #Air Force E7 - Master Sergeant (8); #Air Force E8 - Senior Master Sergeant (9); #Air Force E9 - Chief/Command Master Sergeant (10)
    element <-remDr$findElement(using= "xpath", 
                                eval(parse(text=paste0("\"/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/form/div[1]/div/div/div/div[1]/div[3]/select/option[", rank, "]\""))))
    element$clickElement()
    element <-remDr$findElement(using= "css", 'Button[type="submit"]')
    element$clickElement()
    
  #CG E7 - Chief Petty Officer (14); #CG E8 - Senior Chief Petty Officer (15); #CG E9 - Master Chief Petty Officer (16);
  #CG E9 - Command Master Chief (17); #CG E9 - Force Command Master Chief (18); #CG E9 - Area Command Master Chief (19)
    element <-remDr$findElement(using= "xpath", 
                      eval(parse(text=paste0("\"/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/form/div[1]/div/div/div/div[1]/div[3]/select/option[", rank, "]\""))))
    element$clickElement()
    element <-remDr$findElement(using= "css", 'Button[type="submit"]')
    element$clickElement()
    
  #### How Many Drivers ####
    drivers=1
    element <-remDr$findElement(using= "xpath", 
                      eval(parse(text=paste0("\"/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/div/div[1]/button[", drivers, "]\""))))
    element$clickElement()

  #### Driver Demographics ####
    sex="Female"  #Male | Female
    element <-remDr$findElement(using= "css", eval(parse(text=paste0("'[name=\"gender\"][value=\"", sex,"\"]'"))))
    element$clickElement()
    
    marital="Married" #Single | Married
    element <-remDr$findElement(using= "css", eval(parse(text=paste0("'[name=\"maritalStatus\"][value=\"", marital,"\"]'"))))
    element$clickElement()
    
    tickets=0 #0=None, 1=1, 2=2 or More
    element <-remDr$findElement(using= "css", eval(parse(text=paste0("'[name=\"tickets\"][value=\"", tickets,"\"]'"))))
    element$clickElement()

    accidents=0 #0=None, 1=1, 2=2 or More
    element <-remDr$findElement(using= "css", eval(parse(text=paste0("'[name=\"accidents\"][value=\"", accidents,"\"]'"))))
    element$clickElement()
      
    #Find Text Box for Date of Birth
      element <-remDr$findElement(using = "css",'input[type="text"]')
    #Send text to input box. don't forget to use `list()` when sending text
      element$sendKeysToElement(list("09/30/1992"))
      element <-remDr$findElement(using= "css", 'Button[type="submit"]')
      element$clickElement()
      see_page()

  #### How Many years Have You Been With Current insurer? ####
      #Less than 1 (less-than-one); #1 to 2 (one-to-two); 3 to 5 (three-to-five); 
      #6 to 8 (six-to-eight); 9 to 11 (nine-to-eleven); 12 or more ();
      current_insurer="nine-to-eleven"
      element <-remDr$findElement(using = "css", eval(parse(text=paste0("'Button[id=\"",current_insurer, "\"]'"))))
      #element$getElementText()[[1]]
      element$clickElement()

  #### How Is Your Credit? ####
      #Not Sure (not-sure); #Poor (poor); #Below Average (below-average); #Average (average);
      #Good (good); #Excellent (excellent)
      credit="average"
      element <-remDr$findElement(using= "css", eval(parse(text=paste0("'Button[id=\"", credit, "\"]'"))))
      element$clickElement()

  #### How Many Vehicles ####
      #1 (vehicle-1); #2 (vehicle-2); #3 (vehicle-3)
      vehicle="vehicle-1"
      element <-remDr$findElement(using= "css", eval(parse(text=paste0("'Button[id=\"", vehicle, "\"]'"))))
      element$clickElement()
  
  #### Vehicle Information ####   
      veh_year=2019 #1981 - 2022
      element <-remDr$findElement(using= "xpath", 
                        "/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/form/div/div/div[2]/div/div[1]/div/div[1]/div[3]/select")
      element <-remDr$findElement(using= "css", 
                                  eval(parse(text=paste0("'[value=\"", veh_year, "\"]'"))))
      element$clickElement()
      element <-remDr$findElement(using= "xpath", 
                        "/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/form/div/div/div[2]/div/div[2]/div/div[1]/div[3]/select")
      make="MAZDA"
      element <-remDr$findElement(using= "css", 
                                  eval(parse(text=paste0("'[value=\"", make, "\"]'"))))
      element$clickElement()
      element <- remDr$findElement(using = "xpath", 
                        "/html/body/div/div/div/div/div[3]/div[1]/div[2]/div[2]/div[2]/div/div/form/div/div/div[2]/div/div[3]/div/div[1]/div[3]/select")
      model="3 4D G.TOU"
      element <-remDr$findElement(using= "css", 
                                  eval(parse(text=paste0("'[value=\"", model, "\"]'"))))
      element$clickElement()
      element <-remDr$findElement(using= "css", 'Button[type="submit"]')
      element$clickElement()
      
  #### Username Text ####
      element <-remDr$findElement(using = "css",'input[type="text"][name="first-name"]')
      element$sendKeysToElement(list("Al"))
      element <-remDr$findElement(using = "css",'input[type="text"][name="last-name"]')
      element$sendKeysToElement(list("Bee"))
      random_num<-sample(100:100000, 1, replace=T)
      element <-remDr$findElement(using = "css",'input[type="text"][name="email"]')
      element$sendKeysToElement(list(paste0("AB", random_num, "@msn.com")))
      element <-remDr$findElement(using= "css", 'Button[type="submit"]')
      element$clickElement()
      
  #### Pulling Quote Data ####
      doc <- htmlParse(element$getPageSource()[[1]])
      
      quotes<- xml2::read_html(remDr$getPageSource()[[1]]) %>%
        rvest::html_nodes("#basic-package-wrapper") %>%
        html_children() %>% html_text()
      all_quotes<-data.frame(quotes[1]) %>% dplyr::rename(Basic=1)
      
      quotes<- xml2::read_html(remDr$getPageSource()[[1]]) %>%
        rvest::html_nodes("#standard-package-wrapper") %>%
        html_children() %>% html_text() 
      all_quotes$Standard<- quotes[1]
      
      quotes<- xml2::read_html(remDr$getPageSource()[[1]]) %>%
        rvest::html_nodes("#expanded-package-wrapper") %>%
        html_children() %>% html_text() 
      all_quotes$Expanded<-quotes[1]
      
#Add in Quote Details
    all_quotes$branch<-branch
    all_quotes$credit<-credit
    all_quotes$current_ins<-current_insurer
    all_quotes$sex_ni<-sex
    all_quotes$marital_status<-marital
    all_quotes$drivers<-drivers
    all_quotes$accidents<-accidents
    all_quotes$tickets<-tickets
    #all_quotes$zip<-zip_code

remDr$close()




