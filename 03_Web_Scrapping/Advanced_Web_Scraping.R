#If you are getting errors see: https://stackoverflow.com/questions/45395849/cant-execute-rsdriver-connection-refused
#shell('docker container ls -a')  #Shows what is open. 
#If you are getting an error about execution failed 125 then be sure to close previous containers
#shell('docker rm -f <container name>) #Kills prevous connection
#If you get Selenium unable to create session from error
  #Update Webbrowser
  #Update webdriver example: webdriver-manager update --chromedriver

library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(httr)

see_page<-function(x){
  remDr$maxWindowSize()
  remDr$screenshot(display = TRUE)
}

end_page<-function(x){
  element <- remDr$findElement("css", "body")
  element$sendKeysToElement(list(key = "end"))
}

#Locate the browswer you will use. Here we use Chrome
#browser = c("chrome", "firefox", "phantomjs", "internet explorer")
  #shell('docker-machine ip default')
  #shell('curl http://192.168.99.100:4445')
  #shell('docker pull atsnngs/phantomjs')
  shell('docker pull selenium/standalone-chrome') 
  shell('docker run -d -p 4445:4444 selenium/standalone-chrome')
  remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445, browserName = "chrome")

#Open Driver
  remDr$open()

#Go to website - Here we look at ToScrape.com
  remDr$navigate("http://toscrape.com/")
  remDr$navigate("http:/r-project.org")
  remDr$navigate("http://google.com/ncr")
  remDr$navigate("https://www.usaa.com/inet/wc/insurance-products")
  
  see_page()
  
  #Get Page Titel
  remDr$getTitle()
  
  #Find Element for Drop Down
  element <-remDr$findElement(using= "css", 'Select[id="quote-select"]')
  element$clickElement()
  see_page()
  element$clickElement()
  
  #Find Button To Start Quote
  element <-remDr$findElement(using= "css", 'Button[type="submit"]')
  see_page()
  element$clickElement()
  see_page()
  
#Find Session Info
  sessionInfo()
  
#Close the Driver
  remDr$close()
  
shell("docker container ls -a")
