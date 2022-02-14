#Bring In Libraries
library(rvest)
library(dplyr)
library(plyr)
library(data.table)

#Create a trim function which uses regular expressions to clean white space
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

#### First Grab Teams and Players on Each Team ####
teams<-c("ATL", "BOS", "BRK", "CHI", "CHO", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM",
         "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
all_players<-data.frame(Player_Name=character(), team=character())  #Creating empty dataset to be filled with loop

for(i in 1:length(teams)){
  #Website to scrap
  theurl<-paste0("http://www.basketball-reference.com/teams/",teams[i],"/2019.html#all_roster") 
  
  #Download/Read the html
  html<- read_html(theurl)
  
  #I use CSS selector to figure out what table to read
  get_roster<-html_nodes(html,"#roster") 
  
  #Make previous object into a table
  table_p<-html_table(get_roster) 
  
  #Keep only players in this table
  players<-data.frame(table_p[[1]][["Player"]])
  players_dt<-data.table(table_p[[1]][["Player"]])
  players<-players %>% dplyr::rename(Player_Name=1)
  
  #Add team name to table 
  players$team<-teams[i]
  
  #Build Container for links
  all_links<-data.frame(html=character()) 
  for(j in 1:nrow(players)){
    #This is the player link to grab their stats later
    get_links<-html_nodes(html, paste0("tr:nth-child(", j,") a")) %>% html_attr("href") 
    link<-data.frame(get_links[1])
    link<-link %>% dplyr::rename(html=1)
    all_links<-rbind(all_links, link) #Bind All Link Data
  }
  
  #Column Bind player data and links
  players<-cbind(players, all_links)
  
  all_players<-rbind(all_players, players)  #Bind all players into one table
  print(paste0("Finished Team: ", teams[i])) #See where we are in the process
}

#Do some final cleaning of player data by adding first and last names
  all_players$first_name<-trim((laply(strsplit(as.character(all_players$Player_Name), split = " "), "[",1)))
  all_players$last_name<-trim((laply(strsplit(as.character(all_players$Player_Name), split = " "), "[",2)))


#### Now Let's Grab Real Stats ####
player<-all_players$Player_Name
team<-all_players$team
  
#Eliminate the .html to create the file path below
all_players$html_new<-trim(gsub(".html", "", all_players$html))
path<-all_players$html_new

#Create Blank Tables
stats<-data.frame(Player=character(),ID=character(), Year=character(), G=character(), Date=character(), Age=character(), Tm=character(),
                Place=character(), Opp=character(), Mp=character(), FG=character(), FGA=character(), 'FG%'=character(),
                '3P'=character(), '3PA'=character(), '3P%'=character(), FT=character(),
                FTA=character(), ORB=character(), TRB=character(), AST=character(), STL=character(), BLK=character(),
                TOV=character(), 'FT%'=character(), PF=character(), PTS=character(),ORB=character(), DRB=character(),
                TRB=character(), AST=character(), STL=character(), BLK=character(), TOV=character(), PF=character(),
                PTS=character(), GmSc=character(),'+/-'=character())

ispresent<-data.frame(player=character(), team=character())
tab<-data.frame(playerid=character(), team=character())

#Create URL of Game Logs
theurl<-paste("http://www.basketball-reference.com",path,"/","gamelog/2019/", sep="")

######Create Loop#####
for(i in 1:nrow(all_players)){
  tryCatch({
    html<- read_html(theurl[i])
    games<-html_node(html,"#pgl_basic")
    games_table<-html_table(games, fill=T)
    
    #Change names of Blank Columns
      names(games_table)[6]<-"Place"
      names(games_table)[8]<-"Result"
    
    #Remove Games Player Didn't Play
    games_table<- games_table[which(games_table$G!=""),]
      
    #Remove Games that are not stats, but columns
    games_table<- games_table[which(games_table$G!="G"),]
    
    #Make 'G' numeric
    games_table$G<-as.numeric(games_table$G)
    
    #Attach Player Name
    games_table$Player<-player[i]
    
    #Attach Team
    games_table$Team<-team[i]
    
    stats<-rbind(stats, games_table)
    
    tab<-as.data.frame(player[i])
    tab$player<-player[i]
    ispresent<-rbind(ispresent,tab)
    
    print(paste0("Finished with: ", player[i]))
  }, error=function(e){cat(conditionMessage(e))})
  
}

