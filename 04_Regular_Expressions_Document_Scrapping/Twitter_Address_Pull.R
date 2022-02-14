#Load Libraries
library(plyr)
library(stringr)
library(Hmisc)
library(tictoc)
library(data.table)

#### Functions ####
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

rm_double<-function ( x ){
  gsub("  ", " ", x) #Remove double spaces to make some other search processes work correctly
}

Clean_Address<-function(vars, cl, tl, num){
  #vars<-gsub(" tx | texas", "", vars)  #Remove State
  vars<-gsub("[<|>|}|{|.|%|&|~|$|*|+|?|!|,|:|)|(|'|-|\"|;|#|-]", "", vars)  #Eliminate Special Characters
  
  #vars<-paste0(' ', vars, ' ') #Add Space Before and After City to Capture It
  #for(i in 1:length(cl)){  #Remove Cities
  #  vars<-gsub(eval(parse(text=paste0('"', cl[i], '"'))), " ", vars)
  #}
  #for(i in 1:length(tl)){  #Removing Street Types
  #  vars<-gsub(eval(parse(text=paste0('" ', tl[i],'[,. ]"'))), " ", vars)
  #}
  #vars<-ifelse(grepl("([0-9]+[Tt][Hh])|([0-9]+[Rr][Dd])|([0-9]+[Ss][Tt])", vars), vars, 
  #                 gsub("([0-9])", "", vars)) #Remove Numbers  
  vars<-ifelse((sapply(gregexpr("[A-z0-9]+", vars), function(x) sum(x > 0)))>num, NA, vars)
  
  vars<-ifelse(grepl("([h][u][r][r][i][c][a][n][e][ ])|([m][p][h][ ])|([r][a][i][n][ ])|([m][i][l][e][s])", vars), NA, vars)
  vars<-trim(vars)
  vars<-ifelse(vars==" ", NA, vars)
  vars<-ifelse(vars=="", NA, vars)
  return(vars)
}  

#### Create List of Street Endings and What They Should be Changed to ####
el_state<-c("[Ff][Mm] [0-9]", "[Rr][Oo][Uu][Tt][Ee] [0-9]", "[Ll][Oo][Oo][Pp] [0-9]",
            "( [Dd][Rr][\\., ])|([Dd][Rr][Ii][Vv][Ee] )", "( [Ll][Nn][\\., ])|( [Ll][Aa][Nn][Ee])", "( [Rr][Dd][\\., ])|( [Rr][Oo][Aa][Dd])", 
            "( [Cc][Ii][Rr][\\., ])|([Cc][Rr][\\., ])|([Cc][Ii][Rr][Cc][Ll][Ee])", " [Bb][Ll][Vv][Dd][\\., ]", "( [Cc][Oo][Uu][Rr][Tt][ ])|([^Mm][ ][Cc][Tt][, ])", 
            " [Tt][Ee][Rr][Rr][EeAa][Cc][Ee]",  "( [Aa][Ll][Ll][Ee][Yy])|( [Aa][Ll][Yy])|( [Aa][Ll][Ll][Yy])", "( [Ss][Qq][Uu][Aa][Rr])|( [Ss][Qq][\\., ])", 
            "( [Pp][Ll][Aa][Zz][Aa])", "( [Hh][Ii][Gg][Hh][Ww][Aa][Yy])|( [Hh][Ww][Yy])", "( [Ss][Tt] )|( [Ss][Tt][\\.])|( [Ss][Tt][Rr][Ee][EeTt][Tt\\., ])",
            "( [Aa][Nn][Nn][Ee][Xx])", "( [Aa][Rr][Cc][Aa])", "( [Bb][Aa][Yy][Oo][Uu])", "( [Bb][Ll][Uu][Ff])", "( [Bb][Rr][Oo][Oo][Kk])",
            "( [Bb][Rr][Uu][Gg][ ])", "( [Cc][Aa][Yy][Oo][Nn] )", "( [Cc][Aa][Pp][Ee][ ])", "( [Cc][Aa][Uu][Ss][Ee][Ww][Aa][Yy])", "( [Cc][Ll][Ii][Ff][Ff])",
            "( [Cc][Oo][Vv][Ee][\\\\., ])", "( [Cc][Rr][Ee][Ss][Cc][Ee][Nn][Tt])", "( [Ee][Ss][Tt][Aa][Tt][Ee])", "( [Ee][Xx][Pp][Rr][Ee][Ss][Ss][Ww][Aa][Yy])",
            "( [Ff][Aa][Ll][Ll][Ss])", "( [Ff][Ee][Rr][Rr][Yy])", "( [Ff][Oo][Rr][Gg][Ee])", "( [Ff][Rr][Ee][Ee][Ww][Aa][Yy])", "( [Gg][Aa][Tt][Ee][Ww][Aa][Yy])",
            "( [Gg][Ll][Ee][Nn])", "( [Hh][Aa][Rr][Bb][Oo][Rr])", "( [Hh][Aa][Vv][Ee][Nn])", "( [Hh][Ee][Ii][Gg][Hh][Tt][Ss])", "( [Hh][Ii][Gg][Hh][Ww][Aa][Yy])",
            "( [Hh][Oo][Ll][Ll][Oo][Ww])", " ( [Ii][Nn][Ll][Ee][Tt])", "( [Jj][Uu][Nn][Cc][Tt][Ii][Oo][Nn])", "( [Kk][Nn][Oo][Ll][Ll])",
            "( [Ll][Oo][Dd][Gg][Ee])", "( [Mm][Aa][Nn][Oo][Rr])", "( [Mm][Ee][Aa][Dd][Oo][Ww][Ss ])", "( [Mm][Ii][Ll][Ll][Ss ])", "( [Mm][Oo][Uu][Nn][Tt])",
            "( [Oo][Rr][Cc][Hh][Aa][Rr][Dd])", "( [Pp][Aa][Rr][Kk][Ww][Aa][Yy])", "( [Rr][Ii][Dd][Gg][Ee])", "( [Ss][Hh][Oo][Rr][Ee][Ss ])",
            "( [Ss][Pp][Rr][Ii][Nn][Gg][Ss ])", "( [Ss][Uu][Mm][Mm][Ii][Tt])", "( [Tt][Rr][Aa][Ff][Ff][Ii][Cc][Ww][Aa][Yy])", "( [Tt][Rr][Aa][Ii][Ll])",
            "( [Tt][Uu][Rr][Nn][Pp][Ii][Kk][Ee])", "( [Vv][Ii][Aa][Dd][Uu][Cc][Tt])", "( [Vv][Ii][Ll][Ll][Aa][Gg][Ee])", "( [Vv][Ii][Ss][Tt][Aa])",
            "( [Tt][Uu][Nn][Nn][Ee][Ll])", "( [Vv][Aa][Ll][Ll][Ee][Yy])", " ([Ss][Qq][Uu][Aa][Rr][Ee])", "( [Cc][Rr][Ee][Ee][Kk][ ])", 
            "( [Bb][Yy][Pp][Aa][Ss][Ss][ ])", "( [Ff][Ii][Ee][Ll][Dd][Ss ])", "( [Ff][Oo][Rr][Ee][Ss][Tt])", "( [Gg][Aa][Rr][Dd][Ee][Nn][Ss ])",
            "( [Bb][Rr][Ii][Dd][Gg])", "( [Bb][Ee][Nn][Dd])", "( [Cc][Rr][Oo][Ss][Ss][Ii][Nn][Gg])", "( [Ff][Oo][Rr][Tt])", "( [Hh][Ii][Ll][Ll][Ss ])",
            "( [Ll][Aa][Nn][Dd][Ii][Nn][Gg])", "( [Ll][Aa][Kk][Ee][Ss ])", "( [Mm][Oo][Uu][Nn][Tt][Aa][Ii][Nn])", "( [Rr][Aa][Nn][Cc][Hh])",  
            "( [Rr][Ii][Vv][Ee][Rr])", "( [Pp][Ll][Aa][Cc][Ee])",  "( [Bb][Ee][Aa][Cc][Hh])", "( [Ww][Aa][Yy])",
            "([Tt][Xx])|([Tt][Ee][Xx][Aa][Ss])|([Hh][Tt][Xx])")
elc_state<-c(" FM ", " ROUTE ", " LOOP ", " DR " , " LN ", " RD " , " CIR " , " BLVD ", " CT ", " TER ", " ALY ", " SQ ", " PLZ ", " HWY "  ,
             " ST " , " ANX ", " ARC  ", " BYU " ,  " BLF ", " BRK ", " BURG ", " CYN ", " CPE ", " CSWY ", " CLF ", " CV ", " CRES ",
             " EST ", " EXPY ", " FLS ", " FRY ", " FRG ", " FWY ", " GTWY ", " GLN ", " HBR ", " HVN ", " HTS ", " HWY ", " HOLW ", " INLT ",
             " JCT ", " KNLS ", " LDG ", " MNR ", " MDWS ", " MLS ", " MT ", " ORCH ", " PKWY ", " RDG ", " SHR ", " SPG ", " SMT ", " TRFY ",
             " TRL ", " TPKE ", " VIA ", " VLG ", " VIS ", " TUNL ", " VLY ", " SQ ",  " CRK ",  " BYP", " FLD ", " FRST ", " GDNS ", " BRG ", 
             " BND ", "XING", " FRG ", " HL ", " LNDG ",  " LK ", " MT ", " RNCH ", " RIV ", " PL ", " BCH ", " WAY ",
             " TX ")
el<-subset(el_state, el_state!="([Tt][Xx])|([Tt][Ee][Xx][Aa][Ss])|([Hh][Tt][Xx])")
elc<-subset(elc_state, elc_state!=" TX ")
citylist<-c(" houston", " beaumont", " dickerson", " bellaire", " spring", " rockport", " dickinson", " dickenson", " katy", 
            " alegro", " pasadena", " freeport", " richmond", " hou ", " crosby", " hankamer", " shepard", " humble",
            " cypress", " alvord", " sugarland", " sugar land", " grange", " hankemer", " crosby", " the woodlands", " san antonio", " dallas",
            " victoria", " austin", " palacios", " la marque", " corpus christi", " fort worth", " bryan", " sweeny")
citylist_upper<-toupper(citylist)

###Bring in Tweets####tic("typelist_time")
tic("Full Process")
add_geo_all_list<-list() #Create Empty dataset to bind all addresses to.

#for(j in 1:16){
for(j in 1:1){
  tic(paste0("Process #", j))
  
  download.file("https://github.com/mattdemography/STA_6233/blob/master/Data/tweets_08_28_Matt.zip?raw=true", mode="wb", destfile = "./tweets.zip")
  unzip("./tweets.zip", unzip="internal")
  tweets_main<-fread(paste0("./tweets_08_28_Matt.csv"), fill=T, blank.lines.skip = T)
  #tweets_main<-read.csv(paste0("~/social_media_cat_system/Harvey_Modeling_", j, ".csv"))
  
  tweets_main<-as.data.frame(tweets_main)
  myvars<-c("text", "id") #Keep enough to bring back to main dataset if needed
  t<-tweets_main[myvars]
  t$full_text<-as.character(t$text)
  
  #Clean Tweets
  t$full_text<-iconv(t$full_text, "latin1", "ASCII", sub="")  #Remove invalid ASCII characters (emoticons) from text. Not doing this messes with some processes
  t$text1<-gsub("RT ", " ", t$full_text) #Remove 'RT'
  t$text1<-gsub("\\n", " ", t$text1) %>% rm_double()  #Makes String Exclude line markers
  t$text1<-paste0(' ', t$text1, ' ') #Add Space in Beginning and End to find  address numbers at start of string
  t$text1<-tolower(t$text1)
  t$text1<-gsub("[a-z0-9.]*/([a-z0-9:/.]*)", " ", t$text1) %>% rm_double()  #Remove any links
  t$text1<-gsub("#([a-z0-9.]* )", " ", t$text1) %>% rm_double()  #Remove Any Words Attached to '#'
  t$text1<-gsub("@([a-z0-9]*: )", " ", t$text1) %>% rm_double()  #Remove Any Words Attached to '@'
  t$text1<-gsub("@([a-z0-9.<?_]*)", " ", t$text1) %>% rm_double() #Remove Any Words Attached to '@'
  t$text1<-gsub("[0-9][0-9][0-9][-][0-9][0-9][0-9][-][0-9][0-9][0-9][0-9]", " ",t$text1) %>% rm_double()  #Delete Phone Numbers
  t$text1<-gsub("(([0-9])*[.-]([0-9])*[.-]([0-9])*)", "", t$text1) %>% rm_double() #Eliminate Numeric Dates
  t$text1<-gsub("[|<|>|\\}|\\{|.|%|&|~|$|*|+|?|!|,|:|)|(|'|-|\"|;|#|-]", " ", t$text1) %>% rm_double()   #Eliminate Special Characters
  t$text1<-gsub("[0-9:. ]*([ap][m][ ][ecmp][dt][t ])|[0-9:. ]*([ap][m][ ])", " ", t$text1)  %>% rm_double()  #Remove Times
  t$text1<-gsub("((jan|january|feb|february|march|apr|april|may|june|july|aug|august|sep|september|oct|october|nov|november|dec|december)( [0-9. ]+[^a-z]))", 
                "", t$text1) #Remove Dates
  t$text1<-gsub("[ ][c][a][t][1-5 ][1-5 ]|[ ][c][a][t][e][g][o][r][y][1-5 ][1-5 ]", " ", t$text1) %>% rm_double() #Remove Hurricane Cat Number
  
  #Count and Remove Duplicate Text
  t$counter<-dplyr::recode(t$id, "\" \"=0; else=1")
  t$dup_text<-ifelse(duplicated(t$text1), 1, 0)
  t<-subset(t, t$dup_text==0)
  
  #Mark and Keep Text with Numbers - Possible Address
  t$numerical<-ifelse(grepl("[0-9]", t$text1), 1, 0)
  t<-subset(t, t$numerical==1)
  t<-t[,c(1:4)]   #Limit dataset to keep size small - not sure how much this helps.
  
  ####Start Creating Address Field ####
  t$pos<-regexpr(" [0-9]", t$text1)
  t$txt_st_end<-str_sub(t$text1, t$pos)
  t$txt_st_end<-ifelse(t$pos<=0, "", t$txt_st_end)
  
  #Keep Only Fields with Potential Addresses
  add<-subset(t, t$txt_st_end!="")
  add<-subset(add, !duplicated(add$txt_st_end))
  rownames(add)<-1:nrow(add)
  
  #Create Text Formats for Regular Expression Code
  add$txt_st_end_cap<-add$txt_st_end
  for(i in 1:length(el)){
    add$txt_st_end_cap<-eval(parse(text=paste0("gsub(el[i], elc[i], add$txt_st_end_cap)")))
  }
  add$txt_st_end_cap<-rm_double(add$txt_st_end_cap) #Remove double spaces to make some other search processes work correctly
  add$txt_st_end_cap_gsub<-gsub("([^'A-Z'])*$", "", add$txt_st_end_cap) #Back Remove
  
  add$txt_city_cap<-add$txt_st_end
  for(i in 1:length(citylist_upper)){
    add$txt_city_cap<-eval(parse(text=paste0("gsub(citylist[i], citylist_upper[i], add$txt_city_cap)")))
  }
  add$txt_city_cap<-rm_double(add$txt_city_cap) #Remove double spaces to make some other search processes work correctly
  add$txt_city_cap<-gsub("([^'A-Z'])*$", "", add$txt_city_cap) #Back Remove
  
  #Mark Evidence of Street Endings - Pre Data
  add$st_type_pre<-NA  
  #add$st_type_post<-NA  
  add_keep<-subset(add, add$txt_st_end_cap_gsub=="")
  add_run<-subset(add, add$txt_st_end_cap_gsub!="")
  for(i in 1:length(el)){
    eval(parse(text=paste0("add_run$st_type_pre<-ifelse(is.na(add_run$st_type_pre) & grepl(el[i], add_run$txt_st_end), elc[i], ifelse(!is.na(add_run$st_type_pre), add_run$st_type_pre, NA))")))
  }
  add_run$st_type_pre<-trim(add_run$st_type_pre)
  
  #Create Street Type List
  typelist<-data.frame(table(add_run$st_type_pre)) 
  typelist<-typelist[order(typelist$Freq, decreasing = T),]
  typelist<-typelist[typelist$Var1 != "WAY",] #Remove 'Way' from list since it is often used in non-roads
  typelist<-unique(typelist$Var1[typelist$Var1 != c("","FM") | !is.na(typelist$Var1)]) %>% tolower()
  typelist<-c(typelist, "way") #add_run it back to list to run through the algo last.
  typelist_upper<-trim(typelist) %>% toupper()
  
  #Mark Evidence of Street Endings - Post Data
  #for(i in 1:length(typelist)){
  #  eval(parse(text=paste0("add_run$st_type_post<-ifelse(is.na(add_run$st_type_post) & grepl(typelist_upper[i], add_run$txt_st_end_cap), typelist_upper[i], ifelse(!is.na(add_run$st_type_post), add_run$st_type_post, NA))")))
  #}
  #add_run$st_type_post<-trim(add_run$st_type_post)
  #add<-rbind(add_keep, add_run)
  
  #Create City List  
  add$City<-NA
  add_keep<-subset(add, add$txt_city_cap=="")
  add_run<-subset(add, add$txt_city_cap!="")
  for(i in 1:length(citylist_upper)){
    eval(parse(text=paste0("add_run$City<-ifelse(is.na(add_run$City) & grepl(citylist_upper[i], add_run$txt_city_cap), citylist_upper[i], ifelse(!is.na(add_run$City), add_run$City, NA))")))
  }
  add_run$City<-trim(add_run$City)
  add<-rbind(add_keep, add_run)
  
  #Create State Variable
  add$State<-str_extract(add$text1, pattern ="([Tt][Xx])|([Tt][Ee][Xx][Aa][Ss])|([Hh][Tt][Xx])") %>% toupper()
  add$State<-ifelse(add$State=="TEXAS"|add$State=="TX"|add$State=="HTX", "TX", "")
  
  #Mark Evidence of Zip Code
  add$Zip<-str_extract(add$text1, pattern =" [7][0-9][0-9][0-9][0-9][, ]")
  
  #Grab From Street Ending to Start of (Potential) Street Number
  # First Grab Numbered Streets, then Everything Before a Zip Code, then Everything Before a Street Type, then city and Clean In Between Steps #
  #### Grab Everything Before Zip Code ####
  add$Street<-NA
  add_keep<-subset(add, !is.na(add$Street) | is.na(add$Zip))
  add_run<-subset(add, is.na(add$Street) & !is.na(add$Zip))
  add_run$Street<-str_extract(add_run$txt_st_end_cap, pattern="([0-9].*[A-z, ]*( [7][0-9][0-9][0-9][0-9]))") %>% tolower()
  add_run$Street<-Clean_Address(add_run$Street, citylist, typelist, 10) #Run Clean_Address Function
  add_run$Street<-ifelse(add_run$Street=="NA", NA, add_run$Street)
  add<-rbind(add_keep, add_run)  
  add$Street<-ifelse(!is.na(add$Street), paste(' ', add$Street, ' ', sep = ""), add$Street) #Add Space in Beginning and End to find  address numbers at start of string
  
  #### Grab Everything Before St Type - Pre #####
  add_keep<-subset(add, is.na(add$st_type_pre) | !is.na(add$Street))
  add_run<-subset(add, !is.na(add$st_type_pre) & is.na(add$Street)) #Subset so that only rows with street endings are analyzed in the next step
  add_run$Street<-ifelse(is.na(add_run$Street), str_extract(add_run$txt_st_end_cap_gsub, pattern="([0-9 ].*[a-z, ]*( [A-Z].*))"), 
                         add_run$Street) %>% tolower()
  add_run$Street<-Clean_Address(add_run$Street, citylist, typelist, 7)
  add<-rbind(add_keep, add_run)
  
  #### Grab Everything Before City Names ####
  add_keep<-subset(add, is.na(add$City) | !is.na(add$Street))
  add_run<-subset(add, !is.na(add$City) & is.na(add$Street)) #Subset so that only rows with Cities are analyzed in the next step
  add_run$Street<-ifelse(is.na(add_run$Street), str_extract(add_run$txt_city_cap, pattern="([0-9 ].*[a-z, ]*( [A-Z].*))"), 
                         add_run$Street) %>% tolower()
  add_run$Street<-Clean_Address(add_run$Street, citylist, typelist, 8)
  add_run$Street<-ifelse(add_run$Street=="NA", NA, add_run$Street)
  add<-rbind(add_keep, add_run)
  
  #Deal with Unique Street Types and then Fill in Street. These streets are a different format than what was done above.
  add$Street_Capture<-NA
  add$Street_Capture<-ifelse(grepl(("([F][M][ ])[0-9]"), add$txt_st_end_cap), str_extract(add$txt_st_end_cap, pattern=("( [F][M]).[0-9]*")), add$Street_Capture) #Deal with Farm Roads
  add$Street_Capture<-ifelse(grepl(("([H][W][Y][ ])[0-9]"), add$txt_st_end_cap), str_extract(add$txt_st_end_cap, pattern=("( [H][W][Y][ ])[0-9]*")), add$Street_Capture)#Deal with Highway
  add$Street_Capture<-ifelse(grepl(("([R][O][U][T][E][ ])[0-9]"), add$txt_st_end_cap), str_extract(add$txt_st_end_cap, pattern=("( [R][O][U][T][E][ ])[0-9]*")), add$Street_Capture)#Deal with Route
  add$Street_Capture<-ifelse(grepl(("([L][O][O][P][ ])[0-9]"), add$txt_st_end_cap), str_extract(add$txt_st_end_cap, pattern=("( [L][O][O][P][ ])[0-9]*")), add$Street_Capture)#Deal with Loop
  
  add_keep<-subset(add, is.na(add$Street_Capture))
  add_run<-subset(add, !is.na(add$Street_Capture))
  add_run$Street<-ifelse(grepl(("[0-9][ ][F][M][ ][0-9]"), add_run$txt_st_end_cap), 
                         str_extract(add_run$txt_st_end_cap, pattern=("([0-9]*).[F][M][ ][0-9]*")), add_run$Street)
  add_run$Street<-ifelse(grepl(("[0-9][ ][H][W][Y][ ][0-9]"), add_run$txt_st_end_cap), 
                         str_extract(add_run$txt_st_end_cap, pattern=("([0-9]*).[H][W][Y][ ][0-9]*")), add_run$Street)
  add_run$Street<-ifelse(grepl(("[0-9][ ][R][O][U][T][E][ ][0-9]"), add_run$txt_st_end_cap), 
                         str_extract(add_run$txt_st_end_cap, pattern=("([0-9]*).[R][O][U][T][E][ ][0-9]*")), add_run$Street)
  add_run$Street<-ifelse(grepl(("[ ][L][O][O][P][ ][0-9]"), add_run$txt_st_end_cap), 
                         str_extract(add_run$txt_st_end_cap, pattern=("([0-9]*.[a-z]*).[L][O][O][P][ ][0-9]*")), add_run$Street)
  add<-rbind(add_keep, add_run)
  
  #### Grab Numbered Streets#### - Takes too much time (32sec) for limited reward(24 iffy streets added)
  #  add_keep<-subset(add, !is.na(add$Street))
  #  add_run<-subset(add, is.na(add$Street))
  #  add_run$Street<-str_extract(add_run$txt_st_end_cap, pattern=("([0-9].|[0-9][ ][a-z]).+[0-9]+[trsn][hdtd]")) #th
  #  add_run$Street<-Clean_Address(add_run$Street, citylist, typelist, 6)
  #  add<-rbind(add_keep, add_run)
  
  #Clean the City Variable
  add$City<-ifelse(add$City=="SUGAR LAND", "SUGARLAND", ifelse(add$City=="DICKERSON", "DICKINSON", ifelse(add$City=="DICKENSON", "DICKINSON", add$City)))
  add$City<-ifelse(grepl("CYPRESS CREEK", add$txt_city_cap), "HOUSTON", ifelse(grepl("CYPRESS STATION", add$txt_city_cap), "HOUSTON", add$City))
  
  #Mark Text with evidence of shelters (these need to be geocoded/marked differently)
  add$Shelter<-ifelse(grepl(("[Ss][Hh][Ee][Ll][Tt][Ee][Rr]"), add$text1), 1, 0)
  add$Rescue<-ifelse(grepl(("[Rr][Ee][Ss][Cc][Uu][Ee]"), add$full_text), 1, 0)
  add$Mentions<-ifelse(grepl(("[Uu][Ss][Cc][Gg]|[Cc][Aa][Jj][Uu][Nn][Nn][Aa][Vv][Yy]"), add$full_text), 1, 0)
  
  #### Make Address Clean for Geocoding ####
  add$Street<-toupper(add$Street) %>% trim()
  add_keep<-subset(add, !is.na(add$Street))
  
  #Look at how many are shelters
  table(add_keep$Shelter)

  #Keep File to be Geocoded
  add_geo<-subset(add_keep, !duplicated(add_keep$Street))
  add_geo$file<-j
  #write.csv(add_geo, paste0("~/social_media_cat_system/Addresses_", j, ".csv"))
  
  myvars<-c("id", "Street", "City", "State", "Zip", "Shelter", "Rescue", "Mentions", "file")
  add_geo<-add_geo[myvars]
  add_geo_all_list[[j]]<-add_geo
  
  Sys.sleep(1)
  toc()
}

add_geo_all=do.call(rbind, add_geo_all_list)
#write.csv(add_geo_all, paste0("~/social_media_cat_system/Addresses_All.csv"))

Sys.sleep(1)
toc()
