#Regular Expressions are Native to R, so no special package is needed yet.

#### Simple Matching ####
#Let's create a simple list of four fruits as strings to work with.
fruit<-c("apple", "pear", "orange", "Blueberries.", "banAna.")

#Find exact match
  grep("apple", fruit) #Gives position of match in list
  grepl("apple", fruit) #Gives true/false of each element in list

#Find any string with an 'a'
  grep("[a]", fruit)
  grepl("[a]", fruit)
  
#Return a new list that only includes strings with an 'a'.
  new_fruit<-subset(fruit, grepl("a", fruit)=="TRUE")
  print(new_fruit)
  
#Find any string with an 'a' followed by at least two characters
  grep("[a]..", fruit)

#Find any string where an 'a' is not the first character

#Find any string with a period
  grep("\\.", fruit)
  grep("[.]", fruit)
  
#Find any string that has an 'a' followed by a period

#### More Complex Regular Expressions ####
#Now let's create a new list of phrases
fats<-c("apple pie is good", "apple pie", "apple pie es", "apple", "what is apple", "apple cake 4.87", "923")

#I want to use my anchors to find just the pharse that begins with 'apple'
  grep("^apple", fats)

#I want to use my anchors to find just the pharse that ends with 'apple'
  grep("apple$", fats)

#Now I just want to find the phrase that begins and ends with 'apple'
  
#Find any digits
  grep("\\d", fats)
  grep("[0-9]", fats) #Finds any digit zero through nine. I can change this to only include 1-3.
  grep("[1-3]", fats)
  
#Find any white space
  grep("\\s", fats)

#Find any word with an 'a' or 'p'
  grep("[ap]", fats)
  
  #We could also say any alpha character with
  grep("[a-z]", fats)
    
#Find any digit except a 4, 8, or 7
  grep("[^487]", fats)
  
#Find any phrase that has an 'e' followed by a space and a vowel
  grep("[e][ ][aieou]", fats)

#Find any phrase that has an 'e' followed by a space and any number
  
#Find any phrase that has an 'e' followed by any character except a space

#### Multiple Matches and Repetition ####

mm<-c("grey", "gray", "great")
  
#Find any spelling of the grey/gray
  grep("gr(e|a)y", mm)  
  grep("gr[ea]y", mm)

library(stringr)
year<-"1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
  str_view(year, "CC?")    
  x<-gsub("CC?", "&", year) #0 OR 1
  x  
  
  str_view(year, "CC+")
  x<-gsub("CC+", "&", year) #1 OR MORE
  x  

  str_view(year, 'C[LX]+')  
  x<-gsub("C[LX]+", "&", year)  
  x
  
  number <- "101000000000100"
  #greedy
  regmatches(number, gregexpr(pattern = "1.*1",text = number))
  #non greedy
  regmatches(number, gregexpr(pattern = "1.?1",text = number))
  
