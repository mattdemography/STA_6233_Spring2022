---
title: "Feedback for Project Number xx"
output:
  word_document: default

---
## Instructions

Use the program below (beginning line 13) to figure out who you will be critiquing. You will critique two of your colleagues' work and provide feedback for them to produce their best work. Make sure you use *your number as it aligns with the Google Document's Position Number column* in the program below (line 16). Remember this is to help them out so do not be mean, but also do not hold back. Be sure to highlight areas of success as equally as areas of failure. Use the template below the code to provide them this feedback. You can find the [project links here](https://docs.google.com/spreadsheets/d/1Z42jxyeUAFtKT4EJ9R3uNlhOddUYBNlRj9eVhquv68M/edit?usp=sharing)

**Be sure to change the name of the project in the title above and in the document title.** This is the project number you are critiquing and should be included in the output file so that the document name reads 'Critique_of_Project##.docx'. Once you are finished, e-mail me both documents (one document per critique) and I will upload them to the 'Peer_Reviewed_Work' folder so that your peers can view them. Do not e-mail me the .Rmd file. These reviews are due by **Sunday, May 8th at 11:59pm CST**. For ease you can use this markdown file to fill out your responses and knit which will produce a word document for you.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
#Insert Your Number Here
your_num<-12

set.seed(160)
s<-sample(1:34, 34, replace=F)
pos<-match(your_num, s)
if(pos>1 & pos<34){
  feedback_nums<-c(s[pos+1], s[pos-1])
}else if(pos==1){
  feedback_nums<-c(s[pos+1], s[length(s)])
}else if(pos==34){
  feedback_nums<-c(s[length(pos)], s[pos-1])
}

print(paste0("You will provide feeback for projects ", feedback_nums[1], " and ", feedback_nums[2]))

```
## Feedback Below

**What did you first notice about this project?**

**What was this project's main story?**

**What were some areas of improvement?**

**What elements would you add to this project?**

**What were some successful elements of this project?**

**Any other thoughts you would like to convey to your peer?**


