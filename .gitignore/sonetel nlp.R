#setwd("C:/Users/Rahul/Dropbox/DS/etc")
#install.packages("RCurl","XML","rvest","openNLP","rJava","NLP","stringi")
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source") 

library(rJava)
library(RCurl)
library(XML)
library(NLP)
library(rvest)
library(openNLP)

# Step 1

# Read the website content pasted in the file input.txt to the variable s1
#The input required for the code will be the content copied from the target website
# and  pasted in the input.txt file which has to be placed in the working directory. 

s<- read.csv("input.txt")

s1<-paste(s[,1],collapse=" ")

s1<-as.String(s1)

### get the email details 

email <-unlist(regmatches(s[,1], gregexpr("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))", s[,1])))
cat("The email address' is/are :",paste0( email, collapse=","))

#Step 2

########## get possible phone numbers from the content

library(stringi)
phone<-unlist(stri_extract_all_regex(s[,1], '(\\d[.-])?\\(?\\d{3}\\)?[-. ]?\\d{3}[-. ]?\\d{4}\\b'))
cat("The possible phone numbers is/are :",paste0( phone[!is.na(phone)], collapse=","))

                                  
# Step 3

##########get the address details 

#library(openNLP)
  # create Sentance,word and POS tokenizers

sentence <- Maxent_Sent_Token_Annotator()
word <- Maxent_Word_Token_Annotator()
POS<- Maxent_POS_Tag_Annotator()

POS_Ann <- annotate(s1,list(sentence,word,POS))
text_Ann <- annotate(s1,list(sentence,word))

text_Doc<- AnnotatedPlainTextDocument(s1,text_Ann)

words(text_Doc) %>% head(10)


Location <-Maxent_Entity_Annotator(kind = "location")

Organization<-Maxent_Entity_Annotator(kind = "organization")

Dates<-Maxent_Entity_Annotator(kind = "date")

pipeline <-list(sentence,word,Location,Organization,Dates)

text_Ann<-annotate(s1,pipeline)
text_Doc <-AnnotatedPlainTextDocument(s1,text_Ann)

entities <-function(doc,kind){
  w<-doc$content
  a<-annotations(doc)[[1]]
  if(hasArg(kind)){k<-sapply(a$features,'[[',"kind")
    w[a[k==kind]]
    
  } else{
    w[a[a$type=="entity"]]
  }
}

address<-entities(text_Doc,"location")

Org <-entities(text_Doc,"organi")

cat("The address is:",paste0( address, collapse=","))
######################## working hours (not done for now)

#dates<-entities(text_Doc,kind="date")
#wh <-unlist(regmatches(s1, gregexpr("([<day>Mon|Tue|Wed|Thu|Fri|Sat|Sun](?:-(?P<today>Mon|Tue|Wed|Thu|Fri|Sat|Sun))?(?P<from>(?:(?:2[0-3])|(?:[01]?[0-9]))(?:[0-5][0-9])?)-(?P<till>(?<=-)(?:(?:2[0-3])|(?:[01]?[0-9]))(?:[0-5][0-9])?)"

#Step 4 : Final result

################### Complete Output

cat("The email address' is/are :",paste0( email, collapse=","))
cat("The possible phone numbers is/are :",paste0( phone[!is.na(phone)], collapse=","))
cat("The address is:",paste0( address, collapse=","))

write(s1,"tt.txt")
