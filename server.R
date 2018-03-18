#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


#Sys.setlocale("LC_ALL", "French") 
Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
library(shiny)
library(tidyverse)
##library(tidytext)
library(glue)
library(data.table)
library(tm)
library(dplyr)
library(ngram)
#library(caret)
#library(stringr)
#library(gridExtra)
lastword<-function(x){
        return(sub('^.* ([[:alnum:]]+)$', '\\1',x))
        #return(word(x,-1))
}



cleaningbis<-function(file){
        
        
        n<- -1L
        con<-file("mystopwords.txt","rb")
        
        
       
        mystopwords<-readLines(con,n)
        
        close(con)
        
        
        
        corpusFile<-Corpus(VectorSource(file))
        corpusFile<-tm_map(corpusFile, tolower)
        corpusFile<-tm_map(corpusFile,removePunctuation,preserve_intra_word_contractions = FALSE, preserve_intra_word_dashes = FALSE)
        corpusFile<-tm_map(corpusFile, removeWords,mystopwords)
        corpusFile<-tm_map(corpusFile, stripWhitespace)
        FileWord<-corpusFile$content
        FileWord<-gsub("[0-9]","",FileWord)
        FileWord<-gsub("^\\s+|\\s+$","",FileWord)
        FileWord<-gsub("\\s+"," ",FileWord)
        return(FileWord)
}



lastword<-function(x){
        return(sub('^.* ([[:alnum:]]+)$', '\\1',x))
        #return(word(x,-1))
}


nextWord4<-function(strst,ng4,ng3) {
        d<-c("",0)
        if (str_count(strst, "\\S+")>2) {
                pattern<-paste(lastword(gsub("\\s*\\w*$", "", gsub("\\s*\\w*$", "", strst))),lastword(gsub("\\s*\\w*$", "", strst)),lastword(strst))
                ng4<-subset(ng4,ngramsMinus1==pattern)
                
                
                nr<-nrow(ng4)
                if (nr>0) {
                        
                        ng3<-subset(ng3,ngrams==ng4$ngramsMinus1[1])
                        den<-ng3$freq[1]
                        d[1]<-ng4$ngramsLast[1]
                        d[2]<-ng4$freq[1]/ng3$freq[1]
                        
                }}
        return(d)      
}


nextWord3<-function(strst,ng3,ng2) {
        d<-c("",0)
        if (str_count(strst, "\\S+")>1) {
                pattern<-paste(lastword(gsub("\\s*\\w*$", "", strst)),lastword(strst))
                ng3<-subset(ng3,ngramsMinus1==pattern)
                nr<-nrow(ng3)
                if (nr>0) {
                        
                        ng2<-subset(ng2,ngrams==ng3$ngramsMinus1[1])
                        den<-ng2$freq[1]
                        d[1]<-ng3$ngramsLast[1]
                        d[2]<-ng3$freq[1]/ng2$freq[1]
                        
                }
        }
        return(d)      
}

nextWord2<-function(strst,ng2,ng1) {
        d<-c("",0)
        if (str_count(strst, "\\S+")>0) {
                pattern<-lastword(strst)
                ng2<-subset(ng2,ngramsMinus1==pattern)
                nr<-nrow(ng2)
                if (nr>0){
                        ng1<-subset(ng1,ngrams==ng2$ngramsMinus1[1])
                        den<-ng1$freq[1]
                        d[1]<-ng2$ngramsLast[1]
                        d[2]<-ng2$freq[1]/ng1$freq[1]
                }
        }
        return(d)      
}




nextWord<-function(strst,ng4,ng3,ng2,ng1){
        d<-c("",0)
        d<-nextWord4(strst,ng4,ng3)
        if (d[1]=="") d<-nextWord3(strst,ng3,ng2)
        if (d[1]=="") d<-nextWord2(strst,ng2,ng1)
        return(d)   
}


conNG1<-file("ng1.txt","r")
conNG2<-file("ng2.txt","r")
conNG3<-file("ng3.txt","r") 
conNG4<-file("ng4.txt","r")

ng1<-read.csv2(conNG1,dec=",",sep=";")
ng2<-read.csv2(conNG2,dec=",",sep=";")
ng3<-read.csv2(conNG3,dec=",",sep=";")
ng4<-read.csv2(conNG4,dec=",",sep=";")

close(conNG1)
close(conNG2)
close(conNG3)
close(conNG4)


ng1<-mutate(ng1,ngrams=as.character(ngrams),freq=as.numeric(freq))
ng2<-mutate(ng2,ngrams=as.character(ngrams),freq=as.numeric(freq),prop=as.numeric(prop),ngramsMinus1=as.character(ngramsMinus1),ngramsLast=as.character(ngramsLast))
ng3<-mutate(ng3,ngrams=as.character(ngrams),freq=as.numeric(freq),prop=as.numeric(prop),ngramsMinus1=as.character(ngramsMinus1),ngramsLast=as.character(ngramsLast))
ng4<-mutate(ng4,ngrams=as.character(ngrams),freq=as.numeric(freq),prop=as.numeric(prop),ngramsMinus1=as.character(ngramsMinus1),ngramsLast=as.character(ngramsLast))

shinyServer(function(input, output) {
        
        output$nxtword <- renderPrint({
        sentence1<-cleaningbis(input$sentence)
        nextWord(sentence1,ng4,ng3,ng2,ng1)[1]
        #lastword(sentence1)
                
        })
  
  
})
