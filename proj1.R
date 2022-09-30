#Defeng Qiao, s2419769; YiZhou Chen, s2450877;Tianai Ren,


#1.
rm(list = ls())

#2.


#3.
setwd("D:/Study/R_Code/groupwork1")
a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers

#4.
#a<-c("aa,","bb.","cc?","dd!") #used for test
split_punct <- function(a){
  for (i in c(",",".",";","!",":","?")) {
    
    if (i=="."){
      ii<-grep("\\.",a)               #indices of "." in a; add "\\" in front of "?" and "." to prevent error
      if (length(ii)){                  #If the "." is not in the words，it will lead to errors
        a<-gsub("\\.","",a)           #remove "." in the words
        xs<-rep(0,length(ii)+length(a)) #vector to store the words and separated marks
        iis<-ii+1:length(ii)           #where should "." go in xs?
        xs[iis]<-"."                    #insert "."
        xs[-iis]<-a                     #insert the rest units
        a<-xs                           #give value back to a
      }
    } else if (i=="?") {               #similar to "."
      ii<-grep("\\?",a)         
      a<-gsub("\\?","",a)       
      xs<-rep(0,length(ii)+length(a))
      iis<- ii+1:length(ii)
      xs[iis]<-"?"
      xs[-iis]<-a
      a<-xs
    }else{                             #rest of the punctuation marks
      ii<-grep(i,a)         
      if (length(ii)){
        a<-gsub(i,"",a)       
        xs<-rep(0,length(ii)+length(a))
        iis<-ii+1:length(ii)
        xs[iis]<-i
        xs[-iis]<-a
        a<-xs
      }
    }
  }
  return(a)            #return the separated a
}
#5.
a <- split_punct(a)      #separate the punctuation marks

#6.
#replace the capital letters with lower case letter
al <- tolower(a)
#and find the vector of unique words
b<-unique(al)

#find the vector of indicies
index<-match(al,b)

#how many time each unique word occurs in the text
times <- tabulate(index)

#sort by number of times
ordern<-sort(times,decreasing = T)  #number of times
orderi<-order(times,decreasing = T) #index

#common words
icom<-orderi[1:500]#前500单词在b中的位置
com<-b[icom]#按次数排列的前500单词

#7.
comi<-match(al,com)#each element of the full text vector corresponds to the common words

comn<-rep(NA,length(comi))    #next common
comn[2:length(comi)]<-comi[1:(length(comi)-1)]

comf<-rep(NA,length(comi))    #final common
comf[3:length(comi)]<-comi[1:(length(comi)-2)]

tr<-cbind(comi,comn)

#prepare for A
sumr<-rowSums(tr)             #the sum of every row that have NA is NA
sumT<-!is.na(sumr)            #turn NA to FALSE, the rest is TRUE
sumi<-c(1:length(sumr))       #index of sum
sumis<-sumi[sumT]             #find where is the value TURE
tr2<-tr[sumis,]               #find all common words double which dont have NA

#create A
A <- array(0,c(length(com),length(com))) #initialize A
for (i in 1:dim(tr2)[1]) {
  A[tr2[i,1],tr2[i,2]]<-A[tr2[i,1],tr2[i,2]]+1
}

tr<-cbind(tr,comf)            #common words triplets

#prepare for B
sumr<-rowSums(tr)             #the sum of every row that have NA is NA
sumT<-!is.na(sumr)            #turn NA to FALSE, the rest is TRUE
sumi<-c(1:length(sumr))       #index of sum
sumis<-sumi[sumT]             #find where is the value TURE
tr3<-tr[sumis,]                 #find all common words triplets which dont have NA

#create T!!!!
T <- array(0,c(length(com),length(com),length(com))) #initialize T
for (i in 1:dim(tr3)[1]) {
  T[tr3[i,1],tr3[i,2],tr3[i,3]]<-T[tr3[i,1],tr3[i,2],tr3[i,3]]+1
}

#create S
S <- ordern[1:500]
#8.


#9.
