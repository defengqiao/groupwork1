#Defeng Qiao, s2419769; Tianai Ren, s2329207; YiZhou Chen, s2450877

#Defeng Qiao: 1,3,4,5,6,7,10, improve and fix bugs of 8,9
#Tianai Ren : 8, improve 4, some help in 10, add comments in detail
#YiZhou Chen: 2,9, some help in 4, add some comments

#Defeng Qiao: 50%; Tianai Ren: 30%; YiZhou Chen: 20% (YiZhou Chen got Covid-19 last weekend, so we shared part of his work)

#1.
rm(list = ls())

#2.download the file from the internet
# dir.create("D:/Study/R_Code/groupwork1")   #create a target file
# download <- function(name){
#   url <- "https://www.gutenberg.org/ebooks/10"  #url
#   download.file(paste0(url,name), paste0("D:/Study/R_Code/groupwork1/", name), quite = TRUE)
# }
# download("pg10.txt")


#3.
#Read the  file into R and pre-processing of data
setwd("D:/Study/R_Code/groupwork1")
setwd("C:/Users/Renti/Documents/Ren Tianai/postgraduate/sem 1/SP R")

a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers

#4.
#
#a<-c("aa,","bb.","cc?","dd!") #used for test
#split_punct is used to search for each word that contains a punctuation mark, separating it from the symbol and placing it appropriately.
split_punct <- function(a){ #Input is text a
  for (i in c(",",".",";","!",":","?")) {#"." and "?" are special characters in regular expressions which are treated separately
    
    if (i=="."){
      ii<-grep("\\.",a)                 #indices of "." in a; add "\\" in front of "?" and "." to prevent error
      if (length(ii)){                  #If the "." is not in the words，it will lead to errors
        a<-gsub("\\.","",a)             #remove "." in the words
        xs<-rep(0,length(ii)+length(a)) #vector to store the words and separated marks
        iis<-ii+1:length(ii)            #where should "." go in xs?
        xs[iis]<-"."                    #insert "."
        xs[-iis]<-a                     #insert the rest units
        a<-xs                           #give value back to a
      }
    } else if (i=="?") {                #similar to "."
      ii<-grep("\\?",a)
      if (length(ii)){ 
        a<-gsub("\\?","",a)       
        xs<-rep(0,length(ii)+length(a))
        iis<- ii+1:length(ii)
        xs[iis]<-"?"
        xs[-iis]<-a
        a<-xs
       }
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
  return(a)              # Output is a being separated
}
#5.
a <- split_punct(a)      #a contains the separate word and punctuation mark

#6.
#Find the 500 most commonly occurring words and put them into order
#replace the capital letters with lower case letter
al <- tolower(a)
#and find the vector of unique words
b<-unique(al)

#find the vector of indexes
index<-match(al,b)

#how many time each unique word occurs in the text
times <- tabulate(index)

#sort by number of times
ordern<-sort(times,decreasing = T)  #number of times sort by reverse order
orderi<-order(times,decreasing = T) #index

#common words
icom<-orderi[1:500]   #position of the 500 most common words in the vector of unique words
com<-b[icom]          #vector 'com' contains 500 most commonly occurring words

#7.
#first column is the index of common words
comi<-match(al,com)   #each element of the full text vector corresponds to the common words

#next column is the index for the following words
comn<-rep(NA,length(comi))                      #Initialize next words index vector
comn[2:length(comi)]<-comi[1:(length(comi)-1)]  #the vector shifted by one place, remove the last word of last column

#first column is the index of the final words
comf<-rep(NA,length(comi))                      #Initialize final words index vector
comf[3:length(comi)]<-comi[1:(length(comi)-2)]  #the vector shifted by one place

tr<-cbind(comi,comn)

#prepare for matrix A
sumr<-rowSums(tr)             #the sum of every row that have NA is NA
sumT<-!is.na(sumr)            #turn NA to FALSE, the rest is TRUE
tr2<-tr[sumT,]                #find all common words double which donot have NA

#create matrix A
A <- array(0,c(length(com),length(com)))  #initialize A
for (i in 1:dim(tr2)[1]) {
  A[tr2[i,1],tr2[i,2]]<-A[tr2[i,1],tr2[i,2]]+1
}

tr<-cbind(tr,comf)            #common words triplets

#prepare for array T
sumr<-rowSums(tr)             #the sum of every row that have NA is NA
sumT<-!is.na(sumr)            #turn NA to FALSE, the rest is TRUE
tr3<-tr[sumT,]                #find all common words triplets which dont have NA

#create T
T <- array(0,c(length(com),length(com),length(com)))  #initialize T
for (i in 1:dim(tr3)[1]) {
  T[tr3[i,1],tr3[i,2],tr3[i,3]]<-T[tr3[i,1],tr3[i,2],tr3[i,3]]+1
}

#create S
S <- ordern[1:500]

#8.
#Simulate 50 words sections from the model
te <- rep(0,50)  
w <- rep(0,50)

#write function is to pick word from A,S,T based on word and the probabilities
write1 <- function(S){                           #Input is vector S
  b1 <- sample(1:length(S), 1,  prob = S/sum(S)) #randomly pick a word from b, based on the probabilities in S
  return(b1)                                     #output is the index of words
}

write2 <- function(A1){                            #Input is A[,w1]
  c1 <- sample(1:length(A1), 1, prob = A1/sum(A1)) ##randomly pick a word based on the probabilities in A
  return(c1)
}

write3 <- function(T12){                               #Input is T12=T[,w1,w2]
  d1 <- sample(1:length(T12), 1, prob = T12/sum(T12)) ##randomly pick a word based on the probabilities in T
  return(d1)
}

#----------------------------------------------------------------
#10.
for (i in 1:length(a)) {                #Exclude the first capitalization of the sentence
  if (a[i]=="."|a[i]=="!"|a[i]=="?"){
    a[i+1]<-"0"
  }
  
}

Ai<-vector() #find location of the words of which first letter is capital letter
for (j in c("^A","^B","^C","^D","^E","^F","^G","^H","^I","^J","^K","^L","^M","^N","^O","^P","^Q","^R","^S","^T","^U","^V","^W","^X","^Y","^Z")) {
  Aij<-grep(j,a)
  if (length(Aij)){
    
    if (!length(Ai)){
      Ai<-Aij
    }else{
      Ai[(length(Ai)+1):(length(Ai)+length(Aij))]<-Aij
      #cat(Ai,"\n")     #use for detecting a bug
    }
  }
}
AL<-0
Ai1<-vector()
for (Aii in Ai) {    #find out those words in uppercase
  if(!(al[Aii]%in%a)){
    AL<-AL+1
    Ai1[AL]<-Aii
  }
}

for (Aii in Ai1) {
  if (al[Aii]%in%com){
    comAi<-match(al[Aii],com) #locate the word in common word
    com[comAi]<-a[Aii]
  }
}

#--------------------------------------------------------------
#8.(continue)
for (i in 1:50){  #Loop for 50 words
  
  if(i>2 & sum(T[,w[i-1],w[i-2]]) > 0){ # whether the pair of words will followed by a word, if not take write2 function
    w[i] <- write3(T[,w[i-1],w[i-2]])   # pick the third word if an appropriate phrase exists
  }else if(i>1 & sum(A[,w[i-1]]) > 0){  # whether a single word followed by another word, if not take write1 function
    w[i] <- write2(A[,w[i-1]])          # pick the second word
  }else{                               
    w[i] <- write1(S)                   # pick the first word
  }
  te[i] <- com[w[i]]                    # find the words in common vector from the index above
}

#a function for capitalization 
wr<-function(w){
  w1<-strsplit(w,"")[[1]]
  w1[1]<-toupper(w1[1])
  w<-paste(w1,collapse = "")
  return(w)
}

#Capitalize the first word 
if (!te[1]%in%c(",",".",";","!",":","?")){
  te[1]<-wr(te[1])
}

#Capitalize the first word of the sentence
for (i in 1:49) {  
  if (te[i]=="."|te[i]=="!"|te[i]=="?"){
    te[i+1]<-wr(te[i+1])
  }
}

#output
cat(te, "")


#9. 
#simulate 50 word sections of text simply taken from S
te<- sample(com, size = 50, replace = TRUE, prob = S/sum(S)) #randomly select 50 wrods(after you get a word and put it back for next time) 

for (i in 1:49){   
  if (te[i]=="."|te[i]=="!"|te[i]=="?"){  #Capitalized the first word of every sentence
    te[i+1]<-wr(te[i+1])
  }
}
if (!te[1]%in%c(",",".",";","!",":","?")){  #Capitalized the first word
  te[1]<-wr(te[1])
}

cat("\n------------------------------------------------\n")
cat(te,"")  #output

