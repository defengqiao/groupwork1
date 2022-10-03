#Defeng Qiao, s2419769; YiZhou Chen, s2450877; Tianai Ren, s2329207


#1.
rm(list = ls())

#2.
# dir.create("D:/Study/R_Code/groupwork1")   #create a target file
# download <- function(name){
#   url <- "https://www.gutenberg.org/ebooks/10"  #url
#   download.file(paste0(url,name), paste0("D:/Study/R_Code/groupwork1/", name), quite = TRUE)
# }
# download("pg10.txt")


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
tr2<-tr[sumT,]               #find all common words double which dont have NA
#create A
A <- array(0,c(length(com),length(com))) #initialize A
for (i in 1:dim(tr2)[1]) {
  A[tr2[i,1],tr2[i,2]]<-A[tr2[i,1],tr2[i,2]]+1
}

tr<-cbind(tr,comf)            #common words triplets

#prepare for T
sumr<-rowSums(tr)             #the sum of every row that have NA is NA
sumT<-!is.na(sumr)            #turn NA to FALSE, the rest is TRUE
tr3<-tr[sumT,]                 #find all common words triplets which dont have NA

#create T!!!!
T <- array(0,c(length(com),length(com),length(com))) #initialize T
for (i in 1:dim(tr3)[1]) {
  T[tr3[i,1],tr3[i,2],tr3[i,3]]<-T[tr3[i,1],tr3[i,2],tr3[i,3]]+1
}

#create S
S <- ordern[1:500]
#8.
#a function select word
te <- rep(0,50)
w <- rep(0,50)

write1 <- function(S){                           #S
  b1 <- sample(1:length(S), 1,  prob = S/sum(S)) #randomly pick a word from b, based on the probabilities in S
  return(b1)
}

write2 <- function(A1){                            #A[,w1]
  c1 <- sample(1:length(A1), 1, prob = A1/sum(A1)) ##randomly pick a word based on the probabilities in A
  return(c1)
}

write3 <- function(T12){                               #T12=T[,w1,w2]
  d1 <- sample(1:length(T12), 1, prob = T12/sum(T12)) ##randomly pick a word based on the probabilities in T
  return(d1)
}

#----------------------------------------------------------------
#10.
for (i in 1:length(a)) {                #先排除句首大写
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
      #cat(Ai,"\n")
    }
  }
}
AL<-0
Ai1<-vector()
for (Aii in Ai) {    #选出那些100%首字母大写的单词
  if(!(al[Aii]%in%a)){
    AL<-AL+1
    Ai1[AL]<-Aii
  }
}

for (Aii in Ai1) {
  if (al[Aii]%in%com){
    comAi<-match(al[Aii],com) #此单词在common words中的位置
    com[comAi]<-a[Aii]
  }
}

#--------------------------------------------------------------
#8.(continue)
for (i in 1:50){   #补注释
  
  if(i>2 & sum(T[,w[i-1],w[i-2]]) > 0){
    w[i] <- write3(T[,w[i-1],w[i-2]])
  }else if(i>1 & sum(A[,w[i-1]]) > 0){
    w[i] <- write2(A[,w[i-1]])
  }else{
    w[i] <- write1(S)
  }
  te[i] <- com[w[i]]
}

#首字母变大写的函数
wr<-function(w){
  w1<-strsplit(w,"")[[1]]
  w1[1]<-toupper(w1[1])
  w<-paste(w1,collapse = "")
  return(w)
}

#第一个单词首字母大写
if (!te[1]%in%c(",",".",";","!",":","?")){
  te[1]<-wr(te[1])
}

#将句首的单词都变为首字母大写
for (i in 1:49) {  
  if (te[i]=="."|te[i]=="!"|te[i]=="?"){
    te[i+1]<-wr(te[i+1])
  }
}


for (t in te){
  cat(t, "")
} 


#9.



