##Preprocessing the data##

folder.path="D:/Columbia University/Spring2017-Applied Data Science/Project_1_Bz2290/Spr2017-Proj1-bz2290/data/InauguralSpeeches"
#Replace to your own path 

speeches=list.files(path = folder.path, pattern = "*.txt")
prez.out=substr(speeches, 6, nchar(speeches)-4)
length.speeches=rep(NA, length(speeches))
ff.all<-Corpus(DirSource(folder.path))

#Speech selected
speech_selected=c("AbrahamLincoln-1","AbrahamLincoln-2","WoodrowWilson-1","WoodrowWilson-2","FranklinDroosevelt-2","FranklinDroosevelt-3","FranklinDroosevelt-4","JohnFKenndy-1","RonaldReagan-1","GeorgeWBush-2")

#Split speech into sentences
L1=sent_detect(ff.all[[1]]$content,endmarks = c("?", ".", "!", "|",";"))#LINCOLN-1
L2=sent_detect(ff.all[[2]]$content,endmarks = c("?", ".", "!", "|",";"))#LINCOLN-2
W1=sent_detect(ff.all[[56]]$content,endmarks = c("?", ".", "!", "|",";"))#WoodrowWilson-1 
W2=sent_detect(ff.all[[57]]$content,endmarks = c("?", ".", "!", "|",";"))#WoodrowWilson-2
F2=sent_detect(ff.all[[13]]$content,endmarks = c("?", ".", "!", "|",";"))#FranklinDroosevelt-2
F3=sent_detect(ff.all[[14]]$content,endmarks = c("?", ".", "!", "|",";"))#FranklinDroosevelt-3
F4=sent_detect(ff.all[[15]]$content,endmarks = c("?", ".", "!", "|",";"))#FranklinDroosevelt-4
J1=sent_detect(ff.all[[35]]$content,endmarks = c("?", ".", "!", "|",";"))#JohnFKenndy-1
R1=sent_detect(ff.all[[41]]$content,endmarks = c("?", ".", "!", "|",";"))#RonaldReagan-1
G2=sent_detect(ff.all[[21]]$content,endmarks= c("?", ".", "!", "|",";"))#GeorgeWBush-2

#Create a matrix of these presidents with their sentences
sentence.list = list(L1,L2,W1,W2,F2,F3,F4,J1,R1,G2)
title.list = c(rep("AbrahamLincoln-1",length(L1)),rep("AbrahamLincoln-2",length(L2)),rep("WoodrowWilson-1",length(W1)),rep("WoodrowWilson-2",length(W2)),rep("FranklinDroosevelt-2",length(F2)),rep("FranklinDroosevelt-3",length(F3)),rep("FranklinDroosevelt-4",length(F4)),rep("JohnFKenndy-1",length(J1)),rep("RonaldReagan-1",length(R1)),rep("GeorgeWBush-2",length(G2)))
emotion.matrix = cbind(title.list,c(L1,L2,W1,W2,F2,F3,F4,J1,R1,G2))
colnames(emotion.matrix)=c("President","Sentences")

#Calculate the generate the emotion matrix
interm.matrix = NULL
for(i in 1 : length(sentence.list))
{
  emotions=diag(1/(word_count(sentence.list[[i]])+0.01))%*%as.matrix(get_nrc_sentiment(sentence.list[[i]]))
  colnames(emotions)=c( "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive")
  interm.matrix = rbind(interm.matrix,cbind(sent.id=as.numeric(1:length(sentence.list[[i]])),nword=word_count(sentence.list[[i]]),as.matrix(emotions)))
}
emotion.matrix = as.data.frame(cbind(emotion.matrix,interm.matrix))
emotion.matrix = Factor_to_Numeric(emotion.matrix)

#Prepare data for the wordcloud

#Remove white space
ff.all<-tm_map(ff.all, stripWhitespace)

#Transform to lower case letters
ff.all<-tm_map(ff.all, content_transformer(tolower))

#Remove Common stopwords
ff.all<-tm_map(ff.all, removeWords, stopwords("english"))

#Remove empty characters
ff.all<-tm_map(ff.all, removeWords, character(0))

#Remove punctions
ff.all<-tm_map(ff.all, removePunctuation)

#Remove numbers
ff.all = tm_map(ff.all,removeNumbers)

#Turn into documentTermMatrix
dtm <- DocumentTermMatrix(ff.all,
                          control = list(weighting = function(x)
                            weightTfIdf(x, 
                                        normalize =FALSE),
                            stopwords = TRUE))
ff.dtm=tidy(dtm)