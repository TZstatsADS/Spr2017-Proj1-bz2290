##Preprocessing the data##

#Please Change it to your own path when running
##########################################################################################################################################################
folder.path="D:/Columbia University/Spring2017-Applied Data Science/Project_1_Bz2290/Spr2017-Proj1-bz2290/data/InauguralSpeeches"#Replace to your own path
##########################################################################################################################################################


speeches=list.files(path = folder.path, pattern = "*.txt")

prez.out=substr(speeches, 6, nchar(speeches)-4)

length.speeches=rep(NA, length(speeches))

ff.all<-Corpus(DirSource(folder.path))


#Speech selected
speech.selected.war=c("AbrahamLincoln-1","AbrahamLincoln-2","WoodrowWilson-1","WoodrowWilson-2","FranklinDRoosevelt-2","FranklinDRoosevelt-3","FranklinDRoosevelt-4","JohnFKennedy-1","RonaldReagan-1","GeorgeWBush-2")

speech.selected.peace=c("WilliamJClinton-2","RonaldReagan-2","JimmyCarter-1","GeorgeWBush-1")

#Split speech into sentences

#For War seeches
L1=sent_detect(ff.all[[1]]$content,endmarks = c("?", ".", "!", "|",";"))#LINCOLN-1

L2=sent_detect(ff.all[[2]]$content,endmarks = c("?", ".", "!", "|",";"))#LINCOLN-2

W1=sent_detect(ff.all[[56]]$content,endmarks = c("?", ".", "!", "|",";"))#WoodrowWilson-1 

W2=sent_detect(ff.all[[57]]$content,endmarks = c("?", ".", "!", "|",";"))#WoodrowWilson-2

F2=sent_detect(ff.all[[13]]$content,endmarks = c("?", ".", "!", "|",";"))#FranklinDRoosevelt-2

F3=sent_detect(ff.all[[14]]$content,endmarks = c("?", ".", "!", "|",";"))#FranklinDRoosevelt-3

F4=sent_detect(ff.all[[15]]$content,endmarks = c("?", ".", "!", "|",";"))#FranklinDRoosevelt-4

J1=sent_detect(ff.all[[35]]$content,endmarks = c("?", ".", "!", "|",";"))#JohnFKenndy-1

R1=sent_detect(ff.all[[41]]$content,endmarks = c("?", ".", "!", "|",";"))#RonaldReagan-1

G2=sent_detect(ff.all[[21]]$content,endmarks= c("?", ".", "!", "|",";"))#GeorgeWBush-2

#For peace speech
C2=sent_detect(ff.all[[53]]$content,endmarks = c("?", ".", "!", "|",";"))#WilliamJClinton-2

R2=sent_detect(ff.all[[42]]$content,endmarks = c("?", ".", "!", "|",";"))#RonaldReagan-2

JC1=sent_detect(ff.all[[33]]$content,endmarks = c("?", ".", "!", "|",";"))#JimmyCarter-1

G1=sent_detect(ff.all[[20]]$content,endmarks= c("?", ".", "!", "|",";"))#GeorgeWBush-1


#Create a matrix of these presidents with their sentences
sentence.list = list(L1,L2,W1,W2,F2,F3,F4,J1,R1,G2,C2,R2,JC1,G1)

title.list = c(rep("AbrahamLincoln-1",length(L1)),rep("AbrahamLincoln-2",length(L2)),rep("WoodrowWilson-1",length(W1)),rep("WoodrowWilson-2",length(W2)),rep("FranklinDRoosevelt-2",length(F2)),rep("FranklinDRoosevelt-3",length(F3)),rep("FranklinDRoosevelt-4",length(F4)),rep("JohnFKennedy-1",length(J1)),rep("RonaldReagan-1",length(R1)),rep("GeorgeWBush-2",length(G2)),rep("WilliamJClinton-2",length(C2)),rep("RonaldReagan-2",length(R2)),rep("JimmyCarter-1",length(JC1)),rep("GeorgeWBush-1",length(G1)))

emotion.matrix = cbind(title.list,c(L1,L2,W1,W2,F2,F3,F4,J1,R1,G2,C2,R2,JC1,G1))

colnames(emotion.matrix)=c("President","Sentences")


#Calculate and generate the emotion matrix
interm.matrix = NULL

for(i in 1 : length(sentence.list))
{
  emotions=diag(1/(word_count(sentence.list[[i]])+0.01))%*%as.matrix(get_nrc_sentiment(sentence.list[[i]]))
  
  colnames(emotions)=c( "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive")
  
  interm.matrix = rbind(interm.matrix,cbind(sent.id=as.numeric(1:length(sentence.list[[i]])),nword=word_count(sentence.list[[i]]),as.matrix(emotions)))
}

emotion.matrix = as.data.frame(cbind(emotion.matrix,interm.matrix))

emotion.matrix = Factor_to_Numeric(emotion.matrix)

#Prepare Data for QQplot
nword.all = emotion.matrix[emotion.matrix$President %in% c(speech.selected.war,speech.selected.peace),c(1,3,4)]

nword.all = nword.all[!is.na(nword.all$nword),]

#Summary Data set for Cluster analysis

#For war group
presid.summary.war=tbl_df(emotion.matrix)%>%
  
  filter(President%in%c(speech.selected.war))%>%
  
  group_by(President)%>%
  
  summarise(
    
    anger=mean(anger,na.rm=TRUE),
    
    anticipation=mean(anticipation,na.rm=TRUE),
    
    disgust=mean(disgust,na.rm=TRUE),
    
    fear=mean(fear,na.rm=TRUE),
    
    joy=mean(joy,na.rm=TRUE),
    
    sadness=mean(sadness,na.rm=TRUE),
    
    surprise=mean(surprise,na.rm=TRUE),
    
    trust=mean(trust,na.rm=TRUE)
    
    #negative=mean(negative),
    
    #positive=mean(positive)
  )

presid.summary.war=as.data.frame(presid.summary.war)

rownames(presid.summary.war)=as.character((presid.summary.war[,1]))


#For peace group
presid.summary.peace=tbl_df(emotion.matrix)%>%
  
  filter(President%in%c(speech.selected.peace))%>%
  
  group_by(President)%>%
  
  summarise(
  
      anger=mean(anger,na.rm=TRUE),
    
      anticipation=mean(anticipation,na.rm=TRUE),
    
      disgust=mean(disgust,na.rm=TRUE),
    
      fear=mean(fear,na.rm=TRUE),
    
      joy=mean(joy,na.rm=TRUE),
    
      sadness=mean(sadness,na.rm=TRUE),
    
      surprise=mean(surprise,na.rm=TRUE),
    
      trust=mean(trust,na.rm=TRUE)
      )

presid.summary.peace=as.data.frame(presid.summary.peace)

rownames(presid.summary.peace)=as.character((presid.summary.peace[,1]))


#For both group
presid.summary.all=tbl_df(emotion.matrix)%>%
  
  filter(President%in%c(speech.selected.war,speech.selected.peace))%>%
  
  group_by(President)%>%
  
  summarise(
    
    anger=mean(anger,na.rm=TRUE),
    
    anticipation=mean(anticipation,na.rm=TRUE),
    
    disgust=mean(disgust,na.rm=TRUE),
    
    fear=mean(fear,na.rm=TRUE),
    
    joy=mean(joy,na.rm=TRUE),
    
    sadness=mean(sadness,na.rm=TRUE),
    
    surprise=mean(surprise,na.rm=TRUE),
    
    trust=mean(trust,na.rm=TRUE)
  
    )

presid.summary.all=as.data.frame(presid.summary.all)

rownames(presid.summary.all)=as.character((presid.summary.all[,1]))



