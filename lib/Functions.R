#Plot Emotion Barplot
Emotion_Barplot= function(pop)
{
  par(mar=c(4, 6, 2, 1))
  emo.means=colMeans(select(subset(emotion.matrix,emotion.matrix$President==pop), anger:trust)>0.01,na.rm=TRUE)
  col.use=c("red2", "darkgoldenrod1", 
            "chartreuse3", "blueviolet",
            "darkgoldenrod2", "dodgerblue3", 
            "darkgoldenrod1", "darkgoldenrod1")
  barplot(emo.means[order(emo.means)], las=2, col=col.use[order(emo.means)], horiz=T, main=pop)
}

#Convert Factor to a Numeric variable
Factor_to_Numeric=function(df)
{
  for(i in 3:14)
  {
    df[,i] = as.numeric(as.character(df[,i]))
  }
  return(df)
}

#Return Short Sentences
return_short_sentence=function(df=emotion.matrix,Presidents,nwords=8)
{
  return(df$Sentences[which(df$President==Presidents & df$nword <= nwords)])
}

#Return Long Sentences
return_long_sentence=function(df=emotion.matrix,Presidents,nwords=60)
{
  return(df$Sentences[which(df$President==Presidents & df$nword >= nwords)])
}

#Return Emotional Sentences
Emotion_Sentences = function(df=emotion.matrix,pop)
{
  df=tbl_df(emotion.matrix)%>%
    filter(President==pop,nword>=4)%>%
    select(Sentences, anger:trust)
  df=as.data.frame(df)
  index=apply(df[,-1], 2, which.max)
  result = NULL
  for(i in 1:length(index))
  {
    result = rbind(result,as.character(df$Sentences[index[i]]))
  }
  result = cbind(c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust"),result)
  return(result)
}