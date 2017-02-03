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

#Plot Multiple Plots of "ggplot"
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
    
  if (is.null(layout)) {
    
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
   }
    if (numPlots==1) {
    
      print(plots[[1]])
   } 
    else {
    
      grid.newpage()
      
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
      for (i in 1:numPlots) {
      
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#QQ plot for presidential Speeches
Quantile_plot = function(President)
{
  quantile.theoretical = c()
  
  quantile.actual = NULL
  
  quantile.theoretical = quantile(nword.all[nword.all$President == President[1],3], probs = seq(0,1,0.01))
  
  quantile.actual = quantile(nword.all[nword.all$President == President[2],3], probs = seq(0,1,0.01))
  
  plot.data = data.frame(actual = quantile.actual,theoretical = quantile.theoretical)
  
  p = ggplot(plot.data,aes(x=theoretical,y=actual))+
    geom_point()+
    labs(x=President[1],y=President[2],title=paste("QQplot for",substr(President[1],start=1,stop=nchar(President[1])-2)))
  
  return(p)
}