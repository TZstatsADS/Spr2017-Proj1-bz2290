# Project: 
### Code lib Folder

Following functions are included in "Functions.R":

1.Emotion_Barplot= function(pop)

This function takes in a the president speech(pop), and produce a emotion barplot. 

2.Factor_to_Numeric=function(df)

This function takes in a data frame and change its factor columns into numeric columns

3.return_short_sentence=function(df=emotion.matrix,Presidents,nwords=8)

This function takes in a default data frame "emotion.matrix", the president speech title as well as a defalut sentence length, and return all sentences in that speech whose length is up to "nwords"

4.return_long_sentence=function(df=emotion.matrix,Presidents,nwords=60)

This function takes in a default data frame "emotion.matrix", the president speech title as well as a defalut sentence length, and return all sentences in that speech whose length is greater than equal to "nwords"

5.Emotion_Sentences = function(df=emotion.matrix,pop)

This function takes in a default data frame emotion.matrix and and president speech title and return the most emotional sentences in this president's speech.

6.multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL)

This function will take in several variables that contain ggplot graph and plot these group by "cols" columns (default to be 1)

7.Quantile_plot = function(President)

This function takes a vector of two strings, in which it will calculate the quantile values for the number of words in sentences filterd by the strings given by user, and return a "ggplot" variable.