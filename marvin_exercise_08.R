#Exercise 8 -- Tess Marvin
#Problem 1: Line Graph Depicting the cumulative score for each team as function of time
scores <- read.table("UWvMSU_1-22-13.txt", header = TRUE, sep = "\t")
UWscore<- c(0);
MSUscore <- c(0);
for(i in 1:nrow(scores))
{
  if(scores[i,]$team == "UW")
  {
    cumulativescore <- scores[i,]$score + UWscore[i]
    UWscore <- c(UWscore, cumulativescore)
    MSUscore<- c(MSUscore, MSUscore[i])
  }
  else
  {
    cumulativescore <- scores[i,]$score + MSUscore[i]
    MSUscore <- c(MSUscore, cumulativescore)
    UWscore <- c(UWscore, UWscore[i])
  }
}
#An example that uses the base package 
#Add a zero time point so that the scores for both teams begins at 0 at the start of game 
timing <- c(0);
for(i in 1:nrow(scores))
{
  timing <- c(timing, scores[i,]$time)
}
#plot using plot function -- UW is in black and MSU is in blue 
plot(x=timing, y=UWscore, type = 'l', xlab = "game clock", ylab= "Cummulative Score per Team")
lines(x=timing, y=MSUscore, type ='l', col = "blue")
# a better plot that looks more like his example using ggplot 
scoredata <- data.frame(timing, UWscore, MSUscore)
library("ggplot2")
a = ggplot() +
  geom_line(data = scoredata, aes(x=scoredata$timing, y=scoredata$UWscore), color = "blue") +
  geom_line(data = scoredata, aes(x=scoredata$timing, y=scoredata$MSUscore), color = "red") + 
  xlab("game clock") +
  ylab("Cummulative Score per Team") +
  theme_classic()
#########################################################################################################
#Problem 2: Guess my number game 
#usage: guessmynumber() then follow prompts 
guessmynumber <- function()
{
  num <- sample(1:100,1)
  guess <- readline(prompt = "I'm thinking of a number 1-100. Guess: ")
  for(i in 1:9)
  {
      if(guess>num)
      {
        guess <- readline(prompt = "Lower. Guess: ")
      }
      else if(guess<num)
      {
        guess <- readline(prompt = "Higher. Guess: ")
      }
    else
    {
      return("Correct!")
    }
  }
  return(paste("The number was ",as.character(num)))
}

