library(ggplot2)
library(dplyr)
library(readxl)


##importData

WNBL1920 <- read_excel("WNBL1920.xlsx")
WNBL1819 <- read_excel("WNBL1920.xlsx")
WNBL1819tidy <- read_excel("WNBL1819tidy.xlsx", col_names = c("Location", "Score"))
WNBL1920tidy <- read_excel("WNBL1920tidy.xlsx")
HomeAdvantage <- read_excel("HomeAdvantage.xlsx")

##Scoring Average Home Vs Away 2019/20

WNBL1920$Home <- as.factor(WNBL1920$Home)
WNBL1920$Away <- as.factor(WNBL1920$Away)
home1920 <- cbind(WNBL1920[3], WNBL1920[6])
away1920 <- cbind(WNBL1920[4], WNBL1920[7])
home1920.summ <- aggregate(home1920, by = home1920[1], FUN = mean)
away1920.away <- aggregate(away1920, by = away1920[1], FUN = mean)

Home1920.grouped <-cbind(home1920.summ[1], home1920.summ[3])
Away1920.grouped <-cbind(away1920.away[1], away1920.away[3])

Summary1920 <- left_join(x = Home1920.grouped, y = Away1920.grouped, by = c("Home" = "Away"))
colnames(Summary1920)[1] <- "Team"

# Scatterplot by Team
ggplot(Summary1920, aes(x=HomeScore, y=AwayScore, colour = Team, label = Team )) + geom_point(size = 5) + 
  geom_abline(intercept = 0, slope = 1, colour = 'red', linetype = 'dashed', size = 1.5) + geom_label() +
  xlim(65,100) + ylim(65,90) + ggtitle("Season19/20")

##Scoring Average by team Home Vs Away 2019/20
HomeScores1819 <- WNBL1819 %>% select(Home, HomeScore)
AwayScores1819 <- WNBL1819 %>% select(Away, AwayScore)
a <- aggregate(HomeScores1819, by = HomeScores1819[1], FUN = mean)
b <- aggregate(AwayScores1819, by = AwayScores1819[1], FUN = mean)
Home1819.grouped <-cbind(a[1], a[3])
Away1819.grouped <-cbind(b[1],b[3])

Summary1819 <- left_join(x = Home1819.grouped, y = Away1819.grouped, by = c("Home" = "Away"))
colnames(Summary1819)[1] <- "Team"

##Scatterplot by team 
ggplot(Summary1819, aes(x=HomeScore, y=AwayScore, colour = Team, label = Team)) + geom_point(size = 5) + 
  geom_abline(intercept = 0, slope = 1, colour = 'red', linetype = 'dashed', size = 1.5) + geom_label() + 
  ggtitle("Season18/19") + xlim(65,100) + ylim(65,90)


##Boxplot of Leauge Scoring Average Home vs Away


ggplot(WNBL1819tidy, aes(x=Location, y=Score, color=Location)) +
  geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=20, size=4, col = 'black') + 
  scale_x_discrete(limits=c("Home", "Away")) +ggtitle("Season18/19") + theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) +ylim(40,115)

ggplot(WNBL1920tidy, aes(x=Location, y=Score, color=Location)) +
  geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=20, size=4, col = 'black') + 
  scale_x_discrete(limits=c("Home", "Away")) +ggtitle("Season 19/20") + theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) +ylim(40,115)

##Home Advantage as proposed by Hariss and Roebber

ggplot(HomeAdvantage, aes(x=HomeAdvantage$HomeAd)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="red") + geom_vline(xintercept = median(HomeAdvantage$HomeAd),linetype="dashed", color = "red", size=1.5) + 
  geom_text(x=-0.1, y=5, label="Median HCA = 0.127")+ xlab("Home Court Advantage") +ggtitle("Home Court Advantage 13/14-18/19") +
  ylab("Frequency")

## Home court by team
HistoryHome <- aggregate(HomeAdvantage, by =  list(HomeAdvantage$Team) , FUN =  mean)
ggplot(data=HistoryHome, aes(x=HistoryHome$Group.1, y=HistoryHome$HomeAd)) +
  geom_bar(stat="identity")+ xlab("Team") + ylab("Home Court Advantage") + 
  geom_hline(yintercept = mean(HistoryHome$HomeAd), linetype = 'dashed' , color = 'red', size = 1.5) + ggtitle("Home Court Advantage by Team")
