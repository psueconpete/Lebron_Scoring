#Nba Scoring
library(dplyr)
library(ggplot2)
library(ggthemes)

all_time_scoring$season <- all_time_scoring$`Season Number`

#creating the variable
scoring <- all_time_scoring %>%
  group_by(Player) %>% 
  mutate(Total = cumsum(PTS), Total_RBS = cumsum(TRB), Total_AST = cumsum(AST), TD_pts = PTS + AST + TRB) %>%
  mutate(Total_TD = cumsum(TD_pts))


#by season
ggplot(data=scoring) +
  geom_line(mapping = aes(x=Age, y=Total, color= Player, linetype=Player), size=1.5) +  
  geom_point(mapping = aes(x=season, y=Total, color= Player)) +
  xlim(30,40) +
  ylim(15000, 40000) +
  theme_fivethirtyeight() +
  ggtitle("Cumulative Career Points By Season")

#by season
ggplot(data=scoring) +
  geom_line(data=filter(scoring, Player == "Lebron James" | Player == "Kobe Bryant" | Player == "Karl Malone" | Player == "Michael Jordan" | Player == "Kareem Abdul-Jabbar"), mapping = aes(x=season, y=Total, color= Player, linetype=Player), size=1.5) +  
  xlim(10,20) +
  ylim(15000, 40000) +
              theme_fivethirtyeight() +
  ggtitle("Cumulative Career Points By Season")

#by age 
ggplot(data=scoring) +
  geom_line(data=filter(scoring, Player == "Lebron James" | Player == "Kobe Bryant" | Player == "Karl Malone" | Player == "Michael Jordan" | Player == "Kareem Abdul-Jabbar"), mapping = aes(x=Age, y=Total, color= Player, linetype=Player), size=1.5) +
  geom_vline(xintercept =34) +
  xlim(30,42) +
  ylim(15000, 40000) +
  theme_fivethirtyeight() +
  ggtitle("Cumulative Points by Age")
  
#by season rebounds
ggplot(data=scoring) +
  geom_line(data=filter(scoring, Player == "Lebron James" | Player == "Kobe Bryant" | Player == "Karl Malone" | Player == "Michael Jordan" | Player == "Kareem Abdul-Jabbar"), mapping = aes(x=season, y=Total_RBS, color= Player, linetype=Player), size=1.5) +  
  xlim(10,22) +
  theme_fivethirtyeight() +
  ggtitle("Cumulative Rebounds by Season")

#by season rebounds
ggplot(data=scoring) +
  geom_line(mapping = aes(x=season, y=Total_RBS, color= Player, linetype=Player), size=1.5) +  
  xlim(10,20) +
  theme_fivethirtyeight()

#by season assists
ggplot(data=scoring) +
  geom_line(data=filter(scoring, Player == "Lebron James" | Player == "Kobe Bryant" | Player == "Karl Malone" | Player == "Michael Jordan" | Player == "Kareem Abdul-Jabbar"), mapping = aes(x=season, y=Total_AST, color= Player, linetype=Player), size=1.5) +  
  xlim(10,20) +
  theme_fivethirtyeight() +
  ggtitle("Cumulative Assists by Season")

#by season assists
ggplot(data=scoring) +
  geom_line(mapping = aes(x=season, y=Total_AST, color= Player, linetype=Player), size=1.5) +  
  xlim(10,20) +
  theme_fivethirtyeight()

#by season TD Score
ggplot(data=scoring) +
  geom_line(data=filter(scoring, Player == "Lebron James" | Player == "Kobe Bryant" | Player == "Karl Malone" | Player == "Michael Jordan" | Player == "Kareem Abdul-Jabbar"), mapping = aes(x=season, y=Total_TD, color= Player, linetype=Player), size=1.5) +  
  xlim(12,22) + 
  ylim(20000, 65000) +
  theme_fivethirtyeight() +
  ggtitle("Cumulative TD Points by Season")

##scoring with lebron bolded and his projection
#by season
ggplot(data=scoring) +
  geom_line(data=filter(scoring, Player == "Kobe Bryant" | Player == "Karl Malone" | Player == "Michael Jordan" | Player == "Kareem Abdul-Jabbar" | Player == "Lebron James"), mapping = aes(x=season, y=Total, color= Player), size=1, alpha=.5) +  
  geom_line(data=filter(scoring, Player == "Lebron James"), mapping=aes(x=season, y=Total), color="deepskyblue", size=1.1, alpha=1) +
  labs(title = "Cumulative Points by Season", subtitle = "Data from Basketball Reference") +
  xlab(label = "Season Number of Career") +
  ylab(label = "Cumulative Points") +
  xlim(14,20) +
  ylim(25000, 40000) +
  theme_fivethirtyeight() +
  geom_abline(intercept = 156, slope = 2075, color = "deepskyblue", linetype = "dashed", size = 1.1) +
  geom_hline(yintercept = 38387)

#Lebron Modeling
LJ <- scoring %>%
  filter(Player == "Lebron James") 
line <- lm(Total ~ season, data = LJ)
summary(line)

#Adjusting for Aging
line2 <- lm(Total ~ 0 + season, data = scoring)
line2
Aging <- scoring %>%
  filter(Age > 36)
line3 <- lm(Total ~ 0 + season, data = Aging)
line3

##scoring with lebron bolded and his projection adjusted for aging
#by season
ggplot(data=scoring) +
  geom_line(data=filter(scoring, Player == "Kobe Bryant" | Player == "Karl Malone" | Player == "Michael Jordan" | Player == "Kareem Abdul-Jabbar" | Player == "Lebron James"), mapping = aes(x=season, y=Total, color= Player), size=1, alpha=.5) +  
  geom_line(data=filter(scoring, Player == "Lebron James"), mapping=aes(x=season, y=Total), color="deepskyblue", size=1.1, alpha=1) +
  labs(title = "Cumulative Points by Season", subtitle = "Data from Basketball Reference") +
  xlab(label = "Season Number of Career") +
  ylab(label = "Cumulative Points") +
  xlim(14,20) +
  ylim(25000, 40000) +
  theme_fivethirtyeight() +
  geom_hline(yintercept = 38387) +
  annotate("text", x=16, y= 39000, label = "Kareem's All Time Scoring Line") 

#FG%
scoring %>%
  group_by(Player) %>%
  summarise(average=mean(`FG%`, na.rm = TRUE)) %>%
  ggplot() +
  geom_point(mapping=aes(x=Player, y= average, color=Player), size=10) +
  theme_economist() +
  xlab(label = "Player Name") +
  ylab(label = "FG%") +
  ggtitle(label = "Field Goal Percentage by Player", subtitle = "Data from Basketball Reference") 

#adding the triple doubles with lebrons trajection of 21 seasons
scoring %>%
  filter(season < 22) %>%
  group_by(Player) %>%
  summarise(tripdub = sum(TD_pts)) %>% 
  arrange(desc(tripdub)) %>%
  View()
  