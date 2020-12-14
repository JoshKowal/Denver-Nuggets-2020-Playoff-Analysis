#Clear Workspace
dev.off()
cat("\014")
rm(list=ls())
set.seed(18552)

#TODO: Change the working directory to the directory in which you cloned the repository.
setwd("/Users/joshuakowal/Desktop/DSCI 304 R Code Files and Data Files/Final Project")

library(bnlearn)
library(gplots)
library(devtools)
library(broom)
library(lsmeans)
library(psych)
library(margins)
library(ggeffects)
library(ggplot2)
library(stargazer)
library(cowplot)
library(ggpubr)
library(tidyverse)
library(ggimage)
library(magick)
library(gganimate)
library(png)
library(gapminder)
library(gifski)
library(reshape2)
library(collapsibleTree)
library(htmlwidgets)
library(plotly)
library(crosstalk)
library(av)
library(interactions)
library(grid)

#Let's first determine what some key characteristics of a star player are.
#I used the following link to help me: https://a16z.com/2020/11/17/nba-startup-metrics-efficiency-usage/
#To increase the number of stars, I changed the cutoff threshold to a minimum offensive rating of 107 and a minimum percent usage of 25%.

season_advanced_avgs = read_csv('Star_Player_Statistics_Season.csv')
season_advanced_avgs$Star<-0
season_advanced_avgs$Star[season_advanced_avgs$ORtg>= 107 & season_advanced_avgs$`%Usage`>=25]<-1
season_advanced_avgs$Star<-as.factor(season_advanced_avgs$Star)

ggplot(data = season_advanced_avgs, aes(x = `%Usage`, y = ORtg, color = Star))+
  geom_point(size = 1)+
  annotate('rect', xmin = 25, xmax = 45, ymin = 107, ymax = 200, alpha = 0.3)+
  annotate('text', x = 35, y = 175, label = 'Star Quadrant', colour = 'black')+
  annotate('segment', x = 25, xend = 25, y = 0, yend = 200, colour = 'black')+
  annotate('segment', x = 0, xend = 45, y = 107, yend = 107, colour = 'black')+
  labs(x = 'Percent of Plays Used', y = 'Offensive Efficiency', title = 'Relationship Between Offensive Efficiency and Percent Usage', subtitle = '2019-2020 Regular Season')+
  scale_color_manual(values = c('red', 'blue'), labels = c('Non-star', 'Star'), name = 'Stardom Status')

stars<-subset(season_advanced_avgs, season_advanced_avgs$Star == 1 & season_advanced_avgs$ORtg <= 130)

#Let's look at the star players' offensive ratings and percent usage
ggplot(data = stars, aes(x = reorder(Player, ORtg), y = ORtg))+
  geom_bar(stat = 'identity', aes(fill = 'red'))+
  coord_flip()+
  labs(x = 'Player', y = 'Offensive Rating', title = 'Each Star Player\'s Cumulative Offensive Rating', subtitle = '2019-2020 Regular Season')+
  theme(legend.position = 'none')

ggplot(data = stars, aes(x = reorder(Player, `%Usage`), y = `%Usage`))+
  geom_bar(stat = 'identity', aes(fill = 'red'))+
  coord_flip()+
  labs(x = 'Player', y = 'Percent Usage', title = 'Each Star Player\'s Cumulative Percent Usage in Plays\nWhile They\'re Playing', subtitle = '2019-2020 Regular Season')+
  theme(legend.position = 'none')

#Let's also see how many stars are on each team
ggplot(data = stars, aes(x = Tm))+
  geom_histogram(stat = 'count', fill = 'blue')+
  coord_flip()+
  labs(x = 'Team', y = 'Number of Stars', title = 'Number of Stars on Each Team', subtitle = 'End of 2019-2020 Regular Season')

#Let's see the basic statistics for each player and predict what stats can predict stardom.
season_basic_avgs<-read_csv('nba_player_statistics_2019_2020_reg_season.csv')
season_basic_avgs$Star<-season_advanced_avgs$Star
season_basic_avgs$ORtg<-season_advanced_avgs$ORtg
season_basic_avgs$DRtg<-season_advanced_avgs$DRtg
season_basic_avgs$`%Usage`<-season_advanced_avgs$`%Usage`

##################################################################
#Round 1 Series

r1_overall<-read_csv('Denver_Utah_Round1.csv')
r1_half<-read_csv('Denver_Utah_Round1_By_Half.csv')
r1_quarter<-read_csv('Denver_Utah_Round1_By_Quarter.csv')

r1_overall$Star<-ifelse(r1_overall$Player %in% stars$Player, 1, 0)
r1_overall$Star<-as.factor(r1_overall$Star)
r1_half$Star<-ifelse(r1_half$Player %in% stars$Player, 1, 0)
r1_half$Star<-as.factor(r1_half$Star)
r1_quarter$Star<-ifelse(r1_quarter$Player %in% stars$Player, 1, 0)
r1_quarter$Star<-as.factor(r1_quarter$Star)

den_subs<-c('Denver Won this Game', 'Denver Lost this Game', 'Denver Lost this Game', 'Denver Lost this Game', 'Denver Won this Game', 'Denver Won this Game', 'Denver Won this Game')
uta_subs<-c('Utah Lost this Game', 'Utah Won this Game', 'Utah Won this Game', 'Utah Won this Game', 'Utah Lost this Game', 'Utah Lost this Game', 'Utah Lost this Game')

for (game in c(1:7)) {
  sub_overall<-subset(r1_overall, r1_overall$Game == game)
  sub_half<-subset(r1_half, r1_half$Game == game)
  sub_quarter<-subset(r1_quarter, r1_quarter$Game == game)
  
  sub_quarter0<-subset(sub_quarter, sub_quarter$Quarter == 1)
  sub_quarter0$Quarter<-0
  sub_quarter0$PTS<-0
  sub_quarter0$TRB<-0
  sub_quarter1<-subset(sub_quarter, sub_quarter$Quarter == 1)
  sub_quarter2<-subset(sub_quarter, sub_quarter$Quarter == 2)
  sub_quarter2$PTS<-sub_quarter2$PTS + sub_quarter1$PTS
  sub_quarter2$TRB<-sub_quarter2$TRB + sub_quarter1$TRB
  sub_quarter3<-subset(sub_quarter, sub_quarter$Quarter == 3)
  sub_quarter3$PTS<-sub_quarter3$PTS + sub_quarter2$PTS
  sub_quarter3$TRB<-sub_quarter3$TRB + sub_quarter2$TRB
  sub_quarter4<-subset(sub_quarter, sub_quarter$Quarter == 4)
  sub_quarter4$PTS<-sub_quarter4$PTS + sub_quarter3$PTS
  sub_quarter4$TRB<-sub_quarter4$TRB + sub_quarter3$TRB
  sub_quarter_mod<-rbind(sub_quarter0, sub_quarter1, sub_quarter2, sub_quarter3, sub_quarter4)
  sub_quarter_mod$Quarter<-factor(
    sub_quarter_mod$Quarter, levels = c(0,1,2,3,4),
    labels = c('at the Start', 'after the 1st Quarter', 'after the 2nd Quarter', 'after the 3rd Quarter', 'after the 4th Quarter')
  )
  den_quarter_mod<-subset(sub_quarter_mod, sub_quarter_mod$Team == 'DEN')
  uta_quarter_mod<-subset(sub_quarter_mod, sub_quarter_mod$Team == 'UTA')
  
  r1dg1p<-ggplot(data = den_quarter_mod, aes(x = Player, y = PTS, fill = Star))+
    geom_bar(position = 'dodge', stat = 'identity')+
    labs(x = 'Player', y = 'Points', title = paste('Denver Player Points {closest_state} for Game', toString(game), sep = ' '), subtitle = den_subs[game])+
    ylim(0,60)+
    scale_fill_manual(values = c('red', 'blue'), labels = c('Non-star', 'Star'), name = 'Stardom Status')+
    coord_flip()
  
  r1ug1p<-ggplot(data = uta_quarter_mod, aes(x = Player, y = PTS, fill = Star))+
    geom_bar(position = 'dodge', stat = 'identity')+
    labs(x = 'Player', y = 'Points', title = paste('Utah Player Points {closest_state} for Game', toString(game), sep = ' '), subtitle = uta_subs[game])+
    ylim(0,60)+
    scale_fill_manual(values = c('red', 'blue'), labels = c('Non-star', 'Star'), name = 'Stardom Status')+
    coord_flip()
  
  r1dg1p_anim<-r1dg1p+transition_states(Quarter)
  animate(r1dg1p_anim, renderer = av_renderer(paste('r1dg',toString(game),'p.mp4', sep = '')))
  
  r1ug1p_anim<-r1ug1p+transition_states(Quarter)
  animate(r1ug1p_anim, renderer = av_renderer(paste('r1ug',toString(game),'p.mp4', sep = '')))
  
  r1dg1r<-ggplot(data = den_quarter_mod, aes(x = Player, y = TRB, fill = Star))+
    geom_bar(position = 'dodge', stat = 'identity')+
    labs(x = 'Player', y = 'Rebounds', title = paste('Denver Player Rebounds {closest_state} for Game', toString(game), sep = ' '), subtitle = den_subs[game])+
    ylim(0,25)+
    scale_fill_manual(values = c('red', 'blue'), labels = c('Non-star', 'Star'), name = 'Stardom Status')+
    coord_flip()
  
  r1ug1r<-ggplot(data = uta_quarter_mod, aes(x = Player, y = TRB, fill = Star))+
    geom_bar(position = 'dodge', stat = 'identity')+
    labs(x = 'Player', y = 'Rebounds', title = paste('Utah Player Rebounds {closest_state} for Game', toString(game), sep = ' '), subtitle = uta_subs[game])+
    ylim(0,25)+
    scale_fill_manual(values = c('red', 'blue'), labels = c('Non-star', 'Star'), name = 'Stardom Status')+
    coord_flip()
  
  r1dg1r_anim<-r1dg1r+transition_states(Quarter)
  animate(r1dg1r_anim, renderer = av_renderer(paste('r1dg',toString(game),'r.mp4', sep = '')))
  
  r1ug1r_anim<-r1ug1r+transition_states(Quarter)
  animate(r1ug1r_anim, renderer = av_renderer(paste('r1ug',toString(game),'r.mp4', sep = '')))
}

#Create one interactive plot with sliders for each team
r1_overall_mod<-subset(r1_overall, r1_overall$MP != 0.0)

r1_overall_mod$Points<-r1_overall_mod$PTS
r1_overall_mod$Rebounds<-r1_overall_mod$TRB
r1_overall_mod$Assists<-r1_overall_mod$AST
r1_overall_mod$Steals<-r1_overall_mod$STL
r1_overall_mod$Blocks<-r1_overall_mod$BLK
r1_overall_mod$`Offensive Rating`<-r1_overall_mod$ORtg
r1_overall_mod$`Defensive Rating`<-r1_overall_mod$DRtg

r1plotly<-ggplot(data = r1_overall_mod, aes(x = MP, y = `+/-`, color = Team, frame = Game))+
  geom_point(aes(text = Player, text2 = Points, text3 = Rebounds, text4 = Assists, text5 = Steals, text6 = Blocks, text7 = `Offensive Rating`, text8 = `Defensive Rating`))+
  labs(x = 'Minutes Played', y = '+/-')+
  geom_line(stat = 'smooth', method = lm, alpha = 0.2)+
  geom_ribbon(stat = 'smooth', method = lm, alpha = 0.2)+
  ggtitle('Effect of Minutes Played on a Player\'s Plus-Minus Rating')+
  scale_color_manual(values = c('blue','orange'), name = 'Team')

r1_interactive<-ggplotly(r1plotly, tooltip = c('text', 'text2', 'text3', 'text4', 'text5', 'text6', 'text7', 'text8'))
r1_interactive
saveWidget(r1_interactive, 'Round1Interactive.html')

##################################################################
#Round 2 Series

r2_overall<-read_csv('Denver_LAC_Round2.csv')
r2_half<-read_csv('Denver_LAC_Round2_By_Half.csv')
r2_quarter<-read_csv('Denver_LAC_Round2_By_Quarter.csv')

r2_overall$Star<-ifelse(r2_overall$Player %in% stars$Player, 1, 0)
r2_overall$Star<-as.factor(r2_overall$Star)
r2_half$Star<-ifelse(r2_half$Player %in% stars$Player, 1, 0)
r2_half$Star<-as.factor(r2_half$Star)
r2_quarter$Star<-ifelse(r2_quarter$Player %in% stars$Player, 1, 0)
r2_quarter$Star<-as.factor(r2_quarter$Star)

den_subs<-c('Denver Lost this Game', 'Denver Won this Game', 'Denver Lost this Game', 'Denver Lost this Game', 'Denver Won this Game', 'Denver Won this Game', 'Denver Won this Game')
lac_subs<-c('Los Angeles Won this Game', 'Los Angeles Lost this Game', 'Los Angeles Won this Game', 'Los Angeles Won this Game', 'Los Angeles Lost this Game', 'Los Angeles Lost this Game', 'Los Angeles Lost this Game')

for (game in c(1:7)) {
  sub_overall<-subset(r2_overall, r2_overall$Game == game)
  sub_half<-subset(r2_half, r2_half$Game == game)
  sub_quarter<-subset(r2_quarter, r2_quarter$Game == game)
  
  sub_quarter0<-subset(sub_quarter, sub_quarter$Quarter == 1)
  sub_quarter0$Quarter<-0
  sub_quarter0$PTS<-0
  sub_quarter0$TRB<-0
  sub_quarter1<-subset(sub_quarter, sub_quarter$Quarter == 1)
  sub_quarter2<-subset(sub_quarter, sub_quarter$Quarter == 2)
  sub_quarter2$PTS<-sub_quarter2$PTS + sub_quarter1$PTS
  sub_quarter2$TRB<-sub_quarter2$TRB + sub_quarter1$TRB
  sub_quarter3<-subset(sub_quarter, sub_quarter$Quarter == 3)
  sub_quarter3$PTS<-sub_quarter3$PTS + sub_quarter2$PTS
  sub_quarter3$TRB<-sub_quarter3$TRB + sub_quarter2$TRB
  sub_quarter4<-subset(sub_quarter, sub_quarter$Quarter == 4)
  sub_quarter4$PTS<-sub_quarter4$PTS + sub_quarter3$PTS
  sub_quarter4$TRB<-sub_quarter4$TRB + sub_quarter3$TRB
  sub_quarter_mod<-rbind(sub_quarter0, sub_quarter1, sub_quarter2, sub_quarter3, sub_quarter4)
  sub_quarter_mod$Quarter<-factor(
    sub_quarter_mod$Quarter, levels = c(0,1,2,3,4),
    labels = c('at the Start', 'after the 1st Quarter', 'after the 2nd Quarter', 'after the 3rd Quarter', 'after the 4th Quarter')
  )
  den_quarter_mod<-subset(sub_quarter_mod, sub_quarter_mod$Team == 'DEN')
  lac_quarter_mod<-subset(sub_quarter_mod, sub_quarter_mod$Team == 'LAC')
  
  r2dg1p<-ggplot(data = den_quarter_mod, aes(x = Player, y = PTS, fill = Star))+
    geom_bar(position = 'dodge', stat = 'identity')+
    labs(x = 'Player', y = 'Points', title = paste('Denver Player Points {closest_state} for Game', toString(game), sep = ' '), subtitle = den_subs[game])+
    ylim(0,60)+
    scale_fill_manual(values = c('red', 'blue'), labels = c('Non-star', 'Star'), name = 'Stardom Status')+
    coord_flip()
  
  r2lag1p<-ggplot(data = lac_quarter_mod, aes(x = Player, y = PTS, fill = Star))+
    geom_bar(position = 'dodge', stat = 'identity')+
    labs(x = 'Player', y = 'Points', title = paste('Los Angeles Player Points {closest_state} for Game', toString(game), sep = ' '), subtitle = lac_subs[game])+
    ylim(0,60)+
    scale_fill_manual(values = c('red', 'blue'), labels = c('Non-star', 'Star'), name = 'Stardom Status')+
    coord_flip()
  
  r2dg1p_anim<-r2dg1p+transition_states(Quarter)
  animate(r2dg1p_anim, renderer = av_renderer(paste('r2dg',toString(game),'p.mp4', sep = '')))
  
  r2lag1p_anim<-r2lag1p+transition_states(Quarter)
  animate(r2lag1p_anim, renderer = av_renderer(paste('r2lag',toString(game),'p.mp4', sep = '')))
  
  r2dg1r<-ggplot(data = den_quarter_mod, aes(x = Player, y = TRB, fill = Star))+
    geom_bar(position = 'dodge', stat = 'identity')+
    labs(x = 'Player', y = 'Rebounds', title = paste('Denver Player Rebounds {closest_state} for Game', toString(game), sep = ' '), subtitle = den_subs[game])+
    ylim(0,25)+
    scale_fill_manual(values = c('red', 'blue'), labels = c('Non-star', 'Star'), name = 'Stardom Status')+
    coord_flip()
  
  r2lag1r<-ggplot(data = lac_quarter_mod, aes(x = Player, y = TRB, fill = Star))+
    geom_bar(position = 'dodge', stat = 'identity')+
    labs(x = 'Player', y = 'Rebounds', title = paste('Los Angeles Player Rebounds {closest_state} for Game', toString(game), sep = ' '), subtitle = lac_subs[game])+
    ylim(0,25)+
    scale_fill_manual(values = c('red', 'blue'), labels = c('Non-star', 'Star'), name = 'Stardom Status')+
    coord_flip()
  
  r2dg1r_anim<-r2dg1r+transition_states(Quarter)
  animate(r2dg1r_anim, renderer = av_renderer(paste('r2dg',toString(game),'r.mp4', sep = '')))
  
  r2lag1r_anim<-r2lag1r+transition_states(Quarter)
  animate(r2lag1r_anim, renderer = av_renderer(paste('r2lag',toString(game),'r.mp4', sep = '')))
}

#Create one interactive plot with sliders for each team
r2_overall_mod<-subset(r2_overall, r2_overall$MP != 0.0)

r2_overall_mod$Points<-r2_overall_mod$PTS
r2_overall_mod$Rebounds<-r2_overall_mod$TRB
r2_overall_mod$Assists<-r2_overall_mod$AST
r2_overall_mod$Steals<-r2_overall_mod$STL
r2_overall_mod$Blocks<-r2_overall_mod$BLK
r2_overall_mod$`Offensive Rating`<-r2_overall_mod$ORtg
r2_overall_mod$`Defensive Rating`<-r2_overall_mod$DRtg

r2plotly<-ggplot(data = r2_overall_mod, aes(x = MP, y = `+/-`, color = Team, frame = Game))+
  geom_point(aes(text = Player, text2 = Points, text3 = Rebounds, text4 = Assists, text5 = Steals, text6 = Blocks, text7 = `Offensive Rating`, text8 = `Defensive Rating`))+
  labs(x = 'Minutes Played', y = '+/-')+
  geom_line(stat = 'smooth', method = lm, alpha = 0.2)+
  geom_ribbon(stat = 'smooth', method = lm, alpha = 0.2)+
  ggtitle('Effect of Minutes Played on a Player\'s Plus-Minus Rating')+
  scale_color_manual(values = c('blue','red'), name = 'Team')

r2_interactive<-ggplotly(r2plotly, tooltip = c('text', 'text2', 'text3', 'text4', 'text5', 'text6', 'text7', 'text8'))
r2_interactive
saveWidget(r2_interactive, 'Round2Interactive.html')







