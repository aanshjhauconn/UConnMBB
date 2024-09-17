library(dplyr)
library(ggplot2)
library(sportyR)

date <- format(Sys.Date(),'%m_%d')
date2 <- format(Sys.Date(),'%m-%d')
#date <- format(Sys.Date()-1,'%m_%d')
#date2 <- format(Sys.Date()-1,'%m-%d')
todays_data <- read.csv(paste('/Users/aansh/OneDrive/Desktop/UConn MBB/Shot Data/shot_data_09_09','.csv',sep=''))
#todays_data <- read.csv(paste('/Users/aansh/OneDrive/Desktop/UConn MBB/Shot Data/shot_data_',date,'.csv',sep=''))
totals_sheet <- read.csv('/Users/aansh/OneDrive/Desktop/UConn MBB/fall_totals.csv')

for (i in 1:nrow(todays_data)){
  if(todays_data$shot_type[i]=='Off Dribble'){
    if(todays_data$shot_distance[i]=='3'){
      todays_data$shot_type[i] <- 'Off Dribble 3'
    }else{
      todays_data$shot_type[i] <- 'Off Dribble 2'
    }
  }
}


for (i in 1:nrow(totals_sheet)){
  if(totals_sheet$shot_type[i]=='Off Dribble'){
    if(totals_sheet$shot_distance[i]=='3'){
      totals_sheet$shot_type[i] <- 'Off Dribble 3'
    }else{
      totals_sheet$shot_type[i] <- 'Off Dribble 2'
    }
  }
}

for (i in 1:nrow(todays_data)){
  if(todays_data$shot_type[i]=='Catch and Shoot'){
    if(todays_data$shot_distance[i]=='3'){
      todays_data$shot_type[i] <- 'Catch and Shoot 3'
    }else{
      todays_data$shot_type[i] <- 'Catch and Shoot 2'
    }
  }
}

for (i in 1:nrow(totals_sheet)){
  if(totals_sheet$shot_type[i]=='Catch and Shoot'){
    if(totals_sheet$shot_distance[i]=='3'){
      totals_sheet$shot_type[i] <- 'Catch and Shoot 3'
    }else{
      totals_sheet$shot_type[i] <- 'Catch and Shoot 2'
    }
  }
}

teamshootingstats <- function(dataframe){
  data <- dataframe
  made <- nrow(data %>% filter(result=='Made'))
  attempted <- nrow(data)
  pct <- made / attempted
  stat <- paste0(made,'/',attempted,' ',round(pct*100,1),'%')
}
#Returns PPS and % for team
getteampps <- function(dataframe){
  data <- dataframe
  if(nrow(data)==0){
    pps <- NA
  }else{
    for (i in 1:nrow(data)){
      if (data$result[i]=='Made'){
        if (data$shot_distance[i]=='3'){
          data$points[i] <- 3
        }else{
          data$points[i] <- 2
        }
      }else{
        data$points[i] <- 0
      }
    }
    pps <- mean(data$points)
  }
}
#plots by number of paint touches
#plot shottypes with size=number of touches
#table of percentages + TOV + fouls
painttouches <- function(dataframe,gtype){
  data <- dataframe
  touch0 <- data %>% filter(paint_touches=='0')
  touch1 <- data %>% filter(paint_touches=='1')
  touch2 <- data %>% filter(paint_touches=='2')
  touch3 <- data %>% filter(paint_touches=='3+')
  x1 <- print(teamshootingstats(touch0))
  x2 <- print(teamshootingstats(touch1))
  x3 <- print(teamshootingstats(touch2))
  x4 <- print(teamshootingstats(touch3))
  text1 <- data.frame(label=c(x1,x2,x3,x4),paint_touches=c('0','1','2','3+'))
  y1 <- paste0('PPS=',print(round(getteampps(touch0),2)))
  y2 <- paste0('PPS=',print(round(getteampps(touch1),2)))
  y3 <- paste0('PPS=',print(round(getteampps(touch2),2)))
  y4 <- paste0('PPS=',print(round(getteampps(touch3),2)))
  text2 <- data.frame(label=c(y1,y2,y3,y4),paint_touches=c('0','1','2','3+'))
  geom_basketball(league='ncaa',display_range='offense',rotation=270,x_trans=-47,y_trans=25,color_updates=list(offensive_half_court='#ecd5b5',
                                                                                                               painted_area='#0b2241',
                                                                                                               restricted_arc='#fffbfd',
                                                                                                               three_point_line=c('#e3122a','#ecd5b5'),
                                                                                                               backboard='#fffbfd',
                                                                                                               center_circle_fill='#0b2241',
                                                                                                               center_circle_outline='#fffbfd',
                                                                                                               free_throw_circle_fill='#ecd5b5',
                                                                                                               two_point_range='#ecd5b5',
                                                                                                               court_apron='#ecd5b5',
                                                                                                               free_throw_circle_outline='#0b2241',
                                                                                                               lane_space_mark='#0b2241',
                                                                                                               endline='#0b2241',
                                                                                                               sideline='#0b2241',
                                                                                                               division_line='#0b2241',
                                                                                                               inbounding_line='#0b2241',
                                                                                                               substitution_line='#0b2241',
                                                                                                               team_bench_line='#0b2241',
                                                                                                               lane_boundary='#0b2241',
                                                                                                               baseline_lower_defensive_box='#0b2241')) + 
    geom_jitter(data=data,aes(x=x,y=y,colour=as.factor(result),shape=as.factor(shot_type)),size=1) + 
    scale_shape_manual(values=c('Catch and Shoot 3'=16,'Catch and Shoot 2'=9,'Floater/Moving'=18,'Layup/Dunk'=15,'Off Dribble 2'=4,'Off Dribble 3'=17,'Post Move'=8)) +
    scale_color_manual(values=c('green','red')) +
    labs(shape='Shot Type') +
    labs(title=paste0('Team Shots by Paint Touch',', ',gtype)) + 
    labs(colour='Shot Result') + 
    facet_grid(~ paint_touches) +
    geom_text(data=text1,mapping=aes(x=12,y=-3,label=label),size=3) +
    geom_text(data=text2,mapping=aes(x=43,y=-3,label=label),size=3)
}
touch3 <- function(dataframe,gtype){
  data <- dataframe
  df <- data %>% filter(shot_distance=='3')
  notouch <- df %>% filter(paint_touches=='0')
  notouch$touch <- 'No Paint Touch'
  yestouch <- df %>% filter(paint_touches != '0')
  yestouch$touch <- 'Paint Touch'
  df <- rbind(notouch,yestouch)
  x1 <- print(teamshootingstats(notouch))
  x2 <- print(teamshootingstats(yestouch))
  text1 <- data.frame(label=c(x1,x2),touch=c('No Paint Touch','Paint Touch'))
  y1 <- paste0('PPS=',print(round(getteampps(notouch),2)))
  y2 <- paste0('PPS=',print(round(getteampps(yestouch),2)))
  text2 <- data.frame(label=c(y1,y2),touch=c('No Paint Touch','Paint Touch'))
  
  geom_basketball(league='ncaa',display_range='offense',rotation=270,x_trans=-47,y_trans=25,color_updates=list(offensive_half_court='#ecd5b5',
                                                                                                               painted_area='#0b2241',
                                                                                                               restricted_arc='#fffbfd',
                                                                                                               three_point_line=c('#e3122a','#ecd5b5'),
                                                                                                               backboard='#fffbfd',
                                                                                                               center_circle_fill='#0b2241',
                                                                                                               center_circle_outline='#fffbfd',
                                                                                                               free_throw_circle_fill='#ecd5b5',
                                                                                                               two_point_range='#ecd5b5',
                                                                                                               court_apron='#ecd5b5',
                                                                                                               free_throw_circle_outline='#0b2241',
                                                                                                               lane_space_mark='#0b2241',
                                                                                                               endline='#0b2241',
                                                                                                               sideline='#0b2241',
                                                                                                               division_line='#0b2241',
                                                                                                               inbounding_line='#0b2241',
                                                                                                               substitution_line='#0b2241',
                                                                                                               team_bench_line='#0b2241',
                                                                                                               lane_boundary='#0b2241',
                                                                                                               baseline_lower_defensive_box='#0b2241')) + 
    geom_jitter(data=df,aes(x=x,y=y,colour=as.factor(result),shape=as.factor(shot_type))) +
    scale_shape_manual(values=c('Catch and Shoot 3'=16,'Catch and Shot 2'=9,'Floater/Moving'=18,'Layup/Dunk'=15,'Off Dribble 2'=4,'Off Dribble 3'=17,'Post Move'=8)) +
    scale_color_manual(values=c('green','red')) +
    labs(title=paste0('3 Pointers With and Without a Paint Touch',', ',gtype)) + 
    labs(colour='Shot Result') + 
    labs(shape='Shot Type') +
    facet_grid(~touch) +
    geom_text(data=text1,mapping=aes(x=12,y=-3,label=label),size=3) +
    geom_text(data=text2,mapping=aes(x=43,y=-3,label=label),size=3)
}

pdf('/Users/aansh/OneDrive/Desktop/UConn MBB/Paint Touches/paint_touches.pdf',paper = 'a4r',height=7.5,width=10,onefile = T)
print(painttouches(todays_data,date2)) #format(Sys.Date(),'%m-%d'))
print(touch3(todays_data,date2))
print(painttouches(totals_sheet,'Total'))
print(touch3(totals_sheet,'Total'))
dev.off()