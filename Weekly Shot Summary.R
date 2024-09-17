library(dplyr)
library(ggplot2)
library(sportyR)
library(gridExtra)

#read in the weeks shots
totals_sheet <- read.csv('/Users/aansh/OneDrive/Desktop/UConn MBB/fall_totals.csv')
for (i in 1:nrow(totals_sheet)){
  if(totals_sheet$shot_type[i]=='Off Dribble'){
    if(totals_sheet$shot_distance[i]=='3'){
      totals_sheet$shot_type[i] <- 'Off Dribble 3'
    }else{
      totals_sheet$shot_type[i] <- 'Off Dribble 2'
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

this_week <- c('09_03_24','09_04_24','09_06_24','09_07_24') #Change the dates for each week
week_data <- totals_sheet[totals_sheet$date %in% this_week,]
week_in_question <- 'Weeks of 9/3-9/7' #Change Title







#Returns shot M/A and % for player
shootingstats <- function(name,dataframe){
  data <- dataframe %>% filter(player==name,shot_distance!='Free Throw')
  made <- nrow(data %>% filter(result=='Made'))
  attempted <- nrow(data)
  pct <- made / attempted
  stat <- paste0(made,'/',attempted,' ',round(pct*100,1),'%')
}

#Returns PPS and % for player
getpps <- function(name,dataframe){
  data <- dataframe %>% filter(player==name,shot_distance!='Free Throw')
  if (nrow(data)==0){
    pps <- NA
  }else{
    for (i in 1:nrow(data)){
      if (data$result[i]=='Made'){
        if (data$shot_distance[i]=='3'){
          data$points[i] <- 3
        }else if (data$shot_distance[i]=='Free Throw'){
          data$points[i] <- 1
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
#Graphs players shots for today next to full graph of shot
weekgraph <- function(name){
  df <- week_data %>% filter(player==name)
  df2 <- totals_sheet %>% filter(player==name)
  df$graphtype <- week_in_question
  df2$graphtype <- 'Total'
  data <- rbind(df,df2)
  x <- print(shootingstats(name,df))
  y <- print(shootingstats(name,df2))
  x1 <- paste0('PPS=',print(round(getpps(name,df),2)))
  y1 <- paste0('PPS=',print(round(getpps(name,df2),2)))
  text2 <- data.frame(label=c(x1,y1),graphtype=c(week_in_question,'Total'))
  text <- data.frame(label=c(x,y),graphtype=c(week_in_question,'Total')) 
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
    geom_jitter(data=data,aes(x=x,y=y,colour=as.factor(result),shape=as.factor(shot_type))) + 
    scale_shape_manual(values=c('Catch and Shoot 3'=16,'Catch and Shoot 2'=9,'Floater/Moving'=18,'Layup/Dunk'=15,'Off Dribble 2'=4,'Off Dribble 3'=17,'Post Move'=8)) + 
    scale_color_manual(values=c(Made='green',Missed='red')) +
    labs(title=name) + 
    labs(colour='Shot Result') + 
    labs(shape='Shot Type') +
    facet_grid(~ graphtype) +
    geom_text(data=text,mapping=aes(x=7,y=-3,label=label)) +
    geom_text(data=text2,mapping=aes(x=45,y=-3,label=label))
}
#Graphs shot types for each player
weekshottypes <- function(name){
  df <- week_data %>% filter(player==name)
  catchandshoot3 <- df %>% filter(shot_type=='Catch and Shoot 3')
  catchandshoot2 <- df %>% filter(shot_type=='Catch and Shoot 2')
  float <- df %>% filter(shot_type=='Floater/Moving')
  lay <- df %>% filter(shot_type=='Layup/Dunk')
  offdribble2 <- df %>% filter(shot_type=='Off Dribble 2')
  offdribble3 <- df %>% filter(shot_type=='Off Dribble 3')
  postmove <- df %>% filter(shot_type=='Post Move')
  x1 <- print(shootingstats(name,catchandshoot3))
  x2 <- print(shootingstats(name,catchandshoot2))
  x3 <- print(shootingstats(name,float))
  x4 <- print(shootingstats(name,lay))
  x5 <- print(shootingstats(name,offdribble2))
  x6 <- print(shootingstats(name,offdribble3))
  x7 <- print(shootingstats(name,postmove))
  text1 <- data.frame(label=c(x1,x2,x3,x4,x5,x6,x7),shot_type=c('Catch and Shoot 3','Catch and Shoot 2','Floater/Moving','Layup/Dunk','Off Dribble 2','Off Dribble 3','Post Move'))
  y1 <- paste0('PPS=',print(round(getpps(name,catchandshoot3),2)))
  y2 <- paste0('PPS=',print(round(getpps(name,catchandshoot2),2)))
  y3 <- paste0('PPS=',print(round(getpps(name,float),2)))
  y4 <- paste0('PPS=',print(round(getpps(name,lay),2)))
  y5 <- paste0('PPS=',print(round(getpps(name,offdribble2),2)))
  y6 <- paste0('PPS=',print(round(getpps(name,offdribble3),2)))
  y7 <- paste0('PPS=',print(round(getpps(name,postmove),2)))
  text2 <- data.frame(label=c(y1,y2,y3,y4,y5,y6,y7),shot_type=c('Catch and Shoot 3','Catch and Shoot 2','Floater/Moving','Layup/Dunk','Off Dribble 2','Off Dribble 3','Post Move'))
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
    geom_jitter(data=df,aes(x=x,y=y,colour=as.factor(result)),size=1) + 
    scale_color_manual(values=c(Made='green',Missed='red')) +
    labs(title=name) + 
    labs(colour='Shot Result') + 
    facet_grid(~ shot_type) +
    geom_text(data=text1,mapping=aes(x=12,y=-3,label=label),size=2) +
    geom_text(data=text2,mapping=aes(x=43,y=-3,label=label),size=2)
}
#Returns shot M/A and % for team
teamshootingstats <- function(dataframe){
  data <- dataframe %>% filter(shot_distance!='Free Throw')
  made <- nrow(data %>% filter(result=='Made'))
  attempted <- nrow(data)
  pct <- made / attempted
  stat <- paste0(made,'/',attempted,' ',round(pct*100,1),'%')
}

#Returns PPS and % for team
getteampps <- function(dataframe){
  if (nrow(dataframe)==0){
    pps <- NA
  }else{
    data <- dataframe %>% filter(shot_distance!='Free Throw')
    for (i in 1:nrow(data)){
      if (data$result[i]=='Made'){
        if (data$shot_distance[i]=='3'){
          data$points[i] <- 3
        }else if (data$shot_distance[i]=='Free Throw'){
          data$points[i] <- 1
        }else{
          data$points[i] <- 2
        }
      }else{
        data$points[i] <- 0
      }
    }
  }
  pps <- mean(data$points)
}
#Graphs teams shots for today and total
weekteam <- function(){
  df <- week_data
  df2 <- totals_sheet
  df$graphtype <- week_in_question
  df2$graphtype <- 'Total'
  data <- rbind(df,df2)
  x <- print(teamshootingstats(df))
  y <- print(teamshootingstats(df2))
  x1 <- paste0('PPS=',print(round(getteampps(df),2)))
  y1 <- paste0('PPS=',print(round(getteampps(df2),2)))
  text2 <- data.frame(label=c(x1,y1),graphtype=c(week_in_question,'Total'))  
  text <- data.frame(label=c(x,y),graphtype=c(week_in_question,'Total'))
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
    geom_jitter(data=data,aes(x=x,y=y,colour=as.factor(result),shape=as.factor(shot_type))) + 
    scale_shape_manual(values=c('Catch and Shoot 3'=16,'Catch and Shoot 2'=9,'Floater/Moving'=18,'Layup/Dunk'=15,'Off Dribble 2'=4,'Off Dribble 3'=17,'Post Move'=8)) + 
    scale_color_manual(values=c('green','red')) +
    labs(title='Team Shots') + 
    labs(colour='Shot Result') + 
    labs(shape='Shot Type') +
    facet_grid(~ graphtype) +
    geom_text(data=text,mapping=aes(x=7,y=-3,label=label)) +
    geom_text(data=text2,mapping=aes(x=45,y=-3,label=label))
}
#Graphs team shot types
weekteamshottypes <- function(){
  df <- week_data
  catchandshoot3 <- df %>% filter(shot_type=='Catch and Shoot 3')
  catchandshoot2 <- df %>% filter(shot_type=='Catch and Shoot 2')
  float <- df %>% filter(shot_type=='Floater/Moving')
  lay <- df %>% filter(shot_type=='Layup/Dunk')
  offdribble2 <- df %>% filter(shot_type=='Off Dribble 2')
  offdribble3 <- df %>% filter(shot_type=='Off Dribble 3')
  postmove <- df %>% filter(shot_type=='Post Move')
  x1 <- print(teamshootingstats(catchandshoot3))
  x2 <- print(teamshootingstats(catchandshoot2))
  x3 <- print(teamshootingstats(float))
  x4 <- print(teamshootingstats(lay))
  x5 <- print(teamshootingstats(offdribble2))
  x6 <- print(teamshootingstats(offdribble3))
  x7 <- print(teamshootingstats(postmove))
  text1 <- data.frame(label=c(x1,x2,x3,x4,x5,x6,x7),shot_type=c('Catch and Shoot 3','Catch and Shoot 2','Floater/Moving','Layup/Dunk','Off Dribble 2','Off Dribble 3','Post Move'))
  y1 <- paste0('PPS=',print(round(getteampps(catchandshoot3),2)))
  y2 <- paste0('PPS=',print(round(getteampps(catchandshoot2),2)))
  y3 <- paste0('PPS=',print(round(getteampps(float),2)))
  y4 <- paste0('PPS=',print(round(getteampps(lay),2)))
  y5 <- paste0('PPS=',print(round(getteampps(offdribble2),2)))
  y6 <- paste0('PPS=',print(round(getteampps(offdribble3),2)))
  y7 <- paste0('PPS=',print(round(getteampps(postmove),2)))
  text2 <- data.frame(label=c(y1,y2,y3,y4,y5,y6,y7),shot_type=c('Catch and Shoot 3','Catch and Shoot 2','Floater/Moving','Layup/Dunk','Off Dribble 2','Off Dribble 3','Post Move'))
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
    geom_jitter(data=df,aes(x=x,y=y,colour=as.factor(result)),size=1) + 
    scale_color_manual(values=c('green','red')) +
    labs(title='Team Shots') + 
    labs(colour='Shot Result') + 
    facet_grid(~ shot_type) +
    geom_text(data=text1,mapping=aes(x=12,y=-3,label=label),size=2) +
    geom_text(data=text2,mapping=aes(x=43,y=-3,label=label),size=2)
}
#Data table of shots/PPS for week ---------------------------------------
week_data_frame <- data.frame(player=sort(unique(week_data$player)))
#all m/a/pct
week_data_frame$makes <- 0
for (i in sort(unique(week_data$player))){
  week_data_frame[which(week_data_frame$player==i),]$makes <- as.numeric(nrow(week_data %>% filter(player==i,result=='Made',shot_distance!='Free Throw')))
}
week_data_frame$attempts <- 0
for (i in sort(unique(week_data$player))){
  week_data_frame[which(week_data_frame$player==i),]$attempts <- as.numeric(nrow(week_data %>% filter(player==i,shot_distance!='Free Throw')))
}
week_data_frame$pct <- round((week_data_frame$makes / week_data_frame$attempts)*100,1)
#threes m/a/pct
week_data_frame$made3 <- 0
for (i in sort(unique(week_data$player))){
  week_data_frame[which(week_data_frame$player==i),]$made3 <- as.numeric(nrow(week_data %>% filter(player==i,shot_distance!='Free Throw',result=='Made',shot_distance=='3')))
}
week_data_frame$attempt3 <- 0
for (i in sort(unique(week_data$player))){
  week_data_frame[which(week_data_frame$player==i),]$attempt3 <- as.numeric(nrow(week_data %>% filter(player==i,shot_distance!='Free Throw',shot_distance=='3')))
}
week_data_frame$pct3 <- round((week_data_frame$made3 / week_data_frame$attempt3)*100,1)
#paint m/a/pct
week_data_frame$made.paint <- 0
for (i in sort(unique(week_data$player))){
  week_data_frame[which(week_data_frame$player==i),]$made.paint <- as.numeric(nrow(week_data %>% filter(player==i,shot_distance!='Free Throw',result=='Made',shot_distance=='Layup/Dunk' | shot_distance=='Paint')))
}
week_data_frame$attempt.paint <- 0
for (i in sort(unique(week_data$player))){
  week_data_frame[which(week_data_frame$player==i),]$attempt.paint <- as.numeric(nrow(week_data %>% filter(player==i,shot_distance!='Free Throw',shot_distance=='Layup/Dunk' | shot_distance=='Paint')))
}
week_data_frame$pct_paint <- round((week_data_frame$made.paint / week_data_frame$attempt.paint)*100,1)
#long 2 m/a/pct
week_data_frame$made.long2 <- 0
for (i in sort(unique(week_data$player))){
  week_data_frame[which(week_data_frame$player==i),]$made.long2 <- as.numeric(nrow(week_data %>% filter(player==i,shot_distance!='Free Throw',result=='Made',shot_distance=='Long 2')))
}
week_data_frame$attempt.long2 <- 0
for (i in sort(unique(week_data$player))){
  week_data_frame[which(week_data_frame$player==i),]$attempt.long2 <- as.numeric(nrow(week_data %>% filter(player==i,shot_distance!='Free Throw',shot_distance=='Long 2')))
}
week_data_frame$pct.long2 <- round((week_data_frame$made.long2 / week_data_frame$attempt.long2)*100,1)
#pps
week_data_frame$pps <- 0
for (i in sort(unique(week_data$player))){
  week_data_frame[which(week_data_frame$player==i),]$pps <- round(((nrow(week_data %>% filter(player==i,shot_distance!='Free Throw',result=='Made',shot_distance=='3'))*3 +
                                                                      (nrow(week_data %>% filter(player==i,shot_distance!='Free Throw',result=='Made',shot_distance=='Layup/Dunk' | shot_distance=='Long 2' | shot_distance=='Paint')))*2)) /
                                                                    (nrow(week_data %>% filter(player==i,shot_distance!='Free Throw'))),2)
}
week_data_frame$pps3 <- round((week_data_frame$pct3 * 3) /100,2)
week_data_frame$pps.paint <- round((week_data_frame$pct_paint * 2)/100,2)
week_data_frame$pps.long2 <- round((week_data_frame$pct.long2 * 2)/100,2)
week_data_frame <- week_data_frame[,c('player','pps','pps3','pps.paint','pps.long2')]

#Data table of shots/PPS for totals --------------------------------------
totals_data_frame <- data.frame(player=sort(unique(totals_sheet$player)))
#all m/a/pct
totals_data_frame$makes <- 0
for (i in sort(unique(totals_sheet$player))){
  totals_data_frame[which(totals_data_frame$player==i),]$makes <- as.numeric(nrow(totals_sheet %>% filter(player==i,result=='Made',shot_distance!='Free Throw')))
}
totals_data_frame$attempts <- 0
for (i in sort(unique(totals_sheet$player))){
  totals_data_frame[which(totals_data_frame$player==i),]$attempts <- as.numeric(nrow(totals_sheet %>% filter(player==i,shot_distance!='Free Throw')))
}
totals_data_frame$pct <- round((totals_data_frame$makes / totals_data_frame$attempts)*100,1)
#threes m/a/pct
totals_data_frame$made3 <- 0
for (i in sort(unique(totals_sheet$player))){
  totals_data_frame[which(totals_data_frame$player==i),]$made3 <- as.numeric(nrow(totals_sheet %>% filter(player==i,shot_distance!='Free Throw',result=='Made',shot_distance=='3')))
}
totals_data_frame$attempt3 <- 0
for (i in sort(unique(totals_sheet$player))){
  totals_data_frame[which(totals_data_frame$player==i),]$attempt3 <- as.numeric(nrow(totals_sheet %>% filter(player==i,shot_distance!='Free Throw',shot_distance=='3')))
}
totals_data_frame$pct3 <- round((totals_data_frame$made3 / totals_data_frame$attempt3)*100,1)
#paint m/a/pct
totals_data_frame$made.paint <- 0
for (i in sort(unique(totals_sheet$player))){
  totals_data_frame[which(totals_data_frame$player==i),]$made.paint <- as.numeric(nrow(totals_sheet %>% filter(player==i,shot_distance!='Free Throw',result=='Made',shot_distance=='Layup/Dunk' | shot_distance=='Paint')))
}
totals_data_frame$attempt.paint <- 0
for (i in sort(unique(totals_sheet$player))){
  totals_data_frame[which(totals_data_frame$player==i),]$attempt.paint <- as.numeric(nrow(totals_sheet %>% filter(player==i,shot_distance!='Free Throw',shot_distance=='Layup/Dunk' | shot_distance=='Paint')))
}
totals_data_frame$pct_paint <- round((totals_data_frame$made.paint / totals_data_frame$attempt.paint)*100,1)
#long 2 m/a/pct
totals_data_frame$made.long2 <- 0
for (i in sort(unique(totals_sheet$player))){
  totals_data_frame[which(totals_data_frame$player==i),]$made.long2 <- as.numeric(nrow(totals_sheet %>% filter(player==i,shot_distance!='Free Throw',result=='Made',shot_distance=='Long 2')))
}
totals_data_frame$attempt.long2 <- 0
for (i in sort(unique(totals_sheet$player))){
  totals_data_frame[which(totals_data_frame$player==i),]$attempt.long2 <- as.numeric(nrow(totals_sheet %>% filter(player==i,shot_distance!='Free Throw',shot_distance=='Long 2')))
}
totals_data_frame$pct.long2 <- round((totals_data_frame$made.long2 / totals_data_frame$attempt.long2)*100,1)
#pps
totals_data_frame$total.pps <- 0
for (i in sort(unique(totals_sheet$player))){
  totals_data_frame[which(totals_data_frame$player==i),]$total.pps <- round(((nrow(totals_sheet %>% filter(player==i,shot_distance!='Free Throw',result=='Made',shot_distance=='3'))*3 +
                                                                                (nrow(totals_sheet %>% filter(player==i,shot_distance!='Free Throw',result=='Made',shot_distance=='Layup/Dunk' | shot_distance=='Long 2' | shot_distance=='Paint')))*2)) /
                                                                              (nrow(totals_sheet %>% filter(player==i,shot_distance!='Free Throw'))),2)
}
totals_data_frame$total.pps3 <- round((totals_data_frame$pct3 * 3) /100,2)
totals_data_frame$total.pps.paint <- round((totals_data_frame$pct_paint * 2)/100,2)
totals_data_frame$total.pps.long2 <- round((totals_data_frame$pct.long2 * 2)/100,2)
totals_data_frame <- totals_data_frame[,c('player','total.pps','total.pps3','total.pps.paint','total.pps.long2')]

# Export Graphs -------
pdf('/Users/aansh/OneDrive/Desktop/UConn MBB/Weekly Summary/week_summary.pdf',paper = 'a4r',height=7.5,width=10,onefile = T)
for (name in sort(unique(week_data$player))){
  print(weekgraph(name))
}
for (name in sort(unique(totals_sheet$player))){
  print(weekshottypes(name))
}
print(weekteam())
print(weekteamshottypes())
plot.new()
print(grid.table(week_data_frame))
plot.new()
print(grid.table(totals_data_frame))
dev.off()