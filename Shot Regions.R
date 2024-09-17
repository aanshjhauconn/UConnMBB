library(dplyr)
library(sportyR)
library(ggplot2)

#L/R corner 3
#Left:xmin=0,xmax=3.5,ymin=0,ymax=10
#Right:xmin=46.5,xmax=50,ymin=0,ymax=10
#L/R wing 3
#Left: anything a 3 with x between 3.5 and 15.5 or x between (0,3.5) and y>10
#Right: anything a 3 with x between 34.5 and 46.5 or x between (46.5,50) and y>10
#top of the key 3
#Anything a 3 with x between 15.5 and 34.5
#L/R  baseline jumper
#Left: anything not a 3 with x between 3.5 and 19, y between 0 and 8
#Right: anything not a 3 with x between 31 and 46.5, y between 0 and 8
#L/R elbow/wing jumper
#Left: anything not a 3 with x between 3.5 and 19, y >8
#Right: anything not a 3 with x between 31 and 46.5, y between 0 and 8
#Straight on jumper
#Anything not a 3 with x between 19 and 31, y>19
#Deep paint L/R
#Left: rectangle x:(19,25) y:(14.75,19)
#Right: rectangle x:(25,31) y(14.75,19)
#Mid paint L/R
#Left: rectangle x:(19,25) y:(10,14.75)
#Right: rectangle x:(25,31) y:(10,14.75)
#Short paint L/R
#Left: rectangle x:(19,25) y:(0,10)
#Right: rectangle y:(25,31) y:(0,10)

data <- read.csv('/Users/aansh/OneDrive/Desktop/UConn MBB/summer_totals.csv')
data$court_region <- 0
for (i in 1:nrow(data)) {
  #corner 3's
  if (data$y[i] <= 10) {
    if (data$x[i] < 3.5) {
      data$court_region[i] <- 'Right Corner 3'
    } else if (data$x[i] > 46.5) {
      data$court_region[i] <- 'Left Corner 3'
    }
  } else if (3.5 <= data$x[i] &
             data$x[i] <= 15.5 & data$shot_distance[i] == '3') {
    #wing 3's
    data$court_region[i] <- 'Right Wing 3'
  } else if (data$x[i] < 3.5 &
             data$y[i] > 10 & data$shot_distance[i] == '3') {
    data$court_region[i] <- 'Right Wing 3'
  } else if (34.5 <= data$x[i] &
             data$x[i] <= 46.5 & data$shot_distance[i] == '3') {
    data$court_region[i] <- 'Left Wing 3'
  } else if (data$x[i] >= 46.5 &
             data$y[i] >= 10 & data$shot_distance[i] == '3') {
    data$court_region[i] <- 'Left Wing 3'
  } else if (15.5 <= data$x[i] &
             data$x[i] <= 34.5 & data$shot_distance[i] == '3') {
    #straight on 3
    data$court_region[i] <- 'Straight on 3'
  } else if (3.5 <= data$x[i] &
             data$x[i] <= 19 &
             data$y[i] >= 8 & data$shot_distance[i] != '3') {
    #wing jumpers
    data$court_region[i] <- 'Right Wing 2'
  } else if (31 <= data$x[i] &
             data$x[i] <= 46.5 &
             data$y[i] >= 8 & data$shot_distance[i] != '3') {
    data$court_region[i] <- 'Left Wing 2'
  } else if (19 <= data$x[i] &
             data$x[i] <= 31 &
             data$y[i] >= 19 &
             data$shot_distance[i] != '3') {
    #straight on jumpers
    data$court_region[i] <- 'Straight on 2'
  } else if (19 <= data$x[i] &
             data$x[i] <= 25 &
             14.75 <= data$y[i] & data$y[i] <= 19) {
    #Deep Paint
    data$court_region[i] <- 'Deep Paint Right'
  } else if (25 <= data$x[i] &
             data$x[i] <= 31 &
             14.75 <= data$y[i] & data$y[i] <= 19) {
    data$court_region[i] <- 'Deep Paint Left'
  } else if (19 <= data$x[i] &
             data$x[i] <= 25 &
             10 <= data$y[i] & data$y[i] <= 14.75) {
    #Mid Paint
    data$court_region[i] <- 'Mid Paint Right'
  } else if (25 <= data$x[i] &
             data$x[i] <= 31 &
             10 <= data$y[i] & data$y[i] <= 14.75) {
    data$court_region[i] <- 'Mid Paint Left'
  }
}
for (i in 1:nrow(data)) {
  if (19 <= data$x[i] &
      data$x[i] <= 25 & data$y[i] <= 10) {
    #Short paint
    data$court_region[i] <- 'Short Paint Right'
  } else if (25 <= data$x[i] & data$x[i] <= 31 & data$y[i] <= 10) {
    data$court_region[i] <- 'Short Paint Left'
  }
}
for (i in 1:nrow(data)) {
  if (3.5 <= data$x[i] &
      data$x[i] <= 19 &
      data$y[i] <= 8 & data$shot_distance[i] != '3') {
    #baseline 2's
    data$court_region[i] <- 'Right Baseline 2'
  } else if (31 <= data$x[i] &
             data$x[i] <= 46.5 &
             data$y[i] <= 8 & data$shot_distance[i] != '3') {
    data$court_region[i] <- 'Left Baseline 2'
  }
}
for (i in 1:nrow(data)) {
  if (data$x[i] <= 25 &
      data$shot_distance[i] == '3' & data$court_region[i] == 0) {
    data$court_region[i] <- 'Right Corner 3'
  } else if (data$x[i] >= 25 &
             data$shot_distance[i] == '3' &
             data$court_region[i] == 0) {
    data$court_region[i] <- 'Left Corner 3'
  }
}
for (i in 1:nrow(data)) {
  if (data$court_region[i] == 0) {
    if (data$x[i] <= 25) {
      data$court_region[i] <- 'Right Wing 2'
    } else if (data$x[i] > 25) {
      data$court_region[i] <- 'Left Wing 2'
    }
  }
}
shotregions <- function(name) {
  df <- data %>% filter(player == name)
  dpl <- df %>% filter(court_region == 'Deep Paint Left')
  dpr <- df %>% filter(court_region == 'Deep Paint Right')
  lb2 <- df %>% filter(court_region == 'Left Baseline 2')
  lc3 <- df %>% filter(court_region == 'Left Corner 3')
  lw2 <- df %>% filter(court_region == 'Left Wing 2')
  lw3 <- df %>% filter(court_region == 'Left Wing 3')
  mpl <- df %>% filter(court_region == 'Mid Paint Left')
  mpr <- df %>% filter(court_region == 'Mid Paint Right')
  rb2 <- df %>% filter(court_region == 'Right Baseline 2')
  rc3 <- df %>% filter(court_region == 'Right Corner 3')
  rw2 <- df %>% filter(court_region == 'Right Wing 2')
  rw3 <- df %>% filter(court_region == 'Right Wing 3')
  spl <- df %>% filter(court_region == 'Short Paint Left')
  spr <- df %>% filter(court_region == 'Short Paint Right')
  so2 <- df %>% filter(court_region == 'Straight on 2')
  so3 <- df %>% filter(court_region == 'Straight on 3')
  x1 <- print(shootingstats(name, dpl))
  x2 <- print(shootingstats(name, dpr))
  x3 <- print(shootingstats(name, lb2))
  x4 <- print(shootingstats(name, lc3))
  x5 <- print(shootingstats(name, lw2))
  x6 <- print(shootingstats(name, lw3))
  x7 <- print(shootingstats(name, mpl))
  x8 <- print(shootingstats(name, mpr))
  x9 <- print(shootingstats(name, rb2))
  x10 <- print(shootingstats(name, rc3))
  x11 <- print(shootingstats(name, rw2))
  x12 <- print(shootingstats(name, rw3))
  x13 <- print(shootingstats(name, spl))
  x14 <- print(shootingstats(name, spr))
  x15 <- print(shootingstats(name, so2))
  x16 <- print(shootingstats(name, so3))
  threes <- df %>% filter(
    court_region %in% c(
      'Left Corner 3',
      'Right Corner 3',
      'Left Wing 3',
      'Right Wing 3',
      'Straight on 3'
    )
  )
  twos <- df %>% filter(
    court_region %in% c(
      'Left Baseline 2',
      'Right Baseline 2',
      'Left Wing 2',
      'Right Wing 2',
      'Straight on 2'
    )
  )
  paint <- df %>% filter(
    court_region %in% c(
      'Deep Paint Left',
      'Deep Paint Right',
      'Mid Paint Left',
      'Mid Paint Right',
      'Short Paint Left',
      'Short Paint Right'
    )
  )
  #text1 <- data.frame(label=c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16),
  #court_region=c('Deep Paint Left','Deep Paint Right','Left Baseline 2','Left Corner 3',
  #'Left Wing 2','Left Wing 3','Mid Paint Left','Mid Paint Right',
  #'Right Baseline 2','Right Corner 3','Right Wing 2','Right Wing 3',
  #'Short Paint Left','Short Paint Right','Straight On 2','Straight on 3'))
  textx1 <- data.frame(
    label = c(x4, x6, x10, x12, x16),
    court_region = c(
      'Left Corner 3',
      'Left Wing 3',
      'Right Corner 3',
      'Right Wing 3',
      'Straight on 3'
    )
  )
  textx2 <- data.frame(
    label = c(x3, x5, x9, x11, x15),
    court_region = c(
      'Left Baseline 2',
      'Left Wing 2',
      'Right Baseline 2',
      'Right Wing 2',
      'Straight on 2'
    )
  )
  textx3 <- data.frame(
    label = c(x1, x2, x7, x8, x13, x14),
    court_region = c(
      'Deep Paint Left',
      'Deep Paint Right',
      'Mid Paint Left',
      'Mid Paint Right',
      'Short Paint Left',
      'Short Paint Right'
    )
  )
  y1 <- paste0('PPS=', print(round(getpps(name, dpl), 2)))
  y2 <- paste0('PPS=', print(round(getpps(name, dpr), 2)))
  y3 <- paste0('PPS=', print(round(getpps(name, lb2), 2)))
  y4 <- paste0('PPS=', print(round(getpps(name, lc3), 2)))
  y5 <- paste0('PPS=', print(round(getpps(name, lw2), 2)))
  y6 <- paste0('PPS=', print(round(getpps(name, lw3), 2)))
  y7 <- paste0('PPS=', print(round(getpps(name, mpl), 2)))
  y8 <- paste0('PPS=', print(round(getpps(name, mpr), 2)))
  y9 <- paste0('PPS=', print(round(getpps(name, rb2), 2)))
  y10 <- paste0('PPS=', print(round(getpps(name, rc3), 2)))
  y11 <- paste0('PPS=', print(round(getpps(name, rw2), 2)))
  y12 <- paste0('PPS=', print(round(getpps(name, rw3), 2)))
  y13 <- paste0('PPS=', print(round(getpps(name, spl), 2)))
  y14 <- paste0('PPS=', print(round(getpps(name, spr), 2)))
  y15 <- paste0('PPS=', print(round(getpps(name, so2), 2)))
  y16 <- paste0('PPS=', print(round(getpps(name, so3), 2)))
  #text2 <- data.frame(label=c(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16),
  #court_region=c('Deep Paint Left','Deep Paint Right','Left Baseline 2','Left Corner 3',
  #'Left Wing 2','Left Wing 3','Mid Paint Left','Mid Paint Right',
  #'Right Baseline 2','Right Corner 3','Right Wing 2','Right Wing 3',
  #'Short Paint Left','Short Paint Right','Straight On 2','Straight on 3'))
  texty1 <- data.frame(
    label = c(y4, y6, y10, y12, y16),
    court_region = c(
      'Left Corner 3',
      'Left Wing 3',
      'Right Corner 3',
      'Right Wing 3',
      'Straight on 3'
    )
  )
  texty2 <- data.frame(
    label = c(y3, y5, y9, y11, y15),
    court_region = c(
      'Left Baseline 2',
      'Left Wing 2',
      'Right Baseline 2',
      'Right Wing 2',
      'Straight on 2'
    )
  )
  texty3 <- data.frame(
    label = c(y1, y2, y7, y8, y13, y14),
    court_region = c(
      'Deep Paint Left',
      'Deep Paint Right',
      'Mid Paint Left',
      'Mid Paint Right',
      'Short Paint Left',
      'Short Paint Right'
    )
  )
  print(
    geom_basketball(
      league = 'ncaa',
      display_range = 'offense',
      rotation = 270,
      x_trans = -47,
      y_trans = 25,
      color_updates = list(
        offensive_half_court = '#ecd5b5',
        painted_area =
          '#0b2241',
        restricted_arc =
          '#fffbfd',
        three_point_line =
          c('#e3122a', '#ecd5b5'),
        backboard =
          '#fffbfd',
        center_circle_fill =
          '#0b2241',
        center_circle_outline =
          '#fffbfd',
        free_throw_circle_fill =
          '#ecd5b5',
        two_point_range =
          '#ecd5b5',
        court_apron =
          '#ecd5b5',
        free_throw_circle_outline =
          '#0b2241',
        lane_space_mark =
          '#0b2241',
        endline =
          '#0b2241',
        sideline =
          '#0b2241',
        division_line =
          '#0b2241',
        inbounding_line =
          '#0b2241',
        substitution_line =
          '#0b2241',
        team_bench_line =
          '#0b2241',
        lane_boundary =
          '#0b2241',
        baseline_lower_defensive_box =
          '#0b2241'
      )
    ) +
      geom_jitter(
        data = threes,
        aes(
          x = x,
          y = y,
          colour = as.factor(result)
        ),
        size = 1
      ) +
      scale_color_manual(values = c(
        Made = 'green', Missed = 'red'
      )) +
      labs(title = paste0(name, ' ', 'Three Pointers')) +
      labs(colour = 'Shot Result') +
      facet_wrap(~ court_region) +
      geom_text(
        data = textx1,
        mapping = aes(x = 12, y = -3, label = label),
        size = 3
      ) +
      geom_text(
        data = texty1,
        mapping = aes(x = 43, y = -3, label = label),
        size = 3
      )
  )
  print(
    geom_basketball(
      league = 'ncaa',
      display_range = 'offense',
      rotation = 270,
      x_trans = -47,
      y_trans = 25,
      color_updates = list(
        offensive_half_court = '#ecd5b5',
        painted_area =
          '#0b2241',
        restricted_arc =
          '#fffbfd',
        three_point_line =
          c('#e3122a', '#ecd5b5'),
        backboard =
          '#fffbfd',
        center_circle_fill =
          '#0b2241',
        center_circle_outline =
          '#fffbfd',
        free_throw_circle_fill =
          '#ecd5b5',
        two_point_range =
          '#ecd5b5',
        court_apron =
          '#ecd5b5',
        free_throw_circle_outline =
          '#0b2241',
        lane_space_mark =
          '#0b2241',
        endline =
          '#0b2241',
        sideline =
          '#0b2241',
        division_line =
          '#0b2241',
        inbounding_line =
          '#0b2241',
        substitution_line =
          '#0b2241',
        team_bench_line =
          '#0b2241',
        lane_boundary =
          '#0b2241',
        baseline_lower_defensive_box =
          '#0b2241'
      )
    ) +
      geom_jitter(
        data = twos,
        aes(
          x = x,
          y = y,
          colour = as.factor(result)
        ),
        size = 1
      ) +
      scale_color_manual(values = c(
        Made = 'green', Missed = 'red'
      )) +
      labs(title = paste0(name, ' ', 'Two Pointers')) +
      labs(colour = 'Shot Result') +
      facet_wrap(~ court_region) +
      geom_text(
        data = textx2,
        mapping = aes(x = 12, y = -3, label = label),
        size = 3
      ) +
      geom_text(
        data = texty2,
        mapping = aes(x = 43, y = -3, label = label),
        size = 3
      )
  )
  print(
    geom_basketball(
      league = 'ncaa',
      display_range = 'offense',
      rotation = 270,
      x_trans = -47,
      y_trans = 25,
      color_updates = list(
        offensive_half_court = '#ecd5b5',
        painted_area =
          '#0b2241',
        restricted_arc =
          '#fffbfd',
        three_point_line =
          c('#e3122a', '#ecd5b5'),
        backboard =
          '#fffbfd',
        center_circle_fill =
          '#0b2241',
        center_circle_outline =
          '#fffbfd',
        free_throw_circle_fill =
          '#ecd5b5',
        two_point_range =
          '#ecd5b5',
        court_apron =
          '#ecd5b5',
        free_throw_circle_outline =
          '#0b2241',
        lane_space_mark =
          '#0b2241',
        endline =
          '#0b2241',
        sideline =
          '#0b2241',
        division_line =
          '#0b2241',
        inbounding_line =
          '#0b2241',
        substitution_line =
          '#0b2241',
        team_bench_line =
          '#0b2241',
        lane_boundary =
          '#0b2241',
        baseline_lower_defensive_box =
          '#0b2241'
      )
    ) +
      geom_jitter(
        data = paint,
        aes(
          x = x,
          y = y,
          colour = as.factor(result)
        ),
        size = 1
      ) +
      scale_color_manual(values = c(
        Made = 'green', Missed = 'red'
      )) +
      labs(title = paste0(name, ' ', 'Paint Shots')) +
      labs(colour = 'Shot Result') +
      facet_wrap(~ court_region) +
      geom_text(
        data = textx3,
        mapping = aes(x = 12, y = -3, label = label),
        size = 3
      ) +
      geom_text(
        data = texty3,
        mapping = aes(x = 43, y = -3, label = label),
        size = 3
      )
  )
}
pdf(
  '/Users/aansh/Desktop/UConn MBB/shot_regions.pdf',
  paper = 'a4r',
  height = 7.5,
  width = 10,
  onefile = T
)
for (name in sort(unique(data$player))) {
  shotregions(name)
}
dev.off()