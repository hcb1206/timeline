
#FOR VEG TIMELINE: 

rm(list = ls())
setwd("~/R/timeline")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(readxl)
library(ggpubr)

df <- read_excel('timelineveg.xlsx')
colnames(df) <- c("dayofgrowth", "weekabbv", "task", "room", "color")
df

x <- df %>% 
  separate(task, c("task1", "task2", "task3"), sep = ", ")
x <- pivot_longer(x, c("task1", "task2", "task3"), 
             names_to = "event", values_to = "task")
df <- select(x, !event)
rm(x)

cycleweek <- c("Yellow", "Green", "Blue")
cyclecolors <- c("gold", "green3", "royalblue")

df$color <- factor(df$color, levels=cycleweek, ordered = TRUE)

positions <- c(0.2, -0.2, 0.4, -0.4, 0.6, -0.6, 0.8, -0.8, 1.0, -1.0, 1.2, -1.2, 1.4, -1.4)
directions <- c(1, -1)

line_pos <- data.frame(
  "dayofgrowth"=unique(df$dayofgrowth),
  "position" = rep(positions, length.out = length(unique(df$dayofgrowth))),
  "direction" = rep(directions, length.out = length(unique(df$dayofgrowth))))

df <- merge(x=df, y=line_pos, by="dayofgrowth", all = TRUE)
df <- df[with(df, order(dayofgrowth)), ]

text_offset <- 0.07

df$day_count <- ave(df$dayofgrowth==df$dayofgrowth, df$dayofgrowth, FUN=cumsum)
df$text_position <- (df$day_count * text_offset * df$direction) +
  df$position

df[124,6] = 1.3
df[124,7] = 1
df[124,9] =1.51
df[125,6] = 1.3
df[125,7] =1
df[125,9] =1.44
df[126,6] = 1.3
df[126,7] =1
df[126,9] =1.37
df[160,6] = -1.4
df[160,9] = -1.47

head(df)

dayofgrowth_range <- c(0, 1:166)
dayofgrowth_df <- data.frame(dayofgrowth_range)
df$taskassigned <- ifelse(is.na(df$task), 0, 1)

timeline_plot<-ggplot(df,aes(x=dayofgrowth,y=0, col=color, label=task))
timeline_plot<-timeline_plot+labs(col="Week")
timeline_plot<-timeline_plot+scale_color_manual(values=cyclecolors, 
                                                labels=cycleweek, drop = FALSE)
timeline_plot<-timeline_plot+ theme_transparent(base_size = 12, base_family = "") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
timeline_plot

# Plot horizontal black line for timeline
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                                        color = "black", size=0.3)
timeline_plot

# Plot vertical segment lines for milestones
timeline_plot<-timeline_plot+geom_segment(data = df[df$taskassigned ==1,], 
                                          aes(xend = dayofgrowth, y = position,
                                              yend = 0), 
                                          color = 'black', size = 0.2)
timeline_plot
# Plot scatter points at zero and date
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=2.5)

# Don't show axes, appropriately position legend
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "bottom"
)

# Show text for each task
timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=task),size=3)
print(timeline_plot)

#FOR FLOWER TIMELINE: 

rm(list = ls())
setwd("~/R")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)
library(readxl)
library(ggpubr)

df <- read_excel('timelineflower.xlsx')
colnames(df) <- c("dayofgrowth", "weekabbv", "task", "room", "color")
df

x <- df %>% 
  separate(task, c("task1", "task2", "task3"), sep = ", ")
x <- pivot_longer(x, c("task1", "task2", "task3"), 
                  names_to = "event", values_to = "task")
df <- select(x, !event)
rm(x)

cycleweek <- c("Yellow", "Green", "Blue")
cyclecolors <- c("gold", "green3", "royalblue")

df$color <- factor(df$color, levels=cycleweek, ordered = TRUE)

positions <- c(0.2, -0.2, 0.4, -0.4, 0.6, -0.6, 0.8, -0.8, 1.0, -1.0, 1.2, -1.2, 1.4, -1.4)
directions <- c(1, -1)

line_pos <- data.frame(
  "dayofgrowth"=unique(df$dayofgrowth),
  "position" = rep(positions, length.out = length(unique(df$dayofgrowth))),
  "direction" = rep(directions, length.out = length(unique(df$dayofgrowth))))

df <- merge(x=df, y=line_pos, by="dayofgrowth", all = TRUE)
df <- df[with(df, order(dayofgrowth)), ]

text_offset <- 0.07

df$day_count <- ave(df$dayofgrowth==df$dayofgrowth, df$dayofgrowth, FUN=cumsum)
df$text_position <- (df$day_count * text_offset * df$direction) +
  df$position


df[1,9] = 0.34
df[2,9] = 0.27
df[1,8] = 2
df[2,8] = 1

head(df)


dayofgrowth_range <- c(55:114)
dayofgrowth_df <- data.frame(dayofgrowth_range)
df$taskassigned <- ifelse(is.na(df$task), 0, 1)

timeline_plot<-ggplot(df,aes(x=dayofgrowth,y=0, col=color, label=task))
timeline_plot<-timeline_plot+labs(col="Week")
timeline_plot<-timeline_plot+scale_color_manual(values=cyclecolors, 
                                                labels=cycleweek, drop = FALSE)
timeline_plot<-timeline_plot+ theme_transparent(base_size = 12, base_family = "") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
timeline_plot

# Plot horizontal black line for timeline
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                                        color = "black", size=0.3)
timeline_plot

# Plot vertical segment lines for milestones
timeline_plot<-timeline_plot+geom_segment(data = df[df$taskassigned ==1,], 
                                          aes(xend = dayofgrowth, y = position,
                                              yend = 0), 
                                          color = 'black', size = 0.2)
timeline_plot
# Plot scatter points at zero and date
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=2.5)

# Don't show axes, appropriately position legend
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "bottom"
)

# Show text for each task
timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=task),size=3)
print(timeline_plot)

