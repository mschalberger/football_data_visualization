#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(rsconnect)

library(worldfootballR)
library(understatr)
library(ggsoccer)

library(dplyr)
library(ggplot2)
library(stringi)
library(ggtext)
library(forcats)
library(stringr)
library(TTR)
library(wesanderson)
library(patchwork)
library(plotly)
library(glue)

mapped_players <- player_dictionary_mapping()
mapped_players$PlayerFBref = stri_trans_general(str = mapped_players$PlayerFBref, id = "Latin-ASCII")

ex_player <- mapped_players%>%
  filter(mapped_players$PlayerFBref == "Lionel Messi")

example <- fb_player_scouting_report(ex_player$UrlFBref, pos_versus = "primary")

stats <- unique(example$Statistic)

players_total <- read.csv("https://raw.githubusercontent.com/mschalberger/understat_mapping/main/understat_playerid.csv")

fbref_plot <- function(data1, template, season1, plot, compare, data2=data1, season2 ) {
  
  data1 <- data1 %>%
    filter(str_detect(scouting_period, season1))
  
  data2 <- data2 %>%
    filter(str_detect(scouting_period, season2))
  
  if (template == "forward" || template == "") {
    
    if (nrow(data1) > 148) {
      
      data1$no <- 1:nrow(data1)
      data2$no <- 1:nrow(data2)
      data_selected1 <- data1[c(3, 8, 13, 24, 42, 128, 45, 115, 133, 107, 101, 102, 26, 147), ]
      data_selected2 <- data2[c(3, 8, 13, 24, 42, 128, 45, 115, 133, 107, 101, 102, 26, 147), ]
      data_selected1 <- data_selected1 %>%
        mutate(stat = case_when(Statistic == "Non-Penalty Goals" |
                                  Statistic == "xG" |
                                  Statistic == "Shots Total" |
                                  Statistic == "Non-Penalty Goals - npxG" |
                                  Statistic == "npxG/Shot" ~ "Attacking",
                                Statistic == "xA" |
                                  Statistic == "Miscontrols" |
                                  Statistic == "Passes into Penalty Area" |
                                  Statistic == "Touches (Att Pen)" |
                                  Statistic == "Progressive Passes Rec" ~ "Possession",
                                TRUE ~ "Defending"))
      
      data_selected1$stat[4] <- "Attacking"
    } else {
      
      data1$no <- 1:nrow(data1)
      data2$no <- 1:nrow(data2)
      data_selected1 <- data1[c(3, 8, 13, 24, 41, 127, 44, 114, 132, 106, 100, 101, 25, 146), ]
      data_selected2 <- data2[c(3, 8, 13, 24, 41, 127, 44, 114, 132, 106, 100, 101, 25, 146), ]
      data_selected1 <- data_selected1 %>%
        mutate(stat = case_when(Statistic == "Non-Penalty Goals" |
                                  Statistic == "xG" |
                                  Statistic == "Shots Total" |
                                  Statistic == "Non-Penalty Goals - npxG" |
                                  Statistic == "npxG/Shot" ~ "Attacking",
                                Statistic == "xA" |
                                  Statistic == "Miscontrols" |
                                  Statistic == "Passes into Penalty Area" |
                                  Statistic == "Touches (Att Pen)" |
                                  Statistic == "Progressive Passes Rec" ~ "Possession",
                                TRUE ~ "Defending"))
      
      data_selected1$stat[4] <- "Attacking"
    }
  } else if (template == "midfielder") {
    
    if (nrow(data1) > 148) {
      
      data1$no <- 1:nrow(data1)
      data2$no <- 1:nrow(data2)
      data_selected1 <- data1[c(3, 9, 10, 13, 53, 44, 47, 116, 125, 133, 146, 147, 107, 96), ]
      data_selected2 <- data2[c(3, 9, 10, 13, 53, 44, 47, 116, 125, 133, 146, 147, 107, 96), ]
      data_selected1 <- data_selected1 %>%
        mutate(stat = case_when(Statistic == "npxG + xA" |
                                  Statistic == "Average Shot Distance" |
                                  Statistic == "Shots Total" |
                                  Statistic == "Non-Penalty Goals" ~ "Attacking",
                                Statistic == "Progressive Passes" |
                                  Statistic == "Passes Under Pressure" |
                                  Statistic == "Passes into Final Third" |
                                  Statistic == "Touches (Live-Ball)" |
                                  Statistic == "Progressive Carries" |
                                  Statistic == "Progressive Passes Rec" ~ "Possession",
                                TRUE ~ "Defending"))
      
      data_selected1$stat[2] <- "Attacking"
      data_selected1$stat[3] <- "Attacking"
    } else {
      
      data1$no <- 1:nrow(data1)
      data2$no <- 1:nrow(data2)
      data_selected1 <- data1[c(3, 9, 10, 13, 52, 43, 46, 115, 124, 132, 145, 146, 106, 97), ]
      data_selected2 <- data2[c(3, 9, 10, 13, 52, 43, 46, 115, 124, 132, 145, 146, 106, 97), ]
      data_selected1 <- data_selected1 %>%
        mutate(stat = case_when(Statistic == "npxG + xA" |
                                  Statistic == "Average Shot Distance" |
                                  Statistic == "Shots Total" |
                                  Statistic == "Non-Penalty Goals" ~ "Attacking",
                                Statistic == "Progressive Passes" |
                                  Statistic == "Passes Under Pressure" |
                                  Statistic == "Passes into Final Third" |
                                  Statistic == "Touches (Live-Ball)" |
                                  Statistic == "Progressive Carries" |
                                  Statistic == "Progressive Passes Rec" ~ "Possession",
                                TRUE ~ "Defending"))
      
      data_selected1$stat[2] <- "Attacking"
      data_selected1$stat[3] <- "Attacking"
    }
  } else if (template == "defender") {
    
    if (nrow(data1) > 148) {
      
      data1$no <- 1:nrow(data1)
      data2$no <- 1:nrow(data2)
      data_selected1 <- data1[c(3, 11, 13, 44, 47, 129, 125, 110, 88, 96, 102, 106, 147, 108), ]
      data_selected2 <- data2[c(3, 11, 13, 44, 47, 129, 125, 110, 88, 96, 102, 106, 147, 108), ]
      data_selected1 <- data_selected1 %>%
        mutate(stat = case_when(Statistic == "Non-Penalty Goals" |
                                  Statistic == "npxG + xA" |
                                  Statistic == "Shots Total" ~ "Attacking",
                                Statistic == "Passes into Final Third" |
                                  Statistic == "Progressive Passes" |
                                  Statistic == "Progressive Carries" |
                                  Statistic == "Touches" |
                                  Statistic == "Dispossessed" ~ "Possession",
                                TRUE ~ "Defending"))
      
      data_selected1$stat[2] <- "Attacking"
    } else {
      
      data1$no <- 1:nrow(data1)
      data2$no <- 1:nrow(data2)
      data_selected1 <- data1[c(3, 11, 13, 43, 46, 128, 124, 109, 87, 95, 101, 105, 146, 107), ]
      data_selected2 <- data2[c(3, 11, 13, 43, 46, 128, 124, 109, 87, 95, 101, 105, 146, 107), ]
      data_selected1 <- data_selected1 %>%
        mutate(stat = case_when(Statistic == "Non-Penalty Goals" |
                                  Statistic == "npxG + xA" |
                                  Statistic == "Shots Total" ~ "Attacking",
                                Statistic == "Passes into Final Third" |
                                  Statistic == "Progressive Passes" |
                                  Statistic == "Progressive Carries" |
                                  Statistic == "Touches" |
                                  Statistic == "Dispossessed" ~ "Possession",
                                TRUE ~ "Defending"))
      
      data_selected1$stat[2] <- "Attacking"
    }
  } else if (template == "full back") {
    
    if (nrow(data1) > 148) {
      
      data1$no <- 1:nrow(data1)
      data2$no <- 1:nrow(data2)
      data_selected1 <- data1[c(3, 9, 10, 13, 114, 46, 47, 125, 43, 44, 147, 96, 107, 102), ]
      data_selected2 <- data2[c(3, 9, 10, 13, 114, 46, 47, 125, 43, 44, 147, 96, 107, 102), ]
      data_selected1 <- data_selected1 %>%
        mutate(stat = case_when(Statistic == "Non-Penalty Goals" |
                                  Statistic == "npxG" |
                                  Statistic == "xA" |
                                  Statistic == "Shots Total" ~ "Attacking",
                                Statistic == "Passes into Final Third" |
                                  Statistic == "Progressive Passes" |
                                  Statistic == "Progressive Carries" |
                                  Statistic == "Touches (Att 3rd)" |
                                  Statistic == "Crosses into Penalty Area" |
                                  Statistic == "Key Passes" ~ "Possession",
                                TRUE ~ "Defending"))
      
      data_selected1$stat[3] <- "Attacking"
    } else {
      
      data1$no <- 1:nrow(data1)
      data2$no <- 1:nrow(data2)
      data_selected1 <- data1[c(3, 9, 10, 13, 113, 45, 46, 124, 42, 43, 146, 95, 106, 101), ]
      data_selected2 <- data2[c(3, 9, 10, 13, 113, 45, 46, 124, 42, 43, 146, 95, 106, 101), ]
      data_selected1 <- data_selected1 %>%
        mutate(stat = case_when(Statistic == "Non-Penalty Goals" |
                                  Statistic == "npxG" |
                                  Statistic == "xA" |
                                  Statistic == "Shots Total" ~ "Attacking",
                                Statistic == "Passes into Final Third" |
                                  Statistic == "Progressive Passes" |
                                  Statistic == "Progressive Carries" |
                                  Statistic == "Touches (Att 3rd)" |
                                  Statistic == "Crosses into Penalty Area" |
                                  Statistic == "Key Passes" ~ "Possession",
                                TRUE ~ "Defending"))
    }
  } else if (template == "winger") {
    
    if (nrow(data1) > 148) {
      
      data1$no <- 1:nrow(data1)
      data2$no <- 1:nrow(data2)
      data_selected1 <- data1[c(3, 22, 24, 42, 143, 45, 119, 47, 124, 133, 107, 146, 101, 102), ]
      data_selected2 <- data2[c(3, 22, 24, 42, 143, 45, 119, 47, 124, 133, 107, 146, 101, 102), ]
      data_selected1 <- data_selected1 %>%
        mutate(stat = case_when(Statistic == "Non-Penalty Goals" |
                                  Statistic == "xG" |
                                  Statistic == "xA" |
                                  Statistic == "Penalty Kicks Won" |
                                  Statistic == "npxG/Shot" ~ "Attacking",
                                Statistic == "Progressive Carrying Distance" |
                                  Statistic == "Successful Dribble %" |
                                  Statistic == "Progressive Passes" |
                                  Statistic == "Passes into Penalty Area" |
                                  Statistic == "Progressive Passes Rec" ~ "Possession",
                                TRUE ~ "Defending"))
      
      data_selected1$stat[3] <- "Attacking"
    } else {
      
      data1$no <- 1:nrow(data1)
      data2$no <- 1:nrow(data2)
      data_selected1 <- data1[c(3, 21, 23, 41, 142, 44, 118, 46, 123, 132, 106, 145, 100, 101), ]
      data_selected2 <- data2[c(3, 21, 23, 41, 142, 44, 118, 46, 123, 132, 106, 145, 100, 101), ]
      data_selected1 <- data_selected1 %>%
        mutate(stat = case_when(Statistic == "Non-Penalty Goals" |
                                  Statistic == "xG" |
                                  Statistic == "xA" |
                                  Statistic == "Penalty Kicks Won" |
                                  Statistic == "npxG/Shot" ~ "Attacking",
                                Statistic == "Progressive Carrying Distance" |
                                  Statistic == "Successful Dribble %" |
                                  Statistic == "Progressive Passes" |
                                  Statistic == "Passes into Penalty Area" |
                                  Statistic == "Progressive Passes Rec" ~ "Possession",
                                TRUE ~ "Defending"))
      
      data_selected1$stat[3] <- "Attacking"
    }
  } else if (template == "goalkeeper") {
    
    if (nrow(data1) > 36) {
      
      data1$no <- 1:nrow(data1)
      data2$no <- 1:nrow(data2)
      data_selected1 <- data1[c(4, 20, 22, 24, 26, 29, 35, 36, 37), ]
      data_selected2 <- data2[c(4, 20, 22, 24, 26, 29, 35, 36, 37), ]
      data_selected1 <- data_selected1 %>%
        mutate(stat = case_when(Statistic == "Save%" |
                                  Statistic == "PSxG" |
                                  Statistic == "PSxG-GA" ~ "Defending",
                                Statistic == "Passes Attempted (Launched)" |
                                  Statistic == "Passes Attempted" |
                                  Statistic == "Average Pass Length" ~ "Possession",
                                TRUE ~ "Attacking"))
    } else {
      
      data1$no <- 1:nrow(data1)
      data2$no <- 1:nrow(data2)
      data_selected1 <- data1[c(4, 19, 21, 23, 25, 28, 34, 35, 36), ]
      data_selected2 <- data2[c(4, 19, 21, 23, 25, 28, 34, 35, 36), ]
      data_selected1 <- data_selected1 %>%
        mutate(stat = case_when(Statistic == "Save%" |
                                  Statistic == "PSxG" |
                                  Statistic == "PSxG-GA" ~ "Defending",
                                Statistic == "Passes Attempted (Launched)" |
                                  Statistic == "Passes Attempted" |
                                  Statistic == "Average Pass Length" ~ "Possession",
                                TRUE ~ "Attacking"))
    }
  }  else if (template == "custom") {
    data_selected2 <- data2
    data_selected1 <- data1 %>% 
      mutate(stat = case_when(
        StatGroup == "Standard" ~ "Attacking", 
        StatGroup == "Shooting" ~ "Attacking",
        StatGroup == "Passing" ~ "Possession",
        StatGroup == "Pass Types" ~ "Possession",
        StatGroup == "Goal and Shot Creation" ~ "Possession",
        StatGroup == "Defense" ~ "Defending",
        StatGroup == "Possession" ~ "Possession",
        StatGroup == "Miscellaneous Stats" ~ "Defending",
        TRUE ~ NA_character_
      ))
  }
  
  temp <- (360 / (length(data_selected1$Player)) / 2)
  myAng <- seq(-temp, -360 + temp, length.out = length(data_selected1$Player))
  ang <- ifelse(myAng < -90, myAng + 180, myAng)
  ang <- ifelse(ang < -90, ang + 180, ang)
  
  if (compare == FALSE &
      plot == "pizza"){
    
    player_name <- data_selected1$Player
    title <- paste(player_name, "Percentile Chart")
    min <- data_selected1$BasedOnMinutes
    sub <- data_selected1$Versus
    sub1 <- data_selected1$scouting_period
    subtitle <- paste("Compared to", sub, " |", sub1, " |", min, "min")
    
    x <- c(data_selected1$Statistic, data_selected1$stat)
    
    ggplot(data_selected1, aes(fct_reorder(Statistic, stat), Percentile)) +
      geom_bar(aes(y = 100, fill = stat), stat = "identity", width = 1, colour = "white",
               alpha = 0.1, show.legend = FALSE) +
      geom_bar(stat = "identity", width = 1, aes(fill = stat), colour = "white", alpha = 1) +
      coord_polar(clip = "off") +
      geom_hline(yintercept = 25, colour = "white", linetype = "dashed", alpha = 0.8) +
      geom_hline(yintercept = 50, colour = "white", linetype = "dashed", alpha = 0.8) +
      geom_hline(yintercept = 75, colour = "white", linetype = "dashed", alpha = 0.8) +
      geom_hline(yintercept = 100, colour = "white", linetype = "dashed", alpha = 0.8) +
      scale_fill_manual(values = c("Possession" = "#41ab5d",
                                   "Attacking" = "#fec44f",
                                   "Defending" = "#de2d26")) +
      geom_label(aes(y = 105, label = Per90, fill = stat), size = 3, color = "white", nudge_x= -0.1,show.legend = FALSE) +
      geom_label(aes(y = Percentile, label = paste(Percentile, "%") , fill = stat), size = 2, color = "black",nudge_x = 0.1, show.legend = FALSE) +
      scale_y_continuous(limits = c(-20, 115)) +
      labs(fill = "",
           title = title,
           subtitle = subtitle) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", color = "white"),
            panel.background = element_rect(fill = "white", color = "white"),
            legend.position = "bottom",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12, colour = "black", angle = ang),
            text = element_text(colour = "black", size = 20),
            plot.title = element_markdown(hjust = 0.5, size = 26, colour = "black", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 20, colour = "black"),
            plot.caption = element_text(hjust = 0.5, size = 15, colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    
    
  } else if (compare == TRUE &
             plot == "pizza"){
    
    player_name1 <- data_selected1$Player
    player_name2 <- data_selected2$Player
    min1 <- data_selected1$BasedOnMinutes
    min2 <- data_selected2$BasedOnMinutes
    sub1 <- data_selected1$Versus
    sub2 <- data_selected2$Versus
    lg1 <- data_selected1$scouting_period
    lg2 <- data_selected2$scouting_period
    title <- paste(player_name1, " |", lg1, " |", min1, "min")
    subtitle <- paste(player_name2, " |", lg2, " |", min2, "min")
    caption <- paste("Compared to", sub1, "and", sub2, "respectively")
    
    x <- data_selected1$Statistic
    
    ggplot(data_selected1, aes(x = fct_reorder(data_selected1$Statistic, data_selected1$stat), y = Percentile)) +
      geom_bar(aes(y = 100), fill = "white", stat = "identity", width = 1, colour = "white",
               alpha = 0.1, show.legend = FALSE) +
      geom_bar(data = data_selected1, aes(y = Percentile, fill = "#f7a258"), colour = "#f7a258", stat = "identity", width = 1, alpha = 1) +
      scale_fill_manual(values = c("#f7a258","#346888")) +
      geom_bar(data = data_selected2, aes(y = Percentile, fill = NA), stat = "identity", width = 1, alpha = 0, colour = "#346888", size = 3) +
      coord_polar(clip = "off") +
      geom_hline(yintercept = 25, colour = "black", linetype = "dashed", alpha = 0.7) +
      geom_hline(yintercept = 50, colour = "black", linetype = "dashed", alpha = 0.7) +
      geom_hline(yintercept = 75, colour = "black", linetype = "dashed", alpha = 0.7) +
      geom_hline(yintercept = 100, colour = "black", linetype = "dashed", alpha = 0.7) +
      geom_label(data = data_selected1 , nudge_x = 0.17,aes(y = 100, label = Per90), size = 3, fill = "white", colour = "#f7a258", show.legend = FALSE) + 
      geom_label(data = data_selected2 , nudge_x = -0.17, aes(y = 100, label = Per90), size = 3, fill = "white", colour = "#346888", show.legend = FALSE) + 
      scale_y_continuous(limits = c(-20, 110)) +
      labs(caption = caption,
           title = title,
           subtitle = subtitle) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", color = "white"),
            panel.background = element_rect(fill = "white", color = "white"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 12, colour = "black", angle = ang),
            text = element_text(colour = "black", size = 20),
            plot.title = element_markdown(hjust = 0.5, size = 26, colour = "#f7a258", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 26, colour = "#346888", face = "bold"),
            plot.caption = element_text(hjust = 0.5, size = 15, colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    
  } else if (compare == FALSE &
             plot == "pyramid"){

    player_name <- data_selected1$Player
    title <- paste(player_name, "Percentile Chart")
    min <- data_selected1$BasedOnMinutes
    sub <- data_selected1$Versus
    sub1 <- data_selected1$scouting_period
    subtitle <- paste("Compared to", sub, " |", sub1, " |", min, "min")
    
    x <- c(data_selected1$Statistic, data_selected1$stat)
    
    ggplot(data_selected1, aes(fct_reorder(Statistic, stat), Percentile)) +
      geom_bar(aes(y = 100, fill = stat), stat = "identity", width = 1, colour = "white",
               alpha = 0.1, show.legend = FALSE) +
      geom_bar(stat = "identity", width = 1, aes(fill = stat), colour = "white", alpha = 1) +
      scale_fill_manual(values = c("Possession" = "#41ab5d",
                                   "Attacking" = "#fec44f",
                                   "Defending" = "#de2d26")) +
      coord_flip()+
      geom_label(aes(y = -5, label = Per90, fill = stat), size = 3, color = "white", show.legend = FALSE) +
      geom_label(aes(y = Percentile, label = paste(Percentile, "%") , fill = stat), size = 2, color = "black", nudge_y = 3,show.legend = FALSE) +
      scale_y_continuous(limits = c(-5, 110), breaks = c(0,25,50,75,100), labels = c("0","25","50","75","100")) + 
      labs(fill = "",
           title = title,
           subtitle = subtitle) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", color = "white"),
            panel.background = element_rect(fill = "white", color = "white"),
            legend.position = "bottom",
            axis.title.y = element_blank(),
            text = element_text(colour = "black", size = 20),
            plot.title = element_markdown(hjust = 0.5, size = 26, colour = "black", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 20, colour = "black"),
            plot.caption = element_text(hjust = 0.5, size = 15, colour = "black"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank()) 
    
    
  } else if (compare == TRUE &
             plot == "pyramid") {
    
    player_name1 <- data_selected1$Player
    player_name2 <- data_selected2$Player
    min1 <- data_selected1$BasedOnMinutes
    min2 <- data_selected2$BasedOnMinutes
    sub1 <- data_selected1$Versus
    sub2 <- data_selected2$Versus
    lg1 <- data_selected1$scouting_period
    lg2 <- data_selected2$scouting_period
    title <- paste(player_name1, " |", lg1, " |", min1, "min")
    subtitle <- paste(player_name2, " |", lg2, " |", min2, "min")
    caption <- paste("Compared to", sub1, "and", sub2, "respectively")
    
    ggplot(data_selected1, aes(x = Statistic, y = Percentile, fill=Player)) +
      geom_bar(data = data_selected1, aes(y = Percentile*-1), stat = "identity",fill = "#f7a258", colour = "#f7a258", show.legend = FALSE) + 
      geom_bar(data = data_selected2, aes(y = Percentile), stat = "identity", fill = "#346888", colour = "#346888", show.legend = FALSE) +
      geom_label(data = data_selected1 ,aes(y = -110, label = Per90), size = 3, fill = "#f7a258", colour = "white", show.legend = FALSE) +
      geom_label(data = data_selected2 ,aes(y = 110, label = Per90), size = 3, fill = "#346888", colour = "white", show.legend = FALSE) +
      coord_flip() +
      theme_minimal()+
      labs(caption = caption,
           title = title,
           subtitle = subtitle) +
      scale_y_continuous(limits = c(-110, 110), breaks = c(-100,-75,-50,-25,0,25,50,75,100), labels = c("100","75","50","25","0","25","50","75","100")) +
      theme(plot.background = element_rect(fill = "white", color = "white"),
            panel.background = element_rect(fill = "white", color = "white"),
            panel.grid.major.y =  element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.y = element_blank(),
            text = element_text(colour = "black", size = 20),
            plot.title = element_markdown(hjust = 0.5, size = 26, colour = "#f7a258", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 26, colour = "#346888", face = "bold"),
            plot.caption = element_text(hjust = 0.5, size = 15, colour = "black"),)
  }
}


# User Interface (Frontend)
ui <- fluidPage(theme = shinytheme("lumen"),
                navbarPage("Football Data Visualization",
                           tabPanel("Fbref",
                                    sidebarPanel(
                                      pickerInput('name', 'Player Name', as.list(mapped_players[,1]), options=pickerOptions(liveSearch=T), selected = "Cristiano Ronaldo"),
                                      materialSwitch('compare','Compare?', value = F, right = T),
                                      conditionalPanel(condition = "input.compare == 1",
                                                       pickerInput('name2', 'Compare to', as.list(mapped_players[,1]), options=pickerOptions(liveSearch=T), selected = "Lionel Messi")),
                                      pickerInput('type', 'Stat Type', choices = c("forward", "winger", "midfielder", "defender", "goalkeeper", "full back", "custom"), options=pickerOptions(liveSearch=F)),
                                      conditionalPanel(condition = "input.type == 'custom'",
                                                       pickerInput('custom', 'Custom Stats', choices = stats, options=pickerOptions(liveSearch=T), multiple = T)),
                                      pickerInput("year", "Season:", choices = c("Last 365 Days", "2021-2022", "2020-2021", "2019-2020", "2018-2019", "2017-2018"), options=pickerOptions(liveSearch=F)),
                                      conditionalPanel(condition ="input.compare == 1",
                                                       pickerInput("year2", "Season for Comparison:", choices = c("Last 365 Days", "2021-2022", "2020-2021", "2019-2020", "2018-2019", "2017-2018"), options=pickerOptions(liveSearch=F))),
                                      radioGroupButtons("plot", "Type of Plot", choices = c("pizza", "pyramid" )),
                                      downloadButton("download", "Download Plot")
                                    ),
                                    mainPanel(plotOutput("plot"))  
                            ),
                            tabPanel("xG",
                                    sidebarPanel(
                                       pickerInput("player2", "Player:", as.list(players_total[,2]), options=pickerOptions(liveSearch=T),selected = "Cristiano Ronaldo"),
                                       sliderInput("range", "Year:",
                                                   min = 2014, max = 2021,
                                                   value = c(2014, 2021),
                                                   sep = ""),
                                       downloadButton("download2", "Download Plot")
                                      ),
                                    mainPanel(plotOutput("plot2")))
                )
)

# Server (Backend)
server <- function(input, output, session) {
  
  plotInput <- reactive({
    
    player <- reactive({
      mapped_players%>%
        filter(mapped_players$PlayerFBref == input$name)
    })
    
    player2 <- reactive({
      mapped_players%>%
        filter(mapped_players$PlayerFBref == input$name2)
    })
    
    dat <- reactive({
      if (input$type == "custom"){
        fb_player_scouting_report(player_url = player()$UrlFBref , pos_versus = "primary") %>%
          filter(Statistic %in% input$custom)
      } else{
        fb_player_scouting_report(player_url = player()$UrlFBref , pos_versus = "primary")
      }
    }) 
    
    dat2 <- reactive({
      if (input$type == "custom"){
        fb_player_scouting_report(player_url = player2()$UrlFBref , pos_versus = "primary") %>%
          filter(Statistic %in% input$custom)
      } else{
        fb_player_scouting_report(player_url = player2()$UrlFBref , pos_versus = "primary")
      }
    })
    
    if(nrow(dat()) == 0) {
      stop("No data available for chosen filters! Please choose diffferent custom stats")
    } else if(nrow(dat2()) == 0 &
              input$compare == TRUE){
      stop("No data available for chosen filters! Please choose diffferent custom stats")
    } else if (nrow(dat()%>%
                    filter(str_detect(scouting_period, input$year))) == 0) {
      stop("No data available for chosen filters! Please choose a different player or season!")
    } else if ((nrow(dat2()%>%
                     filter(str_detect(scouting_period, input$year))) == 0) &
               input$compare == TRUE){
      stop("No data available for chosen filters! Please choose a different player or season!")
    } else {  
      fbref_plot(data1 = dat(), template = input$type, season1 = input$year,plot = input$plot, compare = input$compare, data2 = dat2(), season2 = input$year2)
    }
  })
  
  plotInput2 <- reactive({
    
    player_id <- reactive({
      players_total%>%
        filter(player_name == input$player2)
    })
    
    
     dataset <- reactive({
       get_player_shots(as.numeric(player_id()[,1]))
       })
     
    
     data <- reactive({
       dataset() %>%
       mutate(X = X * 120,
              Y = Y * 80) %>%
       mutate(result = ifelse(result == "Goal", "Goal", "No Goal")) %>%
       mutate(isGoal = ifelse(result == "Goal", 1, 0)) %>%
       mutate(GxG = isGoal - xG) %>%
       mutate(GxGSM = SMA(GxG, n = 50)) %>%
       mutate(date = as.Date(date)) %>%
        filter(year >= input$range[1] & year <= input$range[2])
     })
     test_data <- data()
     
     g1 <- ggplot(data(), aes(x = date, y = GxGSM, colour = GxGSM)) +
       geom_line(size = 2) + 
       #geom_point(size = 3) + 
       scale_color_gradientn(colours = wes_palette("Zissou1", 21, type = "continuous")) +
       geom_hline(yintercept = 0, size = 1, colour = "black", linetype = "longdash") +
       labs(title = input$player2, y = "G - xG", subtitle = paste(sum(data()$isGoal) , "Goals with" , round(sum(data()$xG)) , "xG from" , nrow(data()) , "Shots.")) +
       theme_minimal() +
       theme(legend.position = "none")
     
  
    
     g2 <- ggplot() +
       annotate_pitch(dimensions = pitch_statsbomb, fill = "#212121", colour = "white") +
       coord_flip(xlim = c(60,120),
                  ylim = c(80, -2)) +
       theme_pitch() +
       geom_point(data = data(), aes(x = X, y = Y, fill = result, size = result), colour = "white", shape = 21, show.legend = FALSE) +
       scale_size_manual(values = c(3,1.5)) +
       scale_fill_manual(values = c("#E1AF00","#78B7C5")) +
       #labs(caption = paste(sum(data()$isGoal) , "Goals with" , round(sum(data()$xG)) , "xG from" , nrow(data()) , "Shots.")) +
       theme_minimal() +
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks.x = element_blank(),
             axis.ticks.y = element_blank(),
             aspect.ratio = 0.5) 
    
     g3 <- ggplot() +
       geom_histogram(data = data(), aes(x = xG, fill = result), bins = 20, position = position_stack(reverse = TRUE)) +
       scale_fill_manual(values = c("#E1AF00","#78B7C5")) +
       labs(x = "Individual Shot xG",
            y = "Frequency") +
       geom_vline(xintercept = mean(data()$xG), colour = "white", size = 0.5, linetype = "longdash") +
       #annotate(geom = "text", x = mean(hist_data()$xG) + 0.02, y = 50, label = glue("xG/Shot=", round(mean(hist_data()$xG), 2)), colour = "white", size = 4) +
       theme_minimal() +
       theme(legend.position = c(0.9, 0.9),
             legend.title = element_blank())
     
     g4 <- ggplot(data = data(), aes(x=as.factor(shotType), y=GxGSM, fill = shotType))+
       geom_hline(yintercept = 0, size = 1, colour = "black", linetype = "dashed") +
       geom_boxplot()+
       scale_fill_manual(values = wes_palette("Zissou1"))+
       theme_minimal()+
       labs(y = "G - xG")+
       theme(
             axis.title.y = element_text(size = 8),
             axis.title.x = element_blank(),
             legend.position = "none"
             ) 
     
     g5 <- ggplot(data = data(), aes(x=as.factor(situation), y=GxGSM, fill = situation))+
       geom_hline(yintercept = 0, size = 1, colour = "black", linetype = "dashed") +
       geom_boxplot()+
       scale_fill_manual(values = wes_palette("Zissou1"))+
       theme_minimal()+
       labs(y = "G - xG")+
       theme(
         axis.title.y = element_text(size = 8),
         axis.title.x = element_blank(),
         legend.position = "none"
       ) 
     
     
     
    g1 / (g2 | g3 ) / (g4 | g5)
     
  })
  
  output$plot <- renderPlot({
    plotInput()
  }, height = 600, width = 850)
  
  output$plot2 <- renderPlot({
    plotInput2()
  }, height = 600)
  
  output$download <- downloadHandler(
    filename = function() { paste(input$name, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png", width = 12, height = 8)
    }
  )
  
   output$download2 <- downloadHandler(
     filename = function() { paste(input$player2, '.png', sep='') },
     content = function(file) {
       ggsave(file, plot = plotInput2(), device = "png")
     }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

