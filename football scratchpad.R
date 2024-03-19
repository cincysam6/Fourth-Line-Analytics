library(nflfastR)
library(RPostgres)
library(dplyr)
library(tidyverse)
library(nflreadr)
library(gganimate)
library(ggplot2)
library(gt)
library(gtExtras)
library(tidyr)
library(patchwork)
library(magrittr)
### This is to build my Database. One time data pull


# Function to convert time in the format "minutes:seconds" to total seconds
convert_to_seconds <- function(time_str) {
  parts <- strsplit(time_str, ":")
  if (length(parts[[1]]) == 2) {
    minutes <- as.numeric(parts[[1]][1])
    seconds <- as.numeric(parts[[1]][2])
    total_seconds <- minutes * 60 + seconds
    return(total_seconds)
  } else {
    return(NA)  # Invalid format
  }
}


## gg_field function - set up as a list of annotations
gg_field <- function(yardmin=0, yardmax=120, buffer=5, direction="horiz",
                     field_color="white",line_color="gray",
                     sideline_color=field_color, endzone_color="white"){
  
  ## field dimensions (units=yards)
  xmin <- 0
  xmax <- 120
  
  ymin <- 0
  ymax <- 53.33
  
  
  ## distance from sideline to hash marks in middle (70 feet, 9 inches)
  hash_dist <- (70*12+9)/36
  
  ## yard lines locations (every 5 yards) 
  yd_lines <- seq(15,105,by=5)
  
  ## hash mark locations (left 1 yard line to right 1 yard line)
  yd_hash <- 11:109
  
  ## field number size
  num_size <- 5
  
  ## rotate field numbers with field direction
  ## first element is for right-side up numbers, second for upside-down
  angle_vec <- switch(direction, "horiz" = c(0, 180), "vert" = c(270, 90))
  num_adj <- switch(direction, "horiz" = c(-1, 1), "vert" = c(1, -1))
  
  ## list of annotated geoms
  p <- list(
    
    ## add field background 
    annotate("rect", xmin=xmin, xmax=xmax, ymin=ymin-buffer, ymax=ymax+buffer, 
             fill=field_color),
    
    ## add end zones
    annotate("rect", xmin=xmin, xmax=xmin+10, ymin=ymin, ymax=ymax, fill=endzone_color),
    annotate("rect", xmin=xmax-10, xmax=xmax, ymin=ymin, ymax=ymax, fill=endzone_color),
    
    ## add yardlines every 5 yards
    annotate("segment", x=yd_lines, y=ymin, xend=yd_lines, yend=ymax,
             col=line_color),
    
    ## add thicker lines for endzones, midfield, and sidelines
    annotate("segment",x=c(0,10,60,110,120), y=ymin, xend=c(0,10,60,110,120), yend=ymax,
             lwd=1.3, col=line_color),
    annotate("segment",x=0, y=c(ymin, ymax), xend=120, yend=c(ymin, ymax),
             lwd=1.3, col=line_color) ,
    
    ## add field numbers (every 10 yards)
    ## field numbers are split up into digits and zeros to avoid being covered by yard lines
    ## numbers are added separately to allow for flexible ggplot stuff like facetting
    
    ## 0
    annotate("text",x=seq(20,100,by=10) + num_adj[2], y=ymin+12, label=0, angle=angle_vec[1],
             col=line_color, size=num_size),
    
    ## 1
    annotate("text",label=1,x=c(20,100) + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ## 2
    annotate("text",label=2,x=c(30,90) + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ## 3
    annotate("text",label=3,x=c(40,80) + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ## 4
    annotate("text",label=4,x=c(50,70) + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    ## 5
    annotate("text",label=5,x=60 + num_adj[1], y=ymin+12, angle=angle_vec[1],
             colour=line_color, size=num_size),
    
    
    ## upside-down numbers for top of field
    
    ## 0
    annotate("text",x=seq(20,100,by=10) + num_adj[1], y=ymax-12, angle=angle_vec[2],
             label=0, col=line_color, size=num_size),
    ## 1
    annotate("text",label=1,x=c(20,100) + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ## 2
    annotate("text",label=2,x=c(30,90) + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ## 3
    annotate("text",label=3,x=c(40,80) + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ## 4
    annotate("text",label=4,x=c(50,70) + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    ## 5
    annotate("text",label=5,x=60 + num_adj[2], y=ymax-12, angle=angle_vec[2],
             colour=line_color, size=num_size),
    
    
    ## add hash marks - middle of field
    annotate("segment", x=yd_hash, y=hash_dist - 0.5, xend=yd_hash, yend=hash_dist + 0.5,
             color=line_color),
    annotate("segment", x=yd_hash, y=ymax - hash_dist - 0.5, 
             xend=yd_hash, yend=ymax - hash_dist + 0.5,color=line_color),
    
    ## add hash marks - sidelines
    annotate("segment", x=yd_hash, y=ymax, xend=yd_hash, yend=ymax-1, color=line_color),
    annotate("segment", x=yd_hash, y=ymin, xend=yd_hash, yend=ymin+1, color=line_color),
    
    ## add conversion lines at 2-yard line
    annotate("segment",x=12, y=(ymax-1)/2, xend=12, yend=(ymax+1)/2, color=line_color),
    annotate("segment",x=108, y=(ymax-1)/2, xend=108, yend=(ymax+1)/2, color=line_color),
    
    ## cover up lines outside of field with sideline_color
    annotate("rect", xmin=0, xmax=xmax, ymin=ymax, ymax=ymax+buffer, fill=sideline_color),
    annotate("rect",xmin=0, xmax=xmax, ymin=ymin-buffer, ymax=ymin, fill=sideline_color),
    
    ## remove axis labels and tick marks
    labs(x="", y=""),
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
          axis.ticks = element_blank()),
    
    ## clip axes to view of field
    if(direction=="horiz"){
      coord_cartesian(xlim=c(yardmin, yardmax), ylim = c(ymin-buffer,ymax+buffer), 
                      expand = FALSE)
      
    } else if (direction=="vert"){
      ## flip entire plot to vertical orientation
      coord_flip(xlim=c(yardmin, yardmax), ylim = c(ymin-buffer,ymax+buffer), expand = FALSE)
      
    }
  )
  
  return(p)
  
}

pbp_data<-load_pbp(seasons=2023)

participation<-load_participation(seasons=2023)
player_stats<-load_player_stats(seasons=2023)
depth_charts<-load_depth_charts(seasons=2023)
ng_stats<-load_nextgen_stats(seasons=2023)
injuries<-load_injuries(seasons=2023)
schedules<-load_schedules(seasons=2023)
teams<-load_teams()
rosters<-load_rosters(seasons=2023)


snap_counts<-load_snap_counts(seasons=2023)
ftn_data<-load_ftn_charting(seasons=2023)
contracts<-load_contracts()
weekly_rosters<-load_rosters_weekly(season=2023)

str(ftn_data)
# Replace these placeholders with your actual PostgreSQL credentials
db_host <- "localhost"
db_name <- "NFL_data"
db_user <- "postgres"
db_password <- "Cincinnati1"

# Construct the connection string
con_string <- paste0("host=", db_host,
                     " dbname=", db_name,
                     " user=", db_user,
                     " password=", db_password)

# Establish the connection
con <- dbConnect(RPostgres::Postgres(), dbname = db_name, user = db_user,
                 password = db_password, host = db_host)




# Name of the table to be created in the PostgreSQL database
#table_name <- "contracts"
#data_to_write<-contracts
# Write the data to the PostgreSQL table
#dbWriteTable(con, table_name, data_to_write, overwrite = TRUE)



# Generate the table name for the specific year
#table_name <- "pbp_data"

# Fetch data from the table for the current year
#query <- paste0("SELECT * FROM ", table_name)
#data <- dbGetQuery(con, query)


### Build a Drive Chart

gameid = '2023_03_LA_CIN'
pos_team = 'CIN'

drive_data<-pbp_data%>%
  filter((game_id == gameid) & (posteam==pos_team) & ( !is.na(drive)))%>%left_join(teams_colors_logos,by=c("posteam"="team_abbr"))%>%
  mutate(posteam_drive_num = dense_rank(drive),
         drive_time_of_possession_seconds = sapply(drive_time_of_possession, convert_to_seconds))

drive_data%>%View()

drive_data%>%select(play_id,desc,posteam_drive_num,pass,rush,special,drive,play_type)%>%filter(drive==11)%>%View()

game_data<-pbp_data%>% filter((game_id == gameid) & (posteam==pos_team) & ( !is.na(drive)))%>%
  left_join(teams_colors_logos,by=c("posteam"="team_abbr"))%>%
  mutate(drive_time_of_possession_seconds = sapply(drive_time_of_possession, convert_to_seconds))

drive_summaries<-game_data%>%
  filter(game_id == gameid & pos_team==posteam & play_type!='extra_point')%>%
  select(posteam,
         drive,
         qtr,
         yards_gained,
         air_yards,
         yards_after_catch,
         posteam_score,
         defteam_score,
         epa,
         wp,
         rush_attempt,
         pass_attempt,
         fixed_drive_result,
         drive_play_count,
         drive_time_of_possession,
         drive_time_of_possession_seconds,
         drive_first_downs,
         drive_start_yard_line,
         drive_end_yard_line,
         drive_ended_with_score,
         drive_yards_penalized,
         drive_start_transition,
         drive_end_transition,
         team_wordmark,
         team_logo_espn,
         play_type)%>%
  group_by(posteam,
           drive,
           qtr,
           posteam_score,
           defteam_score,
           fixed_drive_result,
           drive_play_count,
           drive_time_of_possession,
           drive_time_of_possession_seconds,
           drive_first_downs,
           drive_start_yard_line,
           drive_end_yard_line,
           drive_ended_with_score,
           drive_yards_penalized,
           drive_start_transition,
           drive_end_transition,
           team_wordmark,
           team_logo_espn)%>%
  mutate(yards_gained = sum(yards_gained,na.rm = TRUE),
         air_yards = sum(air_yards,na.rm = TRUE),
         yac = sum(yards_after_catch,na.rm = TRUE),
         rush_attempts = sum(rush_attempt,na.rm = TRUE),
         pass_attempts = sum(pass_attempt,na.rm = TRUE),
         drive_epa = round(sum(epa,na.rm = TRUE),1),
         epa_per_play = round(drive_epa/drive_play_count,2),
         pass_epa = round(mean(epa[play_type == "pass"], na.rm = TRUE),2),
         run_epa = round(mean(epa[play_type == "run"], na.rm = TRUE),2),
         pass_rate = round(pass_attempts/drive_play_count,3),
         success_rate = round((sum(epa > 0)/drive_play_count),3),
         wp_difference = round(first(wp) - last(wp) ,3),
         wp_start = round(first(wp),3),
         wp_end = round(last(wp),3),
         pace_of_play = round(drive_time_of_possession_seconds/drive_play_count,1))%>%mutate(pass_epa = replace(pass_epa,is.na(pass_epa),0),
                                                                                             run_epa = replace(run_epa,is.na(run_epa),0))%>%select(-play_type,-rush_attempt,-pass_attempt,-yards_after_catch,-epa,-wp)%>%ungroup()%>%distinct()

drive_summaries<-drive_summaries%>%mutate(drive_num = row_number())
  
drive_summaries%>%View()


#### NEED TO FIX DATA ERROR
### NEED TO MAKE EPA PLOT LOOK BETTER
### NEED TO MAKE LABELS FOR EPA PLOT LOOK BETTER
#### NEED TO MAKE THIS SO POSTEAM IS A VARIABLE FOR GAME ID AND IN DRIVE CHART PLOTS



drive_summaries%>%
  filter(drive_num==1)%>%
  pivot_longer(cols=-posteam,names_to="Variable",values_to="Value",values_transform = list(Value = as.character))%>%
  gt()


plot_title<-as.data.frame(drive_summaries%>%select(drive_num,
                                                   posteam_score,
                                                   defteam_score,
                                     team_wordmark,
                                     drive_start_yard_line,
                                     drive_end_yard_line,
                                     drive_time_of_possession,
                                     drive_start_transition,
                                     drive_end_transition,
                                     fixed_drive_result,
                                     drive_play_count,
                                     pass_rate,
                                     success_rate,
                                     wp_start,
                                     wp_end,
                                     wp_difference,
                                     drive_epa,
                                     epa_per_play,
                                     pass_epa,
                                     run_epa,
                                     yards_gained,
                                     yac,
                                     air_yards,
                                     team_logo_espn,
                                     pace_of_play))%>%filter(drive_num==1)

browns_wordmark<-teams_colors_logos%>%filter(team_abbr=='BAL')%>%select(team_wordmark)





plot_title_table<-plot_title%>%select(drive_start_yard_line,
                                      drive_end_yard_line,
                                      drive_time_of_possession,
                                      drive_start_transition,
                                      drive_end_transition,
                                      yards_gained,
                                      pass_rate,
                                      success_rate,
                                      drive_epa,
                                      epa_per_play,
                                      pass_epa,
                                      run_epa,
                                      wp_difference,
                                      wp_start,
                                      wp_end,
                                      yac,
                                      air_yards,
                                      pace_of_play)%>%gt()%>%
  gt_theme_espn()%>%
  fmt_percent(  # Format the PassRate column as a percentage
    columns = pass_rate,
    decimals = 1  # Specify the number of decimal places
  )%>%
  fmt_percent(  # Format the PassRate column as a percentage
    columns = success_rate,
    decimals = 1  # Specify the number of decimal places
  )%>%
  fmt_percent(  # Format the PassRate column as a percentage
    columns = wp_start,
    decimals = 1  # Specify the number of decimal places
  )%>%
  fmt_percent(  # Format the PassRate column as a percentage
    columns = wp_end,
    decimals = 1  # Specify the number of decimal places
  )%>%
  fmt_percent(  # Format the PassRate column as a percentage
    columns = wp_difference,
    decimals = 1  # Specify the number of decimal places
  )%>%  data_color(
    columns = vars(wp_difference,drive_epa,epa_per_play,pass_epa,run_epa),
    colors = scales::col_bin(
      bins = c(-Inf, 0, Inf),
      palette = c("#ffb6c1", "#b4eeb4"),
    )
  )%>%
  tab_header(title = htmltools::HTML(paste0("<span style='vertical-align: center; font-size: 48px;'> ",plot_title$posteam_score,"</span>",web_image(url = plot_title$team_wordmark,height=px(75)),"<span style='vertical-align: top; font-size: 36px;'> VS.</span>",web_image(url = browns_wordmark$team_wordmark,height=px(75)), "<span style='font-size: 36px;'>","</span>"  ),"<span style='vertical-align: center; font-size: 48px;'> ",plot_title$defteam_score,"</span>"),subtitle = htmltools::HTML(paste0("<span style='font-size: 18px;'>","<b>Possession Team:</b> ",web_image(url = plot_title$team_logo_espn,height=px(35)),"  Drive Number: ","<b>",plot_title$drive_num)," of ",max(drive_summaries$drive_num),"</b>","Drive Outcome: ","<b>",plot_title$fixed_drive_result,"     </b>","     Play Count: ","<b>",plot_title$drive_play_count,"</b>","</span>"))%>%
  cols_label(drive_start_transition = "Drive Starts With",
             drive_end_transition = "Drive Ends With",
             drive_time_of_possession = "Time of Poss",
             drive_start_yard_line="Drive Start",
             drive_end_yard_line = "Drive End",
             yards_gained = "Yards Gained",
             pass_rate = "Pass Rate",
             success_rate = "Success Rate",
             drive_epa = "Drive EPA",
             epa_per_play = "EPA/Play",
             pass_epa = "Pass EPA/Play",
             run_epa = "Run EPA/Play",
             wp_start = "Win Prob - Drive Start",
             wp_end = "Win Prob - Drive End",
             wp_difference = "WP Difference",
             air_yards = "Air Yards",
             yac = "YAC",
             pace_of_play = "Pace/Play (sec)") %>%opt_align_table_header(align = "center")

plot_title_table



drive_1_data<-drive_data%>%filter(posteam_drive_num==4 & (play_type == "pass" | play_type == "run"))%>%
  select(drive,
         fumble_lost,
         drive_play_count,
         drive_time_of_possession,
         drive_first_downs,
         fixed_drive_result,
         drive_start_transition,
         drive_end_transition,
         down,
         ydstogo,
         incomplete_pass,
         air_yards,
         interception,
         epa,
         yards_gained,
         play_type,
         series_result,
         yardline_100,
         desc,
         posteam)%>%
  mutate(play_num = as.factor(row_number()),
          y_pos =(53-as.numeric(play_num)*4),
         chart_yd = ifelse(incomplete_pass==1,air_yards,yards_gained),
         xend = yardline_100-chart_yd,
         play_type = ifelse(fumble_lost == 1 | interception == 1, "turnover", play_type))


team_colors <-c('#FB4F14','#000000','#d32f1e')


epa_plot<-ggplot(data = drive_1_data, aes(x=play_num,y=epa,fill=play_type)) +
  geom_col() + 
  scale_fill_manual(values = team_colors)+
  geom_text(data = drive_1_data, aes(x=play_num,y=epa, label = round(epa,2),color = factor(sign(epa))), 
            vjust = 1, size = 5) + 
  scale_color_manual(values = c("red", "black")) +
  theme_bw() + theme(legend.position = "none",# Hide panel borders and remove grid lines
                                         panel.border = element_blank(),
                                         panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank()) + labs(x="Play Number", caption ="data from nflfastR  Twitter: @cincysam6") + 
  geom_hline(yintercept=0) + 
  expand_limits(y = c(min(drive_1_data$epa) - 1, max(drive_1_data$epa) + 1))

epa_plot 


#This is good, but save it for something with more data points 


#library(ggpubr)
#library(ggthemes)

#epa_plot<-ggdotchart(drive_1_data, x="play_num",y="epa",
#           color = "play_type",                                # Color by groups
#           palette = team_colors, # Custom color palette
#           add = "segments",
#           group = "play_num", # Add segments from y = 0 to dots
#           add.params = list(color = "lightgray", size = 2), # Change segment color and size                                # Order by groups
#           dot.size = 10,                                 # Large dot size
#           label = round(drive_1_data$epa,2),                        # Add mpg values as dot labels
#           font.label = list(color = "white", size = 8, 
##                             vjust = 0.5),               # Adjust label parameters
#           ggtheme = theme_pubr()                        # ggplot2 theme
#)+
#  geom_hline(yintercept = 0, linetype = 2, color = "lightgray") + theme(legend.position = "none") + ylim(-7,7)






drive_plot <- ggplot() + 
  
  # Creating field underlay
  gg_field(yardmin = 00, yardmax = 120) +
  
  geom_point(data = drive_1_data, aes(x = yardline_100, y = y_pos, color = play_type), size = 3) +
  
  geom_segment(data = drive_1_data[drive_1_data$incomplete_pass == 0,], 
               aes(x = yardline_100, xend = xend, y = y_pos, yend = y_pos, color = play_type), 
               size = 1.5) +
  
  geom_segment(data = drive_1_data[drive_1_data$incomplete_pass == 1,], 
               aes(x = yardline_100, xend = xend, y = y_pos, yend = y_pos, color = play_type), 
               size = 0.75, linetype = "dashed") +
  
  # Adding labels to points using the 'desc' column
  geom_text(data = drive_1_data, aes(x = yardline_100, y = y_pos, label = desc), 
            vjust = -1, size = 3) +
  
  scale_color_manual(values = team_colors) +  # Use the custom color palette
  
  # Filling forest green for behind the back of the endzone
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        # Move the legend to the bottom and make it horizontal
        legend.position = c(0.5, 0.1),  # Adjust the legend position
        legend.direction = "horizontal",
        plot.caption = element_text(face = "italic")) +
# Plot Title (initial title)
 labs(caption = "dashed line indicates incomplete pass")

drive_plot

#tmp <- tempfile(fileext = '.png') #generate path to temp .png file

#tmp<-"table.png"
#gt::gtsave(tmp,plot_title_table) #save gt table as png
#table_png <- png::readPNG(tmp, native = TRUE) # read tmp png file

png_filename <- "temp_table.png"
gtsave(plot_title_table, filename = png_filename, vwidth = 1500, vheight = 1000)

# Read the saved PNG file back into R
library(png)
table_png <- readPNG(png_filename, native = TRUE)



drive_chart<-(ggplot() + theme_void())+ table_png + drive_plot + epa_plot + plot_layout(ncol = 1, nrow = 4,heights = c(0,1,2.5,1),widths = c(1,1,0.8,0.8))

drive_chart









dir.create(game_id, showWarnings = FALSE)  # Use showWarnings to suppress warnings if the folder already exists


# Specify the file path to save the ggplot in the new folder
file_path <- file.path(game_id,paste0("drive_chart_", 1, ".png"))  # Replace "my_plot.png" with your desired file name and extension

# Save the ggplot to the folder
ggsave(file_path, plot = drive_chart) 
































drive_plot<-
  ggplot() + 
  
  #creating field underlay
  gg_field(yardmin = 00, yardmax = 120) +
  
  
  geom_point(data = drive_1_data, aes(x = yardline_100,
                                      y = y_pos,color=play_type),size=3) +
  
  geom_segment(data = drive_1_data[drive_1_data$incomplete_pass == 0,], 
               aes(x = yardline_100, xend = xend, y = y_pos, yend = y_pos, color = play_type), 
               size = 1.5) +
  
  geom_segment(data = drive_1_data[drive_1_data$incomplete_pass == 1,], 
               aes(x = yardline_100, xend = xend, y = y_pos, yend = y_pos, color = play_type), 
               size = 0.75, linetype = "dashed") +
  
  # Adding labels to points using the 'desc' column
  geom_text(data = drive_1_data, aes(x = yardline_100, y = y_pos, label = desc), 
            vjust = -1, size = 3) +  # Adjust the vjust and hjust for label positioning
  
  #filling forest green for behind back of endzone
  theme(panel.background = element_rect(fill = "white",
                                        color = "white"),
        panel.grid = element_blank(),
        # Move the legend to the bottom and make it horizontal
        legend.position = "bottom",  # Adjust the legend position
        legend.direction = "horizontal") +
  # Plot Title (initial title)
  labs(title = paste0("Drive Result: ",drive_1_data$fixed_drive_result," Play Count: ",drive_1_data$drive_play_count), caption = "data from nflfastR") +
  
  
  
  transition_states(as.integer(play_num),wrap=FALSE,state_length=10) +
  shadow_mark() 


























plot_title_table<-plot_title%>%select(drive_start_yard_line,
                                      drive_end_yard_line,
                                      drive_time_of_possession,
                                      drive_start_transition,
                                      drive_end_transition,
                                      drive_play_count,
                                      pass_rate,
                                      pass_success_rate,
                                      run_success_rate,
                                      drive_epa,
                                      epa_per_play,
                                      pass_epa,
                                      run_epa,
                                      wp_difference,
                                      wp_start,
                                      wp_end,
                                      yards_gained,
                                      penalty_yards,
                                      yac,
                                      air_yards,
                                      pace_of_play)%>%mutate(drive_end_transition = map(drive_end_transition,add_drive_color),
                                                             drive_start_transition = map(drive_start_transition,add_drive_color))%>%gt()%>%
  gt_theme_espn()%>%
  fmt_percent(  # Format the PassRate column as a percentage
    columns = pass_rate,
    decimals = 1  # Specify the number of decimal places
  )%>%
  fmt_percent(  # Format the PassRate column as a percentage
    columns = pass_success_rate,
    decimals = 1  # Specify the number of decimal places
  )%>%
  fmt_percent(  # Format the PassRate column as a percentage
    columns = run_success_rate,
    decimals = 1  # Specify the number of decimal places
  )%>%
  fmt_percent(  # Format the PassRate column as a percentage
    columns = wp_start,
    decimals = 1  # Specify the number of decimal places
  )%>%
  fmt_percent(  # Format the PassRate column as a percentage
    columns = wp_end,
    decimals = 1  # Specify the number of decimal places
  )%>%
  fmt_percent(  # Format the PassRate column as a percentage
    columns = wp_difference,
    decimals = 1  # Specify the number of decimal places
  )%>%  data_color(
    columns = vars(wp_difference,drive_epa,epa_per_play,pass_epa,run_epa),
    colors = scales::col_bin(
      bins = c(-Inf, 0, Inf),
      palette = c("#ffb6c1", "#b4eeb4"),
    )
  )%>%tab_header(title = htmltools::HTML(paste0("<span style='vertical-align: center; font-size: 48px;'> ",plot_title$posteam_score_post,"</span>",web_image(url = plot_title$team_wordmark,height=px(75)),"<span style='vertical-align: top; font-size: 36px;'> VS.</span>",web_image(url = browns_wordmark$team_wordmark,height=px(75)), "<span style='font-size: 36px;'>","</span>"  ),"<span style='vertical-align: center; font-size: 48px;'> ",plot_title$defteam_score_post,"</span>"),subtitle = htmltools::HTML(paste0("<span style='font-size: 18px;'>","<b>Possession Team:</b> ",web_image(url = plot_title$team_logo_espn,height=px(35)),"  Drive Number: ","<b>",plot_title$drive_num)," of ",max(drive_summaries$drive_num),"</b>","Drive Outcome: ","<b>",plot_title$fixed_drive_result,"     </b>","</span>"))%>%
  cols_label(drive_start_transition = "Drive Starts With",
             drive_end_transition = "Drive Ends With",
             drive_play_count = "# Plays (Excl. Penalties)",
             drive_time_of_possession = "Time of Poss",
             drive_start_yard_line="Drive Start",
             drive_end_yard_line = "Drive End",
             yards_gained = "Yards Gained",
             pass_rate = "Pass Rate",
             pass_success_rate = "Pass",
             run_success_rate = "Run",
             drive_epa = "Total",
             epa_per_play = "Per Play",
             pass_epa = "Pass/Play",
             run_epa = "Run/Play",
             wp_start = "WP Start",
             wp_end = "WP End",
             wp_difference = "WP Diff",
             air_yards = "Air Yards",
             yac = "YAC",
             pace_of_play = "Pace/Play (sec)") %>%opt_align_table_header(align = "center")%>%
  tab_spanner(label = "Win Probability", columns=vars(wp_start,wp_end,wp_difference))%>%
  tab_spanner(label = "EPA", columns=vars(drive_epa,epa_per_play,pass_epa,run_epa))%>%
  tab_spanner(label = "Success Rate", columns=vars(pass_success_rate,run_success_rate))

