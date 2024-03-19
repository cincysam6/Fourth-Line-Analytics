library(RPostgres)

# Replace these placeholders with your actual PostgreSQL credentials
db_host <- "localhost"
db_name <- "AFL_data"
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
table_name <- "test"

# Write the data to the PostgreSQL table
dbWriteTable(con, table_name, data_to_write, overwrite = TRUE)

# Close the connection
#dbDisconnect(con)

afl_player_stats_2021<-fetch_player_stats(season = 2015)

# Name of the table to be created in the PostgreSQL database
table_name <- "afl_player_stats_2013"

# Write the data to the PostgreSQL table
dbWriteTable(con, table_name, afl_player_stats_2013, overwrite = TRUE)


# Specify the range of years you want to process
start_year <- 2011
end_year <- 2023

# Loop through each year
for (year in seq(start_year, end_year)) {
  data <- fetch_ladder_squiggle(season = year)
  dbWriteTable(con, paste0("afl_ladder_squiggle_",year), data, overwrite = TRUE)
  cat("Data for year", year, "has been written to the database.\n")
}

# Close the database connection
dbDisconnect(con)




















### Scratch pad work 


# Replace these placeholders with your actual PostgreSQL credentials
db_host <- "localhost"
db_name <- "AFL_data"
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



# Years range (update this based on your data)
start_year <- 2017
end_year <- 2023

# Create an empty list to store data frames
data_list <- list()

# Loop through each year
for (year in start_year:end_year) {
  # Generate the table name for the specific year
  table_name <- paste0("afl_player_stats_", year)
  
  # Fetch data from the table for the current year
  query <- paste0("SELECT * FROM ", table_name)
  data <- dbGetQuery(con, query)
  
  # Append the data to the list
  data_list[[year - start_year + 1]] <- data
}

afl_player_stats <- do.call(rbind, data_list)



single_game_player<-afl_player_stats%>%filter(providerId=="CD_M20170140101" & player.player.player.playerId=="CD_I260278")


single_game_player %>%
  select(player.photoURL, player.player.position, player.player.player.surname,
         player.player.player.playerJumperNumber, kicks, handballs, disposals, marks) %>%
  gt() %>%
  gt_img_rows(columns = player.photoURL,img_source = "web") %>%
  tab_header(title = single_game_player$player.player.player.surname) %>%
  tab_stubhead(
    labels = list(
      as.character(tags$img(src = player.photoURL, width = 50, height = 50))
    ))%>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = c(player.player.player.surname)))













#### I NEED TO CLEAN THESE DUMB COLUMN NAMES


# Replace these placeholders with your actual PostgreSQL credentials
db_host <- "localhost"
db_name <- "AFL_data"
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



# Years range (update this based on your data)
start_year <- 2017
end_year <- 2023

# Create an empty list to store data frames
data_list <- list()

# Loop through each year
for (year in start_year:end_year) {
  # Generate the table name for the specific year
  table_name <- paste0("afl_player_stats_", year)
  
  # Fetch data from the table for the current year
  query <- paste0("SELECT * FROM ", table_name)
  data <- dbGetQuery(con, query)
  data$year <- year
  
  # Append the data to the list
  data_list[[year - start_year + 1]] <- data
}

afl_player_stats <- do.call(rbind, data_list)

afl_player_stats <-afl_player_stats%>%rename(playerID = player.player.player.playerId,
                          playerNumber = player.jumperNumber,
                          last_name = player.player.player.surname,
                          first_name = player.player.player.givenName,
                          position = player.player.position,
                          captain = player.player.player.captain,
                          photoURL = player.photoURL
                          )

afl_player_stats%>%filter(year==2022 & playerID == 'CD_I1008139')


afl_player_season_stats = afl_player_stats%>%select(first_name,
                                                    last_name,
                                                    team.name,
                                                    playerID,
                                                    year,
                                                    photoURL,
                                                    playerNumber,
                                                    timeOnGroundPercentage,
                                                    goals,
                                                    behinds,
                                                    kicks,
                                                    handballs,
                                                    disposals,
                                                    marks,
                                                    bounces,
                                                    tackles,
                                                    contestedPossessions,
                                                    uncontestedPossessions,
                                                    totalPossessions,
                                                    inside50s,
                                                    marksInside50,
                                                    contestedMarks,
                                                    hitouts,
                                                    clangers,
                                                    freesAgainst,
                                                    freesFor,
                                                    goalAssists,
                                                    turnovers,
                                                    intercepts,
                                                    tacklesInside50,
                                                    shotsAtGoal
                                                    )%>%
  mutate(minutes_played = 80*(timeOnGroundPercentage/100))%>%group_by(playerID,
                                                                 first_name,
                                                                 last_name,
                                                                 photoURL,
                                                                 playerNumber,
                                                                 team.name)%>%mutate(games_played = n(),
                                                                                minutes_played = sum(minutes_played),
                                                                                goals = sum(goals),
                                                                                 behinds = sum(behinds),
                                                                                 kicks = sum(kicks),
                                                                                 handballs = sum(handballs),
                                                                                 disposals = sum(disposals),
                                                                                 marks = sum(marks),
                                                                                 bounces = sum(bounces),
                                                                                 tackles = sum(tackles),
                                                                                 contestedPossessions = sum(contestedPossessions),
                                                                                 uncontestedPossessions = sum(uncontestedPossessions),
                                                                                 totalPossessions = sum(totalPossessions),
                                                                                 inside50s = sum(inside50s),
                                                                                 marksInside50 = sum(marksInside50),
                                                                                 contestedMarks = sum(contestedMarks),
                                                                                 hitouts = sum(hitouts),
                                                                                 clangers = sum(clangers),
                                                                                 freesAgainst = sum(freesAgainst),
                                                                                 freesFor = sum(freesFor),
                                                                                 goalAssists = sum(goalAssists),
                                                                                 turnovers = sum(turnovers),
                                                                                 intercepts = sum(intercepts),
                                                                                 tacklesInside50 = sum(tacklesInside50),
                                                                                 shotsAtGoal = sum(shotsAtGoal)
                                                                                )%>%select(-timeOnGroundPercentage)%>%ungroup()%>%distinct()

afl_player_season_stats<-afl_player_season_stats%>%mutate(goalsPer80 = (goals/minutes_played)*80,
                                                            behindsPer80 = (behinds/minutes_played)*80,
                                                            kicksPer80 = (kicks/minutes_played)*80,
                                                            handballsPer80 = (handballs/minutes_played)*80,
                                                            disposalsPer80 = (disposals/minutes_played)*80,
                                                            marksPer80 = (marks/minutes_played)*80,
                                                            bouncesPer80 = (bounces/minutes_played)*80,
                                                            tacklesPer80 = (tackles/minutes_played)*80,
                                                            contestedPossessionsPer80 = (contestedPossessions/minutes_played)*80,
                                                            uncontestedPossessionsPer80 = (uncontestedPossessions/minutes_played)*80,
                                                            totalPossessionsPer80 = (totalPossessions/minutes_played)*80,
                                                            inside50sPer80 = (inside50s/minutes_played)*80,
                                                            marksInside50Per80 = (marksInside50/minutes_played)*80,
                                                            contestedMarksPer80 = (contestedMarks/minutes_played)*80,
                                                            hitoutsPer80 = (hitouts/minutes_played)*80,
                                                            clangersPer80 = (clangers/minutes_played)*80,
                                                            freesAgainstPer80 = (freesAgainst/minutes_played)*80,
                                                            freesForPer80 = (freesFor/minutes_played)*80,
                                                            goalAssistsPer80 = (goalAssists/minutes_played)*80,
                                                            turnoversPer80 = (turnovers/minutes_played)*80,
                                                            interceptsPer80 = (intercepts/minutes_played)*80,
                                                            tacklesInside50Per80 = (tacklesInside50/minutes_played)*80,
                                                            shotsAtGoalPer80 = (shotsAtGoal/minutes_played)*80)


unique_years <- unique(afl_player_season_stats$year)

# Loop through each year
for (y in unique_years) {
  year_data <- afl_player_season_stats %>%
    filter(year == y)
  
  # Construct the table name
  table_name <- paste("afl_player_season_stats_", y, sep = "")
  
  # Write the data to a new table in PostgreSQL
  dbWriteTable(con, table_name, year_data, overwrite = TRUE)
  
}
  
  
  
  
  
  
  
  
  ### ADD PERCENTILE COLUMNS TO THIS DF 
  
  
  #### I NEED TO CLEAN THESE DUMB COLUMN NAMES
  # Replace these placeholders with your actual PostgreSQL credentials
  db_host <- "localhost"
  db_name <- "AFL_data"
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
  
  
  
  # Years range (update this based on your data)
  start_year <- 2017
  end_year <- 2023
  
  # Create an empty list to store data frames
  data_list <- list()
  
  # Loop through each year
  for (year in start_year:end_year) {
    # Generate the table name for the specific year
    table_name <- paste0("afl_player_season_stats_", year)
    
    # Fetch data from the table for the current year
    query <- paste0("SELECT * FROM ", table_name)
    data <- dbGetQuery(con, query)
    data$year <- year
    
    # Append the data to the list
    data_list[[year - start_year + 1]] <- data
  }
  
  afl_player_stats <- do.call(rbind, data_list)
  
  # Get unique years from the data
  unique_years <- unique(afl_player_stats$year)
  
  # Split the data by year into a list of data frames
  data_by_year <- split(afl_player_stats, afl_player_stats$year)
  
  percentile_ranks_by_year <- lapply(data_by_year, function(subset_data) {
    subset_data %>%
      group_by(year) %>%
      mutate(across(goals:shotsAtGoalPer80, ~ round((rank(.) / (n() + 1)) * 100, 0))) %>%
      ungroup()
  })
player_percentile_ranks_2017 <- percentile_ranks_by_year[[1]]
player_percentile_ranks_2018 <- percentile_ranks_by_year[[2]]
player_percentile_ranks_2019 <- percentile_ranks_by_year[[3]]
player_percentile_ranks_2020 <- percentile_ranks_by_year[[4]]

player_percentile_ranks_2021 <- percentile_ranks_by_year[[5]]

player_percentile_ranks_2022 <- percentile_ranks_by_year[[6]]
player_percentile_ranks_2023 <- percentile_ranks_by_year[[7]]


    table_name <- "player_percentile_ranks_2017"
    
    dbWriteTable(con, table_name, player_percentile_ranks_2017, overwrite = TRUE)

  
    
    
    # Construct the table name
    table_name <- "afl_player_info"
    
    # Write the data to a new table in PostgreSQL
    dbWriteTable(con, table_name, player_details, overwrite = TRUE)

    
    