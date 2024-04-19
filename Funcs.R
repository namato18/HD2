library(httr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)

# planet_info = readRDS("dat/planet_info.rds")

base_url = "https://helldivers-2-dotnet.fly.dev/"

# URL of the API
WarID <- "https://helldivers-2-dotnet.fly.dev/raw/api/WarSeason/current/WarID"
WarStatus <- "https://helldivers-2-dotnet.fly.dev/raw/api/WarSeason/801/Status"
WarInfo <- "https://helldivers-2-dotnet.fly.dev/raw/api/WarSeason/801/WarInfo"
WarStatSummary <- "https://helldivers-2-dotnet.fly.dev/raw/api/Stats/war/801/summary"
NewsFeed <- "https://helldivers-2-dotnet.fly.dev/raw/api/NewsFeed/801"
WarAssignments <- "https://helldivers-2-dotnet.fly.dev/raw/api/v2/Assignment/War/801"
Planets <- "https://helldivers-2-dotnet.fly.dev/api/v1/planets"
Regen <-"https://helldivers-2-dotnet.fly.dev/api/v1/planets/199"

# Make the GET request
# response <- GET("https://helldivers-2-dotnet.fly.dev/api/v1/planets")
# 
# df = fromJSON(rawToChar(response$content))

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

GetCampaignInfo <- function(){
  
  Planets <- "https://helldivers-2-dotnet.fly.dev/api/v1/campaigns"
  
  # Make the GET request
  response <- GET(Planets)
  
  df = fromJSON(rawToChar(response$content))
  
  df_overview = df %>% unnest(planet) %>%
    unnest(statistics) %>%
    unnest(event, names_sep = "_") %>%
    unnest(biome, names_sep = "_") %>%
    arrange(desc(playerCount))
  
  df = df %>% unnest(planet) %>% unnest(statistics) %>% unnest(event, names_sep = "_") %>%
    arrange(desc(playerCount)) %>%
    filter(playerCount != 0)

  df$currentOwner[!is.na(df$event_id)] = df$event_faction[!is.na(df$event_id)]
  df$health[!is.na(df$event_id)] = df$event_health[!is.na(df$event_id)]
  df$maxHealth[!is.na(df$event_id)] = df$event_maxHealth[!is.na(df$event_id)]
  
  df = df %>%
    select(name, sector, maxHealth, health, currentOwner, playerCount)
  
  Percentage_Taken = paste0(round(100 - (df$health / df$maxHealth * 100), digits = 2)," %")
  
  df$Percentage_Taken = Percentage_Taken
  
  df = df %>% select(-health, -maxHealth)
  
  to.return = list(df_overview = df_overview,
                   df = df)
  
  return(to.return)
}

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

PlanetPopBar <- function(df){
  
  faction_colors = c("Automaton" = "#FF000099", "Terminids" = "#FFA50099")

  
  # df = df_planets
  df = df %>%
    head(5)
  
  plot_planets = ggplot(data = df, aes(x = name, y = playerCount, fill = currentOwner)) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values = faction_colors) +
    geom_text(aes(label = playerCount), vjust = 1.5, size = 4, color = "white") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
          panel.background = element_blank(),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 15),
          plot.title = element_text(size = 20),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          legend.position = "none",
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20)) +
    labs(title = "Player Population by Planet", x = "Planet", y = "Population")
  
  df_sector = df %>%
    group_by(sector) %>%
    summarize(sums = sum(playerCount))
  
  plot_sectors = ggplot(data = df_sector, aes(x = sector, y = sums, fill = sector)) + 
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank()) +
    labs(title = "Player Population by Sector", x = "Sector", y = "Population")
  
  df_faction = df %>%
    group_by(currentOwner) %>%
    summarize(sums = sum(playerCount))
  
  
  
  plot_factions = ggplot(data = df_faction, aes(x = currentOwner, y = sums, fill = currentOwner)) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values = faction_colors) +
    geom_text(aes(label = sums), vjust = 1.5, size = 4, color = "white") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
          panel.background = element_blank(),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 15),
          plot.title = element_text(size = 20),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          legend.position = "none",
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20)) +
    labs(title = "How Many Players Fighting Each Faction", x = "Faction", y = "Population")
  
  to.return = list(plot_planets = plot_planets,
                   plot_sectors = plot_sectors,
                   plot_factions = plot_factions)
  
  return(to.return)
  
}

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

GetPlanetHistory <- function(planet_id){
  # planet_id = 0
  
  url <- paste0("https://helldiverstrainingmanual.com/api/v1/war/history/",planet_id)
  
  # Make the GET request
  response <- GET(url)
  
  df = fromJSON(rawToChar(response$content))
  
  if(is.null(nrow(df))){
    print("no history found")
    return(NULL)
  }else{
    df$created_at = as_datetime(df$created_at)
    df$liberation = round((df$max_health - df$current_health) / df$max_health * 100, digits = 2)
    
    return(df)
  }
  

  
  
}

GetAllPlanets <- function(){
  
  
  url <- "https://helldivers-2-dotnet.fly.dev/api/v1/planets"
  
  # Make the GET request
  response <- GET(url)
  
  df = fromJSON(rawToChar(response$content))
  
  return(df)
}
