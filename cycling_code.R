
# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)

# Import Files ------------------------------------------------------------

cycling <- read_xlsx("C:/Users/jason/OneDrive/Documents/2022_CyclingLog (2).xlsx", sheet = "Cycling")



# Data Wrangling ----------------------------------------------------------

cycling$Week <- week(cycling$Date)
cycling$Year <- year(cycling$Date)

cycling_weekly <- cycling %>%
  group_by(Year, Week, Bike, Ride) %>%
  summarize(Avg_Time = mean(`Time...8`),
            Avg_Calories = mean(Calories),
            Avg_Distance = mean(Miles),
            Avg_Temp = mean(Temp),
            Avg_Humidity = mean(Humidity),
            Avg_Wind = mean(Wind),
            Total_Time = sum(`Time...8`),
            Total_Calories = sum(Calories),
            Total_Distance = sum(Miles))


cycling_weekly$Week <- ifelse(cycling_weekly$Week %in% c(1:9), paste0(0, cycling_weekly$Week), cycling_weekly$Week)
cycling_weekly$Entry <- paste0(cycling_weekly$Year, "_", cycling_weekly$Week)


# Plot --------------------------------------------------------------------

plot_theme <-   theme(plot.background = element_rect(fill="white"))+
  theme(panel.background = element_rect(fill="white", colour="grey50"))+
  theme(plot.title = element_text(face = "bold", 
                                  size = 18,
                                  color="Navy"))+
  theme(axis.title = element_text(face = "bold", size = 16))+
  theme(axis.text.x = element_text(vjust = 1,
                                   hjust = 1,
                                   size = 12,
                                   angle = 60))

progress_plot <- ggplot(cycling_weekly, aes(x = Entry, y = Total_Distance, group = Bike, fill = Bike))+
  geom_bar(stat = "identity")+
  plot_theme

progress_plot

bikeSummary <- function(df, bike){
  df <- cycling %>%
    filter(Bike == bike) %>%
    group_by(Year, Week) %>%
    summarize(counts <- n(),
              Avg_Calories = mean(Calories),
              Avg_Distance = mean(Miles),
              Avg_Temp = mean(Temp),
              Avg_Humidity = mean(Humidity),
              Avg_Wind = mean(Wind),
              Total_Time = sum(`Time...8`),
              Total_Calories = sum(Calories),
              Total_Distance = sum(Miles))
  
  df$Week <- ifelse(df$Week %in% c(1:9), paste0(0, df$Week), df$Week)
  df$Entry <- paste0(df$Year, "_", df$Week)
  
  return(df)
  
}

bikePlot <- function(df, metric){
  
  plot <- ggplot(df, aes(x = Entry, y = !!sym(metric)))+
    geom_bar(stat = 'identity')+
    plot_theme
  
  return(plot)
  
}

bikeWrapper <- function(df, bike, metric){
  
  df <- bikeSummary(df, bike)
  plot <- bikePlot(df, metric)
  return(plot)
  
}

bikeWrapper(df, "Peloton", "Total_Time")


