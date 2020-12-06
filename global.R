library(shiny)
library(tidyverse)


crime <- read_csv("CrimeStatebyState.csv")


state_crime <- filter(crime,State != "United States-Total")

northeast_states<- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania")
midwest_states <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota")
south_states <- c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "District of Columbia", "West Virginia", "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas")
west_states <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington")

state_crime <- mutate(state_crime, region=ifelse(State %in% northeast_states, "Northeast",
                                                 ifelse(State %in% midwest_states, "Midwest",
                                                        ifelse(State %in% south_states, "South",
                                                               ifelse(State %in% west_states, "West","NA")))))
regional_rates <- summarize(group_by(state_crime, region,Year),
                            violent_rate = sum(`Violent crime total`)/sum(Population)*100000,
                            murder_rate = sum(`Murder and nonnegligent Manslaughter`)/sum(Population)*100000,
                            rape_rate = sum(`Legacy rape /1`)/sum(Population)*100000,
                            assault_rate = sum(`Aggravated assault`)/sum(Population)*100000,
                            robbery_rate = sum(`Robbery`)/sum(Population)*100000)