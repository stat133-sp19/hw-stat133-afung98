#Title: Shots Code Script
#Description: Codes used to make shots-data.csv
#inputs:andre-iguodala.csv, draymond-green.csv, kevin-durant.csv, klay-thompson.csv, stephen-curry.csv
#outputs: shots-data.csv

#reading each raw data and adding player name column
curry <- read.csv("~/workout01/data/stephen-curry.csv", sep = ",", stringsAsFactors = FALSE, header = TRUE)
library(dplyr)
curry <- mutate(curry, name = "Stephen Curry")
thompson <- mutate(read.csv("~/workout01/data/klay-thompson.csv", sep = ",", stringsAsFactors = FALSE, header = TRUE), name = "Klay Thompton")
durant <- mutate(read.csv("~/workout01/data/kevin-durant.csv", sep = ",", stringsAsFactors = FALSE, header = TRUE), name = "Kevin Durant")
green <- mutate(read.csv("~/workout01/data/draymond-green.csv", sep = ",", stringsAsFactors = FALSE, header = TRUE), name = "Draymond Green")
iguodala <- mutate(read.csv("~/workout01/data/andre-iguodala.csv", sep = ",", stringsAsFactors = FALSE, header = TRUE), name = "Andre Iguodala")

#changing original values of shot_made_flag
curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"
curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- "shot_yes"
durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"
durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"
green$shot_made_flag[green$shot_made_flag == "n"] <- "shot_no"
green$shot_made_flag[green$shot_made_flag == "y"] <- "shot_yes"
iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"

#adding minute column to each
curry <- mutate(curry, minute = ((curry$period * 12) - curry$minutes_remaining))
thompson <- mutate(thompson, minute = ((thompson$period * 12) - thompson$minutes_remaining))
durant <- mutate(durant, minute = ((durant$period * 12) - durant$minutes_remaining))
green <- mutate(green, minute = ((green$period * 12) - green$minutes_remaining))
iguodala <- mutate(iguodala, minute = ((iguodala$period * 12) - iguodala$minutes_remaining))

#sending each data frame to output folder
sink(file = "~/workout01/output/stephen-curry-summary.txt")
summary(curry)
sink()
sink(file = "~/workout01/output/klay-thompson-summary.txt")
summary(thompson)
sink()
sink(file = "~/workout01/output/kevin-durant-summary.txt")
summary(durant)
sink()
sink(file = "~/workout01/output/draymond-green-summary.txt")
summary(green)
sink()
sink(file = "~/workout01/output/andre-iguodala-summary.txt")
summary(iguodala)
sink()

#binding data frames
bind_dataframe <- rbind(curry, thompson, durant, green, iguodala, stringsAsFactors = FALSE)

#export to csv inside data folder
write.csv(bind_dataframe, file = "~/workout01/data/shots-data.csv")

#saving summary of bind_dataframe to output folder
sink(file = "~/workout01/output/shots-data-summary.txt")
summary(bind_dataframe)
sink()

