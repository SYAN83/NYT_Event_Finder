library(RSQLite)
library(DBI)
source("helpers.R")
library(dplyr)
library(tidyr)

dbPath <- "nytimes.db"
tbName <- "nytevent"

events <- fromDB(dbPath, tbName)
days <- dayFreq(events)

event.day <- eventByDay(events, days)
