library(dplyr); library(magrittr); library(stringr); library(furrr); library(sf); library(move2); library(lubridate)

acc <- readRDS("data/raw/acc.rds") 
class(acc)

testACC <- accMasseur_eobsVult_updated(acc)
