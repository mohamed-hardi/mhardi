#Using ggplot library
library(ggplot2)

setwd("~/GitHub/mhardi")

# Load required libraries
require(data.table)
# library(sqldf)
library(dplyr)
library(DT)
library(rCharts)

# Read data
data <- fread("./data/sets.csv")
head(data)
setnames(data, "t1", "theme")
setnames(data, "descr", "name")
setnames(data, "set_id", "setId")
# data$miniFigure <- as.numeric(data$theme=="Collectible Minifigures")
# Exploratory data analysis
sum(is.na(data)) # 0
length(unique(data$setId)) # 9944
table(data$year) # 1950 - 2015
length(table(data$year)) # 64
years <- sort(unique(data$year))
length(table(data$theme)) # 98
themes <- sort(unique(data$theme))
# sqldf("SELECT distinct year FROM data") 


## Helper functions

#' Aggregate dataset by year
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minPiece
#' @param maxPiece
#' @param themes
#' @return data.table
#'
groupByYearPiece <- function(dt, minYear, maxYear, minPiece,
                             maxPiece, themes) {
  result <- dt %>% filter(year >= minYear, year <= maxYear,
                          pieces >= minPiece, pieces <= maxPiece,
                          theme %in% themes) 
  return(result)
}

#' Aggregate dataset by themes
#' 
#' @param dt data.table
#' @param minYear
#' @param maxYear
#' @param minPiece
#' @param maxPiece
#' @param themes
#' @return result data.table
#' 
groupByTheme <- function(dt, minYear, maxYear, 
                         minPiece, maxPiece, themes) {
  # use pipelining
  # print(dim(dt))
  dt <- groupByYearPiece(dt, minYear, maxYear, minPiece,
                         maxPiece, themes) 
  # print(dim(result))
  result <- datatable(dt, options = list(iDisplayLength = 50))
  return(result)
  # The following does not work
  #     fn$sqldf("SELECT * FROM data 
  #          WHERE year >= $minYear and year <= $maxYear
  #          and theme in $themes")
  
  #return(data.table(result))
}

