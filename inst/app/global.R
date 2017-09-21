if (!require("shiny"))
  install.packages("shiny", repos = "http://cran.ma.imperial.ac.uk/")
if (!require("shinydashboard"))
  install.packages("shinydashboard", repos = "http://cran.ma.imperial.ac.uk/")
if (!require("shinyjs"))
  install.packages("shinyjs", repos = "http://cran.ma.imperial.ac.uk/")
if (!require("RSQLite"))
  install.packages("RSQLite", repos = "http://cran.ma.imperial.ac.uk/")
if (!require("DBI"))
  install.packages("DBI", repos = "http://cran.ma.imperial.ac.uk/")
if (!require("plyr"))
  install.packages("plyr", repos = "http://cran.ma.imperial.ac.uk/")
if (!require("dplyr"))
  install.packages("dplyr", repos = "http://cran.ma.imperial.ac.uk/")
if (!require("DT")) # download button extention available in development version only
  devtools::install_github('rstudio/DT')
if (!require("lubridate"))
  install.packages("lubridate", repos = "http://cran.ma.imperial.ac.uk/")
if (!require("shinyBS"))
  install.packages("shinyBS", repos = "http://cran.ma.imperial.ac.uk/")
if (!require("tidyr"))
  install.packages("tidyr", repos = "http://cran.ma.imperial.ac.uk/")





library(shiny)
library(shinydashboard)
library(shinyjs)
library(RSQLite)
library(DBI)
library(plyr)
library(dplyr)
library(DT)
library(lubridate)
library(shinyBS)
library(tidyr)
library(krakenApp) # To run the app in the inst directory


# Relative path to database in server
dbfile <- "/kraken/kraken2.db"

# Path to dummy database if the real does not exist
if(!file.exists(dbfile)) {
  dbfile <- system.file(package = "krakenApp", "app", "data", "kraken2.db") #kraken2_2012
}


# Path to the link files
# The actual link to the pdf file is made up of linkPath and the invoice number, and is created by createLink():
# e.g. "<a href='file:///C:/Projects/Kraken/Q2/MGB-EXT-1000001.pdf'>MGB/EXT/1000001</a>"
linkPath <- "file://filestore/mangowork/tmp/kraken/FY%2015-16"

source("helpers.R", local = TRUE)

## TERRIBLE LOADING OF DATA FILES FROM HDD WILL REMOVE

skills <-  read.csv("../../../dataFiles/skills.csv",
                    stringsAsFactors = FALSE,
                    header = FALSE)

skills <- skills$V1


skillsScores <- read.csv("../../../dataFiles/dummySkillsnew.csv",
                         stringsAsFactors = FALSE,
                         encoding = "UTF-8")
