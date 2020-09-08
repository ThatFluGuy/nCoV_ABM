### Program description #######################################################
# Program name: Compile_Google_Mobility_Data.R                                #
# Program author: Mike Jackson                                                #
# Project: nCoV_ABM                                                           #
#_____________________________________________________________________________#
# Input datasets: Downloads the current version of Google's mobility data:    #
# https://www.google.com/covid19/mobility/. Data are archived and an archived #
# version can be used instead.                                                #
#_____________________________________________________________________________#
# Output datasets: #
#_____________________________________________________________________________#
# Program description:                                                        #
# This program takes cell phone mobility data compiled by Google and converts #
# it to a format that can be used in the ABM. Mobility is divided into        #
# workplace vs. community. Steps in this program:                             #
# (1) Set up options and directories                                          #
# (2) Import and clean mobility data                                          #
#_____________________________________________________________________________#

library(data.table)
library(dplyr)

### (1) Set up options and directories ########################################

get.fresh <- TRUE         # Download new data from Google's community mobility website?
use.recent <- TRUE        # If not get.fresh, use most recent download?
use.date <- "2020-09-06"  # Specify date of preferred mobility dataset

path <- "C:/Users/O992928/Documents/nCoV_ABM/"

### (2) Import and clean mobility data ########################################

# (A) Get preferred version of the mobility data
# If downloading new data, save a copy named with today's date

if(get.fresh==TRUE){
  goog <- fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=724703ce99ccb906")
  filename <- paste("Mobility", Sys.Date(), sep="-")
  saveRDS(goog, file=paste0(path, "Compiled_Data/Google/", filename))
} else {
  files.v <- list.files(path=paste0(path, "Compiled_Data/Google"))
  dates.v <- as.Date(substr(files.v, 10, 19), format="%Y-%m-%d")
  
  if (use.recent==TRUE){
    goog <- readRDS(file=paste0(path, "Compiled_Data/Google/", files.v[dates.v==max(dates.v)]))
  } else {
    goog <- readRDS(file=paste0(path, "Compiled_Data/Google/Mobility-", use.date))
  }
  
}

# (B) Data cleaning: Restrict to King County, Washington
# Rename the desired mobility variables and restrict to needed variables
king <- goog %>% 
  filter(country_region=="United States", sub_region_1 == "Washington",
         sub_region_2=="King County") %>%
  rename(workplace = workplaces_percent_change_from_baseline,
         retail = retail_and_recreation_percent_change_from_baseline,
         grocery = grocery_and_pharmacy_percent_change_from_baseline,
         residential = residential_percent_change_from_baseline) %>%
  select(date, workplace, retail, grocery, residential)

# Convert date from character to Date
king$date <- as.Date(king$date, format="%Y-%m-%d")


### (3) Format for GAMA #######################################################
# For travel to workplaces, use workplace mobility. Only include weekdays and #
# compute weekly average mobility decline relative to pre-pandemic baseline.  #
# For travel to community sites, use average of retail and (-)residential.    #
# Set mobility differences to zero for February.                              #

# A. Workplace weekly average travel
# workplace.wk gets the averge for each week, starting with Monday Feb 17 2020
# By setting weekends to NA and a 5 day forward window, only the Mondays are not NA
workplace.day <- ifelse(weekdays(king$date) %in% c("Saturday", "Sunday"), NA,
                    king$workplace)
workplace.wk <- frollmean(x=workplace.day, n=5, align="left")
workplace.wk <- workplace.wk[is.na(workplace.wk)==FALSE]

# B. Retail weekly average travel
retail.day <- frollmean(x=king$retail, n=7, align="left", na.rm=TRUE)
retail.wk <- retail.day[weekdays(king$date) == "Monday"]

# C. Residential weekly average time
residential.day <- frollmean(x=king$residential, n=7, align="left", na.rm=TRUE)
residential.wk <- residential.day[weekdays(king$date) == "Monday"]

# D. Combine Retail and Residential
x <- data.frame(retail.wk, -residential.wk)
community.wk <- apply(X=x, MARGIN=1, FUN=mean)

# E. Create output file
mobility.df <- data.frame(date=seq.Date(from=as.Date("2020-02-17"), by=7, length=length(workplace.wk)),
                          workplace=workplace.wk,
                          community=community.wk)

dates.df <- data.frame(date=seq.Date(from=as.Date("2020-01-27"), by=7, length=5))

mobility.df <- full_join(dates.df, mobility.df,  by="date")

mobility.df$workplace[mobility.df$date < as.Date("2020-03-01")] <- 0
mobility.df$community[mobility.df$date < as.Date("2020-03-01")] <- 0




