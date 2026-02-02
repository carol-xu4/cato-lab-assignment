## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, readr)

# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Cato Lab Assignment")

# Read in data ------------------------------------------------------------
realincome = read.csv("data/input/MEFAINUSA672N.csv")
realgdp = read.csv("data/input/A939RX0Q048SBEA.csv")

# cleaning ----------------------------------------------------------------
realincome = realincome %>%
    rename(year = observation_date,
        income = MEFAINUSA672N)

realgdp = realgdp %>%
    rename( year = observation_date,
        gdp = A939RX0Q048SBEA)

# merging columns by date, 1953 - 2024
# NA rows were removed (february-december, and years < 1953), since income is annual and gdp was taken monthly
data = merge(realincome, realgdp, 
    by = "year", all = FALSE)

# indexing percent growth since 1953, since columns adjusted by different units
data = data %>%
    mutate(gdp_index = gdp / gdp[year == "1953-01-01"] * 100,
        income_index = income / income[year == "1953-01-01"] * 100)

# writing final dataset ----------------------------------------------------
write_csv(data, "data/output/finaldata.csv")
