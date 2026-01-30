## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, readr)

# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Cato Lab Assignment")

## Read in data ------------------------------------------------------------
dfwhc1 = read_excel("data/input/___.xlsx")

## Cleaning ----------------------------------------------------------------

# rename columns