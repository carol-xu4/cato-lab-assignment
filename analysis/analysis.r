## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, readr, ggthemes, scales)


# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Cato Lab Assignment")

# Read in data ------------------------------------------------------------
data = read.csv("data/output/finaldata.csv")

# plot --------------------------------------------------------------------
data$year = as.Date(data$year)

# plot 1 (indexed)
data = data %>%
    mutate(gdp_percent = gdp_index - 100,
    income_percent = income_index - 100)

data2 = data %>%
    select(year, income_percent, gdp_percent) %>%
    pivot_longer(cols = c(income_percent, gdp_percent),
        names_to = "series", 
        values_to = "index") %>%
    mutate(series = recode(series, 
        income_percent = "Median Household Income", 
        gdp_percent = "GDP Per Capita"))

label_data = data2 %>%
    group_by(series) %>%
    filter(year == max(year)) %>%
    ungroup() %>%
    mutate(y_nudge = ifelse(series== "Median Household Income", -75, 0))

ggplot(data2, aes(x = year, y = index, color = series)) +
    geom_line(linewidth = 4.0) +
    scale_color_manual(values = c(
        "GDP Per Capita" = "#C97703",
        "Median Household Income" = "#3043B4")) +
    scale_y_continuous(breaks = seq(0, 300, by = 100),
        labels = function(x) paste0(x, "%")) + 
    coord_cartesian(ylim = c(0,300)) +
    labs(title = "U.S. Median Household Income vs. GDP (1953-2024)",
        subtitle = "Percent change since 1953; Real median family (2023 $) and real GDP per capita (2017 $) ",
        caption = "Source: U.S. Cenus Bureau; Federal Reserve Economic Data (FRED)",
        x = NULL, y = NULL) + 
    scale_x_date(limits = as.Date(c("1953-01-01", "2024-01-01")),
        breaks = seq(as.Date("1953-01-01"), as.Date("2024-01-01"), by = "10 years"),
        date_labels = "%Y") +
    geom_text(data = label_data,
        aes(label = series, y = index + y_nudge),
        hjust = 1.05, vjust = 0, size = 12,
        show.legend = FALSE) +
    theme_stata() + 
    theme(
        text = element_text(family = "sans"),
        plot.title = element_text(size = 40, face = "bold", hjust = 0, color = "black"),
        plot.subtitle = element_text(size = 30, color = "black", margin = margin(b = 12), hjust = 0),
        legend.position = "none",
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30, angle = 0, vjust = 0.5, hjust = 0.5),
        plot.caption = element_text(size = 20, vjust = -1.0),
        plot.background = element_rect(fill = "white"))
ggsave("results/plot1.png", 
    width = 20, height = 15)

# plot 2 (actual values)
y1 = range(data$income)
y2 = range(data$gdp)

ggplot(data, aes(x = year)) +
  geom_line(aes(y = income, color = "Median Household Income"),
            linewidth = 1.3) +
  geom_line(aes(y = rescale(gdp, to = y1, from = y2),
                color = "GDP per Capita"),
            linewidth = 1.3) +
  geom_point(aes(y = income, color = "Median Household Income"), size = 2) +
  geom_point(aes(y = rescale(gdp, to = y1, from = y2),
                 color = "GDP per Capita"), size = 2) +
  scale_y_continuous(
    name = "U.S. Real Median Household Income (2023 $)",
    sec.axis = sec_axis(
      ~ rescale(., to = y2, from = y1),
      name = "Real GDP per Capita (2017 $)")) +
  labs(title = "Median Income vs GDP per Capita (1953–2024)",
    x = "Year",
    y = NULL,
    color = "") +
  scale_x_date(
    date_breaks = "10 years",
    date_labels = "%Y") +
  theme_stata() +
  theme(
    plot.title = element_text(size = 35, face = "bold"),
    axis.title  = element_text(size = 25, face = "bold"),
    axis.text   = element_text(size = 20),
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 16))
ggsave("results/plot2.png", 
    width = 18, height = 12)
