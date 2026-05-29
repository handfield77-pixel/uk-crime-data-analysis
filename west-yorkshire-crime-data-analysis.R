# West Yorkshire Crime Data Analysis Dashboard

# Author: SelenaH Dev

# MSc Data Analytics Portfolio Project

#

# Description:

# This script imports, cleans, analyses, and visualises West Yorkshire crime data

# from 2025. The analysis focuses on crime type distribution, monthly crime trends,

# and the locations with the highest crime counts. The outputs were used to create

# an interactive dashboard using R.

#

# Skills Demonstrated:

# - Data Cleaning

# - Data Transformation

# - Data Visualisation

# - Exploratory Data Analysis

# - Dashboard Development

#

# Tools Used:

# - R

# - RStudio

# - dplyr

# - ggplot2

# - readr

# ============================================================================

# 1. Load Required Packages

# ============================================================================

library(readr)
library(dplyr)
library(ggplot2)

# ============================================================================

# 2. Import Crime Data

# ============================================================================

# Load all CSV files containing West Yorkshire crime data

files <- list.files("../",
pattern = "csv",
recursive = TRUE,
full.names = TRUE,
ignore.case = TRUE)

crime_data <- do.call(rbind, lapply(files, read_csv))

# ============================================================================

# 3. Select and Clean Relevant Columns

# ============================================================================

crime_small <- crime_data[, c("Month", "Crime type", "Location")]

# Remove records with missing locations

crime_filtered <- crime_small[crime_small$Location != "No Location", ]

# ============================================================================

# 4. Group Similar Crime Categories

# ============================================================================

crime_grouped <- crime_filtered %>%
mutate(`Crime type` = case_when(
`Crime type` %in% c("Other crime", "Other theft") ~ "Other",
`Crime type` %in% c("Robbery",
"Theft from the person",
"Bicycle theft") ~ "Minor theft offences",
TRUE ~ `Crime type`
))

# ============================================================================

# 5. Crime Type Distribution Analysis

# ============================================================================

crime_counts_grouped <- crime_grouped %>%
count(`Crime type`)

ggplot(crime_counts_grouped,
aes(x = reorder(`Crime type`, n),
y = n / 1000)) +
geom_bar(stat = "identity",
fill = "#9B8AAE") +
coord_flip() +
scale_y_continuous(breaks = seq(0, 130, by = 10)) +
ggtitle("Crime Count by Type in West Yorkshire (2025)") +
labs(
x = "Crime Type",
y = "Number of Crimes (Thousands)"
) +
theme_minimal() +
theme(
plot.title = element_text(size = 14, face = "bold"),
axis.title = element_text(size = 11),
axis.text = element_text(size = 10)
)

# ============================================================================

# 6. Monthly Crime Trend Analysis

# ============================================================================

monthly_counts <- crime_grouped %>%
count(Month)

monthly_counts$Month <- format(
as.Date(paste0(monthly_counts$Month, "-01")),
"%b"
)

monthly_counts$Month <- factor(
monthly_counts$Month,
levels = c(
"Jan","Feb","Mar","Apr","May","Jun",
"Jul","Aug","Sep","Oct","Nov","Dec"
)
)

ggplot(monthly_counts,
aes(x = Month,
y = n,
group = 1)) +
geom_line(
color = "black",
linewidth = 1.2
) +
geom_point(
color = "#8E7A9B",
size = 3
) +
scale_y_continuous(
breaks = seq(22000, 28000, by = 1000)
) +
ggtitle("Monthly Crime Levels in West Yorkshire (2025)") +
labs(
x = "Month",
y = "Number of Crimes"
) +
theme_minimal() +
theme(
plot.title = element_text(face = "bold")
)

# ============================================================================

# 7. Top 10 Crime Locations Analysis

# ============================================================================

location_counts <- crime_grouped %>%
count(Location) %>%
arrange(desc(n)) %>%
head(10)

ggplot(location_counts,
aes(x = reorder(Location, n),
y = n)) +
geom_bar(
stat = "identity",
fill = "#B8C9A3"
) +
coord_flip() +
scale_y_continuous(
breaks = seq(0, 9000, by = 1000)
) +
ggtitle("Top 10 Locations by Crime Count in West Yorkshire (2025)") +
labs(
x = "Location",
y = "Number of Crimes"
) +
theme_minimal() +
theme(
plot.title = element_text(face = "bold")
)

# ============================================================================

# End of Script

# ============================================================================
