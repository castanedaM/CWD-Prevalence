# Title: CWD Prevalence Project
# Author: MCG
# Date last updated: 9/17/2020


# Extras ------------------------------------------------------------------

readMe <- "ReadMe.md"

cat("**CWD Prevalence** Project", file = readMe, sep = "\n")

cat(paste0("Date: ", Sys.Date()), file = readMe, sep = "\n", append = TRUE)

# Part I -------------------------------------------------------------

# Read csv data
cwd_1 <- read.csv("Master_CWD_Data.csv", 
                header = TRUE, 
                stringsAsFactors = FALSE)

# Check basic structure
str(cwd_1) # 11201 rows, 14 columns
names(cwd_1) 

# [1] "Deer.ID"     "Sample.Date" "Year"        "Month"      
# [5] "Origin"      "Sample"      "Species"     "Sex"        
# [9] "Age"         "County"      "grid"        "lat"        
# [13] "long"        "status" 

head(cwd_1)

# Looking for structure in columns to assign data type. First thing is to
# identify NA strings

# Generates Multivariate Imputations by Chained Equations (MICE)
library(mice) 

# Display missing-data patterns per row per column. The total of NA's shows in
# the left y-axis (pink = NA, blue = exists).
md.pattern(cwd_1, rotate.names = TRUE)

# Visualization of missing or imputed values in
library(VIM)

# Display histogram and pattern of NAs as well as list of frequency values. 
aggr_plot <- aggr(cwd_1, 
                  col=c('lightgreen','pink'),
                  numbers = TRUE, 
                  sortVars = TRUE, 
                  labels = names(cwd),
                  cex.axis = .7, 
                  gap = 3, 
                  ylab = c("Histogram of missing data","Pattern"))


# Look for all NaS
naStrings <- c(NA, NULL, " ")

# For Deer.ID
unique(cwd_1$Deer.ID)

unique(gsub("[[:digit:]]+", "", cwd_1$Deer.ID))

# NA = "" 
which(is.na(cwd_1$Deer.ID))

table(is.na(cwd_1$Deer.ID))

which(cwd_1$Deer.ID == "")

naStrings <- c(naStrings, "")
# Make a character string (unique)

# For Sample Date
unique(cwd_1$Sample.Date)
# Make a date

# For Year
unique(cwd_1$Year)
# Make a factor

# For Month
unique(cwd_1$Month)

table(is.na(cwd_1$Month))

naStrings <- c(naStrings, "NA")
# Make a factor <- maybe month abbv

# For Origin
unique(cwd_1$Origin)

# [1] "Free-Ranging" "Enclosure"    "Other" 
# Make a factor

# For Sample
unique(cwd_1$Sample)

# [1] "Active - Hunter Killed"    "Active - Other"           
# [3] "Targeted-Clinical suspect" "Active - Road-Killed"     
# [5] "Targeted - Other" 

# Decide on what categories to use, divided info into two columns, maybe?

# For Species
unique(cwd_1$Species)
# [1] "White Tailed Deer" ""                 
# Nas are "" but already in naString

# For Sex
unique(cwd_1$Sex)

# Nas = unknown
naStrings <- c(naStrings, "Unknown")

# For Age
unique(cwd_1$Age)

# Keep track of NAs
naStrings <- c("NA", " ", "UNK", "unk", "", "-", "NA", "Unk")

# In next step, make a categorical varibale with 3 categories

# For County
unique(cwd_1$County)

# [1] "Frederick"  "Shenandoah" "Clarke"     "Warren"    
# [5] "Culpeper"   "Fauquier"

# For grid
unique(cwd_1$grid)

table(is.na(cwd_1$grid))

table(cwd_1$grid)

# Not a numeric column, is a character column - identifier

# For lat and long
unique(cwd_1$lat)
table(is.na(cwd_1$lat)) # Fix later NAs are Nas

# For Status
unique(cwd_1$status) 
# [1] 0 1
# Categorical data <- factor

# Part II ------------------------------------------------------------

# Read data once more, but include naStrings
cwd_2 <- read.csv("Master_CWD_Data.csv", 
                header = TRUE, 
                stringsAsFactors = FALSE, 
                na.strings = naStrings)

names(cwd_2)
head(cwd_2)


# Display missing-data patterns per row per column. The total of NA's shows in
# the left y-axis (pink = NA, blue = exists).
md.pattern(cwd_2, rotate.names = TRUE)

# [1] "Deer.ID"   <- identifier = character
# [2] "Sample.Date" <- date = date
# [3] "Year"       <- date = date (factor)
# [4] "Month"      <- date = date (factor)
# [5] "Origin"     <- category = factor
# [6] "Sample"     <- category = factor (make two columns)
# [7] "Species"    <- category = factor
# [8] "Sex"        <- category = factor
# [9] "Age"        <- category = fator (make into three subgroups)
# [10] "County"     <- category = factor
# [11] "grid"       <- identifier = character
# [12] "lat"        <- coordinate = numeric
# [13] "long"       <- coordinate = numeric
# [14] "status"     <- category = factor


# Date columns

# First same cwd_2
cwd_3 <- cwd_2

library(lubridate) # https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf

cwd_3$Sample.Date <- as.Date(cwd_3$Sample.Date, format = "%m/%d/%Y")
cwd_3$Month <- month(cwd_3$Sample.Date, label = TRUE, abbr = TRUE)
cwd_3$Year <- year(cwd_3$Sample.Date)

# barplot(table(cwd_3$Sample.Date)) 
# barplot(table(cwd_3$Year))
# barplot(table(cwd_3$Month))

# Sample Column <- make two
unique(cwd_3$Sample)

# [1] "Active - Hunter Killed"    "Active - Other"           
# [3] "Targeted-Clinical suspect" "Active - Road-Killed"     
# [5] "Targeted - Other"

# save cwd_3 
cwd_4 <- cwd_3

cwd_4$Sample[which(cwd_4$Sample == "Active - Road-Killed")] <- "Active - Road Killed"

unique(cwd_4$Sample)

library(tidyr)

cwd_4 <- cwd_4 %>% separate(Sample, c("Sample1","Sample2"), "-")


# Fix lat and long with grid

# Read in Data for grids

grids <- read.csv("GridsDMA1LatLong.csv", stringsAsFactors = FALSE, header = TRUE)
head(grids)

# Left join based on grid numbers

library(dplyr)

cwd_5 <- left_join(cwd_4 , grids, by = c("grid" = "Grid_txt"))

head(cwd_5)

# Check if lat long exists is there a grid?
md.pattern(cwd_5[, c("grid", "lat", "long")], rotate.names = TRUE)

md.pattern(cwd_5[, c("grid", "Latitude", "Longitude")], rotate.names = TRUE)

# T02033 grid number 5315, not in grid file

# Extention for grids to get coordinates when grid is none existant and county
# name is known
cwd_5_1 <- cwd_5[ , c("grid", "County", "status")]

cwd_5_2 <- aggregate(status ~ grid + County, cwd_5_1, sum)

grids_2 <- left_join(grids, cwd_5_2, by = c("Grid_txt" = "grid"))

head(grids_2)


# Fix Age column

unique(cwd_5$Age)
as.data.frame(table(cwd_5$Age)) 

# [1] "2.5"         "1.5"         "3.5"        
# [4] "2"           "3"           "4"          
# [7] "15"          "6"           "6.5"        
# [10] "1.6"         "7"           "1"          
# [13] "4.5"         NA            "M"          
# [16] "1.7"         "5.5"         "5"          
# [19] "0.5"         "2,5"         "10.5"       
# [22] "ADULT"       "8"           "10"         
# [25] "Unk"         "10+"         "adult   8pt"
# [28] "9"           "adult 10pt"  "adult 10ot" 
# [31] "adult 8pt"  


juvenile <- c(seq(1, 4, 0.5), "2,5")
young_adult <- c(seq(4.5, 6, 0.5), "M")
adult <- c(seq(6.5, 11, 0.5), "ADULT", "10+", 
           "adult   8pt", "adult 10pt", 
           "adult 10ot", "adult 8pt")

cwd_5$Age <- ifelse(cwd_5$Age %in% juvenile, "Juvenile", 
                    ifelse(cwd_5$Age %in% young_adult, "Young Adult", 
                           "Adult"))

# reorder and remove some columns
md.pattern(cwd_5, rotate.names = TRUE)

names(cwd_5)

# [1] "Deer.ID"    
# [2] "Sample.Date"
# [3] "Year"       
# [4] "Month"      
# [5] "Origin"     
# [6] "Sample1"    
# [7] "Sample2"    
# [8] "Species"    
# [9] "Sex"        
# [10] "Age"        
# [11] "County"     
# [12] "grid"       
# [13] "lat"  (delete)      
# [14] "long"    (delete)   
# [15] "status"     (move to last)
# [16] "Latitude"   
# [17] "Longitude" 

cwd_6 <- cwd_5[ , c(1:12, 16, 17, 15)]

head(cwd_6)
str(cwd_6)

# Set categorical data to factors and continous to numeric
names(cwd_6)

factor_columns <- c(1:12, 15)
numeric_columns <- c(13, 14)

cwd_6[ , factor_columns] <- lapply(cwd_6[ , factor_columns], as.factor)
cwd_6[ , numeric_columns] <- lapply(cwd_6[ , numeric_columns], as.numeric)

str(cwd_6)

# write.csv(x = cwd_6, file = "Master_CWD_Data_CLEAN.csv", row.names = FALSE)


# Some plots --------------------------------------------------------------

library(ggplot2)
library(RColorBrewer)

library(showtext)
font_add("Arial", "arial.ttf")
showtext_auto()

## Loading Google fonts (https://fonts.google.com/)
font_add_google(name = "Roboto", 
                family = "Roboto")

# font_add_google(name = "Arvo", 
                # family = "Arvo")

showtext_auto()

theme_set(theme_classic() + theme(
  legend.key = element_rect(fill = "transparent", colour = "transparent"),
  legend.title = element_text(size = 20, family = "Roboto", face = "bold"),
  legend.text = element_text(size = 20, family = "Roboto"),
  
  axis.title = element_text(size = 25, family = "Roboto", face = "bold"),
  axis.text = element_text(size = 20),
  axis.line.x = element_line(colour = "black", size = 1, linetype = "solid"),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "#b8b9ba"),
  panel.grid.minor.y = element_line(color = "#e1e2e3"),
  
  panel.background = element_blank()
))

# For all
ggplot(data = cwd_6, aes(x = Month, fill = status)) +
  geom_histogram(stat = "count", color = "black") +
  scale_fill_brewer(palette = "Set1")

ggplot(data = cwd_6, aes(x = Year, fill = status)) +
  geom_histogram(stat = "count") +
  scale_fill_brewer(palette = "Set2")

ggplot(data = cwd_6, aes(x = Age, fill = status)) +
  geom_histogram(stat = "count") +
  scale_fill_brewer(palette = "Set3")



# For only positives
data <- subset(cwd_6, status == 1)

ggplot(data = data, aes(x = Month, fill = Month)) +
  geom_histogram(stat = "count", color = "black") +
  scale_fill_brewer(palette = "Spectral") +
  theme(legend.position = "none")


# Classic palette BuPu, with 4 colors
coul <- brewer.pal(11, "Spectral") 

# Add more colors to this palette : 
# pal <- colorRampPalette(c("red", "yellow"))

coul <- colorRampPalette(coul)(12)

ggplot(data = data, aes(x = Year, fill = Year)) +
  geom_histogram(stat = "count", color = "black", fill = coul) +
  theme(legend.position = "none")

ggplot(data = data, aes(x = Age, fill = Age)) +
  geom_histogram(stat = "count", color = "black") +
  scale_fill_brewer(palette = "Spectral") +
  theme(legend.position = "none")


# OTHERS ------------------------------------------------------------------
# Assigned/impute values to missing data, using MICE

# library(missForest)

# Use the set.seed function when running simulations to ensure all results,
# figures, etc are reproducible.

# Got an error on line below: Error in solve.default(xtx + diag(pen)) : system
# is computationally singular: reciprocal condition number = 7.72413e-19. Forced
# model to use "cart" (classification and regression trees). Solution and
# explanation found here:
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586

# tempData <- mice(cwd, method = "cart", seed = 12345)

# https://cran.r-project.org/web/packages/missForest/missForest.pdf Runs random
# forest to impute missing values, and can be ran in parrallel Default 10
# iterations, return normalized root mean squared error (NRMSE) and The
# proportion of falsely classifeid (PFC)

# tempData <- missForest(data)
