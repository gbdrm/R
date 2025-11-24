# 1. Vectors
# vector from 1 to 99 in steps of 15
my_vector <- seq(1, 99, 15)
my_vector

# add character item
my_vector <- c(my_vector, "foo")
my_vector

# check type after adding item
typeof(my_vector)

# coerce back to double
my_vector_double <- as.double(my_vector)
my_vector_double

# 2. Importing data
influenza_data <- read.csv("Influenza_Vaccination_Coverage.csv")

# examine top and tail
head(influenza_data)
tail(influenza_data)

# summarize values
summary(influenza_data)

# data structure
str(influenza_data)

# 3. Data verbs
# how many unique values of "Geography"
length(unique(influenza_data$Geography))

# how many rows for each Season? how many unique Months?
table(influenza_data$Season)
length(unique(influenza_data$Month))

# filter to Colorado only
colorado_data <- influenza_data[influenza_data$Geography == "Colorado", ]
nrow(colorado_data)

# filter to Colorado and group by Vaccine
colorado_vaccine <- table(colorado_data$Vaccine)
colorado_vaccine

# 4. Cleaning column names to summarize

# rename Geography Type to geo_type
names(influenza_data)[names(influenza_data) == "Geography.Type"] <- "geo_type"

# rename other columns with spaces and punctuation using janitor
library(janitor)
influenza_data <- clean_names(influenza_data)

# check data types
str(influenza_data)

# convert estimate_percent to numeric
influenza_data$estimate_percent <- parse_number(influenza_data$estimate_percent)

# verify the conversion
str(influenza_data)
