## Data Exploration and Cleaning:
#a. Import a dataset of your choice into R. (Hint: Use the read.csv() function).
#b. Use the head() function to see the first couple rows of the dataset.
#c. Check for missing values in the dataset and handle them accordingly.
#d. Identify any potential outliers in one of the numeric columns using the boxplot()
#function.

#a
df <- read.csv("https://raw.githubusercontent.com/mwaskom/seaborn-data/master/iris.csv")
#b
head(df)
#c
any(is.na(df))
sum(is.na(df))
#d
boxplot(df$sepal_length)
boxplot(df$sepal_width)
boxplot(df$petal_length)
boxplot(df$petal_width)

outliers <- boxplot(df$sepal_width, plot = FALSE)$out
outliers

# Add fake data (3 new rows) with outlier values
outlier_rows <- data.frame(
  sepal_length = c(15, 0.5, 20),   
  sepal_width  = c(3.5, 3.0, 4.0),
  petal_length = c(5.0, 1.0, 6.0),
  petal_width  = c(1.8, 0.2, 2.5),
  species = c("setosa", "versicolor", "virginica")
)

# Combine with the original data
df_out <- rbind(df, outlier_rows)

boxplot(df_out$sepal_length)

outliers <- boxplot(df_out$sepal_length, plot = FALSE)$out
outliers

##
## 2. Data Summarization:
## Calculated for both - original data and data with outliers
# Mean
mean(df$sepal_length)
mean(df_out$sepal_length)

# Median
median(df$sepal_length)
median(df_out$sepal_length)

# Standard deviation
sd(df$sepal_length)
sd(df_out$sepal_length)


table(df_out$species)
prop.table(table(df_out$species)) # percentage


###
## 3. Visualization

install.packages("ggplot2")   # if not installed yet
library(ggplot2)

ggplot(df_out, aes(x = sepal_length)) +
  geom_histogram(
    bins = 20,              
    fill = "blue",         
    color = "black"        
  ) +
  labs(
    title = "Distribution of Sepal Length",
    x = "Sepal Length (cm)",
    y = "Count"
  ) 

# b. Plot a scatter plot between two numeric columns and adjust its aesthetics.
ggplot(df_out, aes(x = sepal_length, y = petal_length, color = species)) +
  geom_point(size = 3, alpha = 0.8) +         # dots
  labs(
    title = "Sepal Length vs Petal Length by Species",
    x = "Sepal Length (cm)",
    y = "Petal Length (cm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "top"                  
  )


#####
## 4. Set Operations with Vectors

# Two numeric vectors
a <- c(1, 2, 3, 4, 5, 6)
b <- c(4, 5, 6, 7, 8, 9)

union(a,b)
intersect(a, b)
setdiff(a, b)
setdiff(b, a)


####
## 5. Probability Computations:
# Calculate probability of an event in a dataset
# Function to calculate probability of an event
probability_of_event <- function(data, condition_func) {
  # data: vector or dataset
  # condition_func - function that returns TRUE/FALSE for each element
  
  event_occurrences <- sum(condition_func(data))
  total <- length(data)
  probability <- event_occurrences / total
  return(probability)
}

# Example usage:
data <- c(1, 2, 3, 4, 5, 6)
probability_of_event(data, function(x) x > 2)
probability_of_event(data, function(x) x == 2)

#### Conditional probability
conditional_probability <- function(data, eventA_func, eventB_func) {
  A_and_B <- sum(eventA_func(data) & eventB_func(data))
  B <- sum(eventB_func(data))
  prob <- A_and_B / B
  return(prob)
}

# Example usage:
data <- 1:10
conditional_probability(data,
                        function(x) x > 5,   # Event A
                        function(x) x %% 2 == 0)  # Event B (even numbers)
# P(A|B) = P(x > 5 and even) / P(even)



### #6
##Bayes' Theorem Implementation:
# a. Given a dataset on medical diagnosis, calculate the posterior probability of a
# disease given a positive test result using Bayes' theorem.

# Given data
p_disease <- 0.01 #assuming 1% of people have the disease
p_healthy <- 1 - p_disease

p_positive_if_disease <- 0.95 # test catches 95% of sick people
p_positive_if_healthy <- 0.10 # 10% of healthy people also test positive

# Bayes' theorem
p_disease_if_positive <- (p_positive_if_disease * p_disease) /
  ((p_positive_if_disease * p_disease) + (p_positive_if_healthy * p_healthy))

cat("Chance of actually having the disease =", round(p_disease_if_positive * 100, 2), "%")


### #7 Simulatioh of 100 die rolls
# Simulate 100 die rolls
rolls <- sample(1:6, size = 100, replace = TRUE)

# calculate frequency and relative frequency
counts <- table(rolls)
rel_freq <- prop.table(counts)

# theoretical probability
theoretical_p <- c(1/6, 1/6,1/6, 1/6,1/6, 1/6)

# Combine results into a data frame
results <- data.frame(
  Face = 1:6,
  Count = as.numeric(counts),
  Relative_Frequency = as.numeric(rel_freq),
  Baseline = theoretical_p,
  Difference = as.numeric(rel_freq) - theoretical_p
)

print(results)

### #8 Distributions
random_numbers <- runif(100)

# display first few values
head(random_numbers)

# plot histogram
hist(
  random_numbers,
  main = "Histogram of 100 Random Numbers (Uniform Distribution)",
  xlab = "Random Number",
  ylab = "Frequency",
  col = "blue",
  border = "black"
)

### #9
# Exploring Discrete Distributions:
#a. Simulate 100 trials of a fair coin toss using R and determine the number of
#heads. What distribution does this follow?

tosses <- sample(c("H", "T"), size = 100, replace = TRUE, prob = c(0.5, 0.5))

# count the number of heads
num_heads <- sum(tosses == "H")

cat("Number of heads:", num_heads, "\n")

## This is Binomial distribution

### #10
## 10. Binomial Distribution:
#a. Using the dbinom, pbinom, and qbinom functions in R, calculate the probabilities
#for the number of successes in 10 trials of a coin toss. Plot the distribution.

n <- 10      # number of trials
p <- 0.5     # probability of success (head)
x <- 0:n     # Possible outcomes: 0 to 10 heads

# dbinom = probability of exactly x heads
prob_exact <- dbinom(x, size = n, prob = p)

# pbinom = cumulative probability (≤ x)
prob_cum <- pbinom(x, size = n, prob = p)

# qbinom = find smallest x with cumulative probability ≥ given value
q_example <- qbinom(0.5, size = n, prob = p)  # median number of heads

# Print some results
cat("Exact probabilities:\n")
print(prob_exact)
cat("\nCumulative probabilities:\n")
print(prob_cum)
cat("\nMedian number of heads:", q_example, "\n")

# Plot the distribution
barplot(prob_exact,
        names.arg = x,
        main = "Binomial Distribution (n=10, p=0.5)",
        xlab = "Number of Heads",
        ylab = "Probability",
        col = "blue")

### #11
# 11. Poisson Distribution:
#a. Assume the average number of customers arriving at a bank in one hour is 5.
#Simulate the number of customers arriving in 100 hours. Plot the results.

cust_per_hour = 5
hours = 100

customers <- rpois(hours, cust_per_hour)

head(customers)

barplot(table(customers),
        main = "Poisson Distribution (λ = 5)",
        xlab = "Number of Customers per Hour",
        ylab = "Frequency",
        col = "green")

### #12
# Generate a sequence of 1000 random numbers following a normal distribution
# with mean 50 and standard deviation 10. Plot the histogram.

data <- rnorm(1000, mean = 50, sd = 10)

# plot histogram
hist(data,
     main = "Normal Distribution (mean = 50, sd = 10)",
     xlab = "Value",
     col = "blue",
     border = "white")

### #13
# 13. Exponential Distribution:
#a. Assume the time between arrivals of buses at a bus stop follows an exponential
#distribution with a rate of 0.2. Simulate the time between 50 bus arrivals.

rate <- 0.2
n <- 50

# time between arrivals
bus_times <- rexp(n, rate = rate) # generates random numbers from an exponential distribution.

# show first few values
head(bus_times)

# Plot histogram
hist(bus_times,
     main = "Exponential Distribution (rate = 0.2)",
     xlab = "Time Between Bus Arrivals",
     col = "blue",
     border = "white")

### #14
# 14. Probability Distribution Functions (PDF) and Cumulative Distribution Functions (CDF):
#a. For the normal distribution generated in #12, plot both the PDF and CDF.

#we already have data in data varianle from #12
x <- seq(min(data), max(data), length.out = 100)

# PDF (density) and CDF values
pdf_values <- dnorm(x, mean = 50, sd = 10)
cdf_values <- pnorm(x, mean = 50, sd = 10)

# plot PDF first (left axis)
plot(x, pdf_values, type = "l", col = "blue", lwd = 2,
     ylab = "PDF",
     xlab = "x",
     main = "Normal Distribution: PDF and CDF")

# add CDF on right axis
par(new = TRUE) #overlays the second plot without erasing the first.
plot(x, cdf_values, type = "l", col = "red", lwd = 2,
     axes = FALSE, xlab = "", ylab = "")
axis(side = 4)           # add right-side axis
mtext("CDF", side = 4, line = 2.5)

legend("topleft", legend = c("PDF", "CDF"),
       col = c("blue", "red"), lwd = 2)

### #15 Data Analysis
# Load the mtcars dataset
data(mtcars)

# extract the mpg variable
mpg <- mtcars$mpg

# Plot histogram
hist(
  mpg,
  main = "Histogram of Miles per Gallon (mpg)",
  xlab = "Miles per Gallon",
  col = "blue",
  border = "black"
)

# Compute descriptive statistics
mean_mpg <- mean(mpg)
median_mpg <- median(mpg)
variance_mpg <- var(mpg)
sd_mpg <- sd(mpg)

# Display the results
cat("Mean MPG:", mean_mpg, "\n")
cat("Median MPG:", median_mpg, "\n")
cat("Variance:", variance_mpg, "\n")
cat("Standard Deviation:", sd_mpg, "\n")

