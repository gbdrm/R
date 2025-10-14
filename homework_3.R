df <- read.csv("https://raw.githubusercontent.com/mwaskom/seaborn-data/master/iris.csv")
head(df)

any(is.na(df))
sum(is.na(df))

boxplot(df$sepal_length)
boxplot(df$sepal_width)
boxplot(df$petal_length)
boxplot(df$petal_width)

outliers <- boxplot(df$sepal_length, plot = FALSE)$out
outliers

# Add fake dadta (3 new rows) with outlier values
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
## Calculated for both - origial data and data with outliers
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
    bins = 20,                # number of bars
    fill = "skyblue",         # bar color
    color = "black"           # border color
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
    plot.title = element_text(hjust = 0.5),   # center the title
    legend.position = "top"                   # move legend
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
probability_event <- function(data, condition) {
  cond <- eval(substitute(condition), data, parent.frame())
  mean(cond, na.rm = TRUE)
}

# test the external data
probability_event(df, sepal_length > 6)
probability_event(df, sepal_width < 3)

# test some local vectors
df2<-data.frame(color = c("red","blue","red","green","red"),
                value = c(5,7,3,9,4))

probability_event(df2, color == "red")   # 0.6
probability_event(df2, value > 5)        # 0.4


conditional_probability<-function(a, b){
  mean(a&b)/mean(b)
}

A <- df$sepal_length > 6
B <- df$species == "versicolor"

conditional_probability(A, B)


