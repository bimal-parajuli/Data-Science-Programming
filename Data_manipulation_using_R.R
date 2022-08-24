# 1.
# Create user defined functions to perform various arithmetic operations 
# and call each functions using a menu driven program.
# Try functions with parameters and without parameters.



add_func <- function(a, b){
  temp <- (a + b)
  return (temp)
}

subtract_func <- function(a, b){
  temp <- (a - b)
  return (temp)
}

multiply_func <- function(a, b){
  temp  <- (a * b)
  return (temp)
}

division_func <- function(a, b){
  temp <- (a/b)
  return (temp)
}


a = as.integer(readline(prompt = "Enter a number      "))
b = as.integer(readline(prompt = "Enter a number.     "))


operation = readline(prompt = "Enter the operation.")
result <- switch(
  operation,
  
  "add" = cat("Addition: ", a , " + ", b, " = ", add_func(a, b)),
  "sub" = cat("Subtraction: ", a, " - ", b, " = ", subtract_func(a, b)),
  "mul" = cat("Multiply:  ", a, " * ", b, " = ", multiply_func(a, b)),
  "div" = cat("Division: ", a, " / ", b, " = ", division_func(a, b))
)



#  Examples of Using Default parameters.

# Taake b = 10 as default parameter.


add_func1 <- function(a, b=10){
  temp <- (a + b)
  return (temp)
}

subtract_func1 <- function(a, b=10){
  temp <- (a - b)
  return (temp)
}

multiply_func1 <- function(a, b=10){
  temp  <- (a * b)
  return (temp)
}

division_func1 <- function(a, b=10){
  temp <- (a/b)
  return (temp)
}


a = as.integer(readline(prompt = "Enter a number      "))
b = as.integer(readline(prompt = "Enter a number.     "))

operation = readline(prompt = "Enter the operation.")
result <- switch(
  operation,
  
  "add" = cat("Addition: ", a , " + ", b, " = ", add_func1(a)),
  "sub" = cat("Subtraction: ", a, " - ", b, " = ", subtract_func1(a)),
  "mul" = cat("Multiply:  ", a, " * ", b, " = ", multiply_func1(a)),
  "div" = cat("Division: ", a, " / ", b, " = ", division_func1(a))
)



# 2. 
# Perform basic statistical operations on a random vector of 100 elements.

# Create a random Vector.

# Using sample() function to generate integers.
random_vec <- sample(0:30, size = 100, replace = TRUE)
random_vec


# Using runif() function to generate real numbers in a range.
random_vec <- runif(100, min = 20, max = 100)
random_vec




random_vec

# a. Mean

mean_value = mean(random_vec)
mean_value

# b. Median

median_value = median(random_vec)
median_value



# c. Mode

# Since there is no inbuilt function to find mode in R, 
# we need to create a user-defined function as follows:

find_mode <- function(vec){
  unique_vec <- unique(vec)
  unique_vec[which.max(tabulate(match(vec, unique_vec)))]
}
mode_value = find_mode(random_vec)
mode_value



# d. Range

# Calculate the upper and lower limits of the vector.
limits <- range(random_vec)
limits

# Find the range.
range_ <- diff(range(random_vec))
range_

# e. IQR
# Using IQR() function. 

iqr_val <- IQR(random_vec)
iqr_val




# f. Standard Deviation

#  Using sd() function.

sd_val <- sd(random_vec)
sd_val




# g. Summary

# using summary() function

summary_ <- summary(random_vec)
summary_



# h. Histogram

# Using hist() function.
hist(random_vec)

# Modifying some parameters of the histogram.
hist(random_vec, main = "Sample histogram", xlab = "Random Value", breaks = 20, border = "blue", col = "yellow")



# tabulate(random_vec)
# tabulate(random_vec, nbins = 10)

data.frame(random_vec[1:20])

# i. Table


rbind(one_to_10=random_vec[1:10],
      ten_to_20 = random_vec[11:20], 
      twenty_to_30 = random_vec[21:30],
      thirty_to_40 = random_vec[31:40],
      forty_to_50 = random_vec[41:50])









# 3.
# Data Frame operations

# a. Create a Data Frame

people_df <- data.frame(
  s_no = c(1:7),
  names = c("Ram", "Hari", "Sita", "Gita", "Babita", "Jethalal", "Champak"),
  age = c(25, 34, 4, 78, 22, 42, 72),
  height_cm = c(150, 132, 145, 156, 178, 157, 165),
  weight_kg = c(45, 65, 73, 46, 51, 71, 64),
  stringsAsFactors = FALSE
)


# b. Access a Component ([, [[, $)


# Using []
  
people_df[2]
# Using [[

people_df[[ "height_cm" ]]

# Using $

people_df$names

# c. Structure of Data Frame

str(people_df)




# d. Add a new Column
people_df

people_df$job = c("Doctor", "Engineer", "Teacher", "Army", "Pilot", "BusinessMan", "Layer")

people_df




# e. Add a new row.

people_df

temp_list <- list(10, "Ramesh", 22, 193, 75, "Designer")

people_df[nrow(people_df) + 1,] <- temp_list




# f. Delete Column

# Using subset() function

people_df
people_df = subset(people_df, select = -c(s_no))

people_df


# g. Delete Specific row
# Delete nth row in the dataframe

people_df
people_df = people_df[-c(3, 5), ]
people_df


# Delete a row by satisfying a particular value.
    # Eg: Delete a row where name = "Ramesh"

people_df

people_df = people_df[!(people_df$names=="Ramesh"),]

people_df

# h. Order data frame (with, order, arrange)

people_df

# Sorting people by age:
# Using order() function,
ordered_people_df <- people_df[order(people_df$age), ]

ordered_people_df


install.packages("dplyr")


# Using arrange()
library(dplyr)

# Creating dataframe
data = data.frame(Customers = c("Roohi", "James", "Satish", "Heera", 
                               "Sehnaaz", "Joe","Raj", "Simran", 
                               "Priya","Tejaswi"),
                 
                 Product = c("Product A", "Product B", "Product C",
                             "Product A", "Product D", "Product B",
                             "Product D", "Product C", "Product D",
                             "Product A"),
                 
                 Salary = c(514.65, 354.99, 345.44, 989.56, 767.50,
                            576.90, 878.67, 904.56,123.45, 765.78)
)

data                

# Sort the dataframe in ascending order
arrange(data, Salary)



# 4. 

# Read Air Quality dataset and handle the handle the missing data using following technique

library(ggplot2)
library(moments)

air_data <- data.frame(airquality)

# For convenience, we are only taking top 11 records of the dataset.
sample_air_data <- head(air_data, 11)

# a. Drop row

# Drop all the rows having a NA record.
sample_air_data

sample_air_data_na_row_removed = na.omit(sample_air_data)

sample_air_data_na_row_removed

# b. Drop Column

# Drop the column having an NA value.
# Using Base R
sample_air_data
sample_air_data_na_col_dropped = sample_air_data[ , colSums(is.na(sample_air_data))==0]
sample_air_data_na_col_dropped


# Using the dplyr package

library(dplyr)
sample_air_data
sample_air_data_na_col_dropped1 = sample_air_data %>% select_if(~ !any(is.na(.)))
sample_air_data_na_col_dropped1


# c. Imputation (Replace with unknown, mean or Group mean)
# Replace the NA with 0,

sample_air_data1 <- sample_air_data

sample_air_data1

sample_air_data1[is.na(sample_air_data1)] = 0

sample_air_data1


# Replace with mean

library(dplyr)
library(tidyr)

sample_air_data2 <- sample_air_data

sample_air_data2

sample_air_data2 <- sample_air_data2 %>% mutate_if(is.numeric, ~replace_na(.,as.integer(mean(., na.rm = TRUE))))

sample_air_data2


# 5. Perform label Encoding on IRIS dataset

# Import the dplyr package
library(dplyr)

# Load the dataset
data <- iris

# Before encoding
glimpse(data)
summary(data$Species)

# Performing Encoding
data$Species <- as.numeric(factor(data$Species))

# After Encoding
glimpse(data)
summary(data$Species)






# 6. Perform one-hot encoding on dataset
library(modelr)

summary(iris)

SpecMM <- model_matrix(iris, Sepal.Length ~ Species-1)

summary(SpecMM)

print(SpecMM)



# 7. Feature Scaling or standardization

# a. Normalization.

# Normalization to a scale


rawData = c(12030,34567,3456,12,3456,0985,1211)


print(rawData)


summary(rawData)


normData = as.data.frame(scale(rawData))


summary(normData)


print(normData)



# b. Z-Scale.


# z scale
rawData = c(12030,34567,3456,12,3456,0985,1211)
rawData

avg <- mean(rawData)
stDev <- sd(rawData)
print(avg)
print(stDev)

rawData.z <- (rawData - avg) / stDev
mean(rawData.z)
sd(rawData.z)

print("Z-scaled vector : ")
print(rawData.z)





