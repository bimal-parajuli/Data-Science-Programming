# 1.
#Print a String onto the Console in R.

# Directly Print a String literal.
"This is a String."

# Store as a variable then print using the print function.
String1 <- "This is a String."
print(String1)





# 2.
# Input a number from prompt.

# Using the readline() and as.integer() function 


var = as.integer(readline())
var



var = scan()
var


# 3.
# Input two vectors and perform following operations.

#Taking a vector input in one line using readline()
prompt <- "Enter a vector (space-separated) \n"
A_vec <- as.integer(strsplit(readline(prompt), " ")[[1]])
B_vec <- as.integer(strsplit(readline(prompt), " ")[[1]])

A_vec
B_vec



#Taking a vector input one member at a time using scan()
A_vec <- scan()
B_vec <- scan()

A_vec
B_vec




# Addition Operator
A_vec
B_vec

Sum_Vec <- (A_vec + B_vec)
Sum_Vec



# Subtraction operator
A_vec
B_vec

Diff_vec <- (A_vec - B_vec)
Diff_vec


# Multiplication Operator:
A_vec
B_vec

Product_vec <- (A_vec * B_vec)
Product_vec


# Division Operator:
A_vec
B_vec

Division_vec <- (A_vec / B_vec)
Division_vec


# Remainder Operator:
A_vec
B_vec

Remainder_vec <- (A_vec %% B_vec)
Remainder_vec


# Quotient Operator:
A_vec
B_vec


Quotient_vec <- (A_vec %/% B_vec)
Quotient_vec




# Exponent Operator:
A_vec
B_vec

Exponent_vec <- (A_vec ^ B_vec)
Exponent_vec



# Less than Operator
A_vec
B_vec

LessThan_vec <- (A_vec < B_vec)
LessThan_vec


# Greater Than Operator
A_vec
B_vec

GreaterThan_vec <- (A_vec > B_vec)
GreaterThan_vec



# <= Less than or equal to operator
A_vec
B_vec

LessOrEquals_vec <- (A_vec <= B_vec)
LessOrEquals_vec



# >= Greater than or equal to operator
A_vec
B_vec

GreaterOrEquals_vec <- (A_vec >= B_vec)
GreaterOrEquals_vec



# Equals to operator
A_vec
B_vec

Equals_vec <- (A_vec == B_vec)
Equals_vec


# Not equals to Operator
A_vec
B_vec

NotEquals_vec <- (A_vec != B_vec)
NotEquals_vec






# & Element wise Logical AND operator
A_vec
B_vec

ElementwiseAND_vec <- (A_vec & B_vec)
ElementwiseAND_vec





# | Element Wise logical OR operator
A_vec
B_vec

ElementwiseOR_vec <- (A_vec | B_vec)
ElementwiseOR_vec





# ! logical not operator
A_vec
B_vec

LogicalNOT_vec_A <- ( !A_vec )
LogicalNOT_vec_B <- ( !B_vec )

LogicalNOT_vec_A
LogicalNOT_vec_B





# && Logical AND operator
A_vec
B_vec

LogicalAND_vec <- (A_vec && B_vec)
LogicalAND_vec





# || Logical and operator
A_vec
B_vec

LogicalOR_vec <- (A_vec || B_vec)
LogicalOR_vec




# Left assignment operator
x <- c(12,4,5,7,3,6,34,6,3,6)
x



# Right assignment operator
c(12,4,5,7,3,6,34,6,3,6) -> y
y


# Miscellanous operators
# Colon operator

x <- 5:14
x


# %in% operator
x


print(5 %in% x)


print(19 %in% x)

# Matrix multiplication %*% operator
x

t(x)

x %*% x

x %*% t(x)


x = 4


# Take a number and check whether the number is odd or even. (if..else)
num = as.integer(readline(prompt="Enter a number: "))

num


if((num %% 2) == 0) {
  print(paste(num,"is Even"))
} else {
  print(paste(num,"is Odd"))
}



#Take the marks of a student as input and print his/her grade. (if..else..if)

marks = as.integer(readline(prompt="Enter the marks of student: "))

marks


if(marks >= 90) {
  print("S grade.")
} else if(marks >= 80) {
  print("A grade.")
} else if(marks >= 70) {
  print("B grade.")
} else if(marks >= 60) {
  print("C grade.")
} else if(marks >= 50) {
  print("D grade.")
} else if(marks >= 40) {
  print("Just passed.")
}





# Design an arithmetic calculator. (switch case)

val1 = as.integer(readline(prompt="Enter a number: "))
val2 = as.integer(readline(prompt="Enter a number: "))


operation = readline(prompt = "Enter the operation:  ")

res <- switch(
  operation,
  
  "add" = cat("Addition, ", val1, " + ", val2, " = ", val1 + val2),
  "sub" = cat("Subtraction, ", val1, " - ", val2, " = ", val1 - val2),
  "mul" = cat("Multiplication", val1, " * ", val2, " = ", val1 * val2),
  "div" = cat("Division", val1, " / ", val2, " = ", val1 / val2)
)



# Find the factorial of a number. (for loop)

num = as.integer(readline(prompt="Enter a number: "))

num

fact = 1

for(i in 1:num ){
  fact = fact*i
}
print(paste("The factorial of ", num, " is ", fact))


# Check whether the number is armstrong or not. (while loop)

num = as.integer(readline(prompt = "Enter a number to check armstrong."))

num

temp = num
arm = 0

while(num > 0){
  remainder = num %% 10
  arm = arm + remainder ^ 3
  num = num %/% 10
}


if(temp == arm){
  print(paste(temp, " is an armstrong number."))
} else {
  print(paste(temp, " is not an armstrong number."))
}


# Print natural number till their sum reaches 100. (repeat loop)

num = 0
sum = 0;

repeat{
  
  print(num)
  sum = sum + num
  num = num + 1

  if(sum>100)
    break
}




