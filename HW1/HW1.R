# Read the txt file
customer <- scan("customers.txt", skip=1)

# Problem 1-7
cat("1. The 5th element is ", customer[5], '\n')
customer_sorted <- sort(customer, decreasing = FALSE) # sort increasing
cat("2. The fifth lowest age is ", customer_sorted[5], '\n')
cat("3. Extracting the five lowest ages together: ", customer_sorted[1:5], '\n')
customer_sorted <- rev(customer_sorted) # decreasingly
cat("4. The five highest ages: ", customer_sorted[1:5], '\n')
cat("5. The average (mean) age: ", mean(customer), '\n')
cat("6. The standard deviation of ages: ", sd(customer), '\n')
age_diff <- customer - mean(customer) # problem 7
cat("8. The average of age_diff is ", mean(age_diff))

# Problem 9a
hist(customer, xlab = "age", breaks = 5)

# Problem 9b
den <- density(customer)
plot(den, frame = FALSE, col = "blue", main = "Density of Customer")

# Problem 9c
boxplot(customer, horizontal=TRUE)
stripchart(customer, method="stack", add=TRUE)
