library(numDeriv) # For computing gradients
library(viridis)  # For color palettes in plotting
library(ggplot2)  # For enhanced plotting

# Gradient descent function remains the same
gradient_descent <- function(f, start_value, max_iter, gamma = 0.00000015) {
  current_value <- start_value
  history <- matrix(nrow = max_iter + 1, ncol = length(start_value))
  
  history[1,] <- start_value
  for (i in 1:max_iter) {
    grad <- grad(f, current_value)
    current_value <- current_value - gamma * grad
    history[i + 1,] <- current_value
  }
  
  list(minimizer = current_value, history = history)
}

# Corrected Function for plotting the optimization process
plot_optimization <- function(history, f) {
  # Ensure the function f is applied correctly over the history
  function_values <- apply(history, 1, function(v) f(v)) # Correctly apply f
  
  # Plot the function value against the iteration number
  plot(function_values, type = 'l', main = "Function Optimization", xlab = "Iteration", ylab = "Function Value", col = "blue")
}

# Function for plotting the contour and optimization path
plot_contour_with_path <- function(f, range_x, range_y, history) {
  x_seq <- seq(range_x[1], range_x[2], length.out = 200)
  y_seq <- seq(range_y[1], range_y[2], length.out = 200)
  z <- outer(x_seq, y_seq, Vectorize(function(x, y) f(c(x, y))))
  
  filled.contour(x_seq, y_seq, log(z + 1), nlevels = 50, color.palette = viridis::viridis,
                 xlab = "x", ylab = "y", main = "Function Contour with Optimization Path",
                 plot.axes = {
                   axis(1); axis(2)
                   points(history[,1], history[,2], col = "orange", pch = 20, cex = 0.5)
                   points(history[1,1], history[1,2], col = "red", pch = 20, cex = 0.7)
                   points(history[nrow(history),1], history[nrow(history),2], col = "red", pch = 8, cex = 0.7)
                 })
}


# Corrected definition for the Goldstein-Price function
my_function_vec <- function(v) {
     x <- v[1]
     y <- v[2]
     (1 + (x + y + 1)^2 * (19 - 14*x + 3*x^2 - 14*y + 6*x*y + 3*y^2)) *
      (30 + (2*x - 3*y)^2 * (18 - 32*x + 12*x^2 + 48*y - 36*x*y + 27*y^2))
   }

# Perform gradient descent
result <- gradient_descent(my_function_vec, start_value = c(1, -0.5), max_iter = 10000, gamma = 0.0000001)

# Plot the contour with optimization path
# Define the range for x and y based on your function's landscape
plot_contour_with_path(my_function_vec, c(-3, 1), c(-2, 2), result$history)

plot_optimization(result$history, my_function_vec)


# Assuming `result` holds the result from the gradient descent optimization
# And `my_function_vec` is your objective function for the optimization

# Print out the optimized parameters
optimized_parameters <- result$minimizer
cat("Optimized parameters: x =", optimized_parameters[1], ", y =", optimized_parameters[2], "\n")

# Evaluate the function at the optimized parameters
optimized_value <- my_function_vec(optimized_parameters)

# Print the evaluated function value
cat("Function value at optimized parameters:", optimized_value, "\n")




# Goldstein price
# my_function_vec <- function(v) {
#   x <- v[1]
#   y <- v[2]
#   (1 + (x + y + 1)^2 * (19 - 14*x + 3*x^2 - 14*y + 6*x*y + 3*y^2)) *
#     (30 + (2*x - 3*y)^2 * (18 - 32*x + 12*x^2 + 48*y - 36*x*y + 27*y^2))
# }
# 
# Booth Function
# 
# my_function_vec <- function(v) {
#   x <- v[1]
#   y <- v[2]
#   (x + 2*y - 7)^2 + (2*x + y - 5)^2
# }


