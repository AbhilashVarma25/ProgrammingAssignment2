# Define a function to cache potentially time-consuming computations
make_cache <- function(f) {
  cache <- NULL
  
  # Define a function that checks if the result is cached
  cached_function <- function(...) {
    args <- list(...)
    arg_string <- paste(args, collapse = "_")
    
    if (!is.null(cache) && identical(cache$args, args)) {
      message("Using cached result.")
      return(cache$result)
    } else {
      result <- f(...)
      cache <<- list(args = args, result = result)
      return(result)
    }
  }
  
  return(cached_function)
}

# Example usage:
# Define a function for a potentially time-consuming computation
compute_mean <- function(x) {
  mean(x)
}

# Create a cached version of the function
cached_mean <- make_cache(compute_mean)

# Call the cached function
cached_mean(c(1, 2, 3, 4, 5))  # This will compute the mean
cached_mean(c(1, 2, 3, 4, 5))  # This will use the cached result
