# llr function that computes the fits at a point zi and apply it to each element of z:
llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

compute_f_hat <- function(z, x, y, omega) {
  weights <- make_weight_matrix(z, x, omega)
  X <- make_predictor_matrix(x)
  
  # Use `sweep` to apply weights to each row of X
  WX <- sweep(X, 1, weights, `*`)  # Each row in X is multiplied by corresponding weight
  Wy <- y * weights                # Each element of y is multiplied by corresponding weight
  
  # Calculate f_hat
  f_hat <- c(1, z) %*% solve(t(WX) %*% X) %*% t(WX) %*% Wy
  return(f_hat)
}


make_weight_matrix <- function(z, x, omega) {
  W <- function(r) {
    ifelse(abs(r) < 1, (1 - abs(r)^3)^3, 0)
  }
  distances <- abs(x - z) / omega
  weights <- W(distances)
  return(weights)  # Return weights as a vector
}


# Below is our task to write predictor matrix function:
make_predictor_matrix = function(x) {
  # Create the predictor matrix X with the first column as 1's and the second as x
  n = length(x)
  X = cbind(rep(1, n), x)
  return(X)
}