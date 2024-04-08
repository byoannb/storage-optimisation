##  GPL-3 License
## Copyright (c) 2024 Yoann Bonnet & Victorien Leconte & Hugo Picard

#' First-fit-decreasing bin packing algorithm
#'
#' @description Optimize the storage of games
#' @param storage a vector of storage facilities' sizes
#' @param games a vector of games' sizes
#' @return the matrix of storaged games
ffd_bin_packing <- function(games, storage) {
  sorted_games <- sort(games, decreasing = TRUE)
  bins <- list()

  for (game in sorted_games) {
    fitted <- FALSE
    for (i in seq_along(bins)) {
      if (sum(bins[[i]]) + game <= storage) {
        bins[[i]] <- c(bins[[i]], game)
        fitted <- TRUE
        break
      }
    }

        if (!fitted) {
      bins <- c(bins, list(game))
    }
  }

  return(bins)
}
