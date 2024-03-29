##  GPL-3 License
## Copyright (c) 2024 Yoann Bonnet & Victorien Leconte & Hugo Picard

#' naive storage optimisation algorithm
#'
#' @description Optimize the storage of games
#' @param storage a vector of storage facilities' sizes
#' @param games a vector of games' sizes
#' @return the matrix of storaged games
naive_storage_opti <- function(storage, games){

  games = sort(games, decreasing = TRUE)
  storages = sort(storages, decreasing = TRUE)

  i_mem = 1
  j_jeu = 1
  mat = matrix(0, nrow = length(storages), ncol = length(games))
  colnames(mat) = games
  rownames(mat) = storages

  while(length(games) != 0 && i_mem <= length(storages) && j_jeu <= length(games)){
    if (storages[i_mem] - games[j_jeu] >= 0){
      storages[i_mem] = storages[i_mem] - games[j_jeu]
      mat[i_mem, j_jeu] = 1
      j_jeu = j_jeu + 1
    }
    else {
      i_mem = i_mem + 1
    }
  }

  cat(sum(rowSums(mat) != 0), "out of", length(storages), "storage facilities were used \n")
  cat(sum(colSums(mat)), "out of", length(games), "games could be stored")
  return(mat)
}
