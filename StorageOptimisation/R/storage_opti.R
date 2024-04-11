## GPL-3 License
## Copyright (c) 2024 Yoann Bonnet & Victorien Leconte & Hugo Picard

#' naive storage optimisation algorithm
#'
#' @description Optimize the storage of games
#' @param jeux a vector of storage facilities' sizes
#' @param taille_memoire a vector of games' sizes
#' @return a list containing the minimum number of storages used and the
#' distribution of each game in each used storage
naive_storage_opti <- function(jeux, taille_memoire) {

  # Fonction pour générer toutes les permutations
  generate_permutations <- function(elements) {
    if (length(elements) <= 1) {
      return(list(elements))
    } else {
      perms <- list()
      for (i in 1:length(elements)) {
        current_element <- elements[i]
        remaining_elements <- elements[-i]
        sub_perms <- generate_permutations(remaining_elements)
        for (perm in sub_perms) {
          perms <- c(perms, list(c(current_element, perm)))
        }
      }
      return(perms)
    }
  }

  # Générer toutes les permutations possibles des jeux
  permutations <- generate_permutations(jeux)
  best_bins <- list()

  memoire_minimale <- Inf  # Initialisation à une valeur infinie

  # Pour chaque permutation
  for (permutation in permutations) {
    bins <- list()
    memoires <- rep(taille_memoire, length(jeux))  # Initialise toutes les mémoires avec la même taille
    nombre_memoires <- 0

    # Essayer de placer chaque jeu dans une mémoire
    for (i in seq_along(permutation)) {
      jeu <- permutation[i]
      fitted <- FALSE
      for (j in seq_along(bins)) {
        if (sum(bins[[j]]) + jeu <= taille_memoire) {
          bins[[j]] <- c(bins[[j]], jeu)
          memoires[j] <- memoires[j] - jeu
          fitted <- TRUE
          break
        }
      }

      if (!fitted) {
        bins <- c(bins, list(jeu))
        memoires <- c(memoires, taille_memoire - jeu)
        nombre_memoires <- nombre_memoires + 1
      }
    }

    # Calculer le nombre de mémoires utilisées
    nombre_memoires <- sum(taille_memoire - memoires > 0)

    # Mettre à jour le nombre minimal de mémoires nécessaires
    if (nombre_memoires < memoire_minimale){
      memoire_minimale <- nombre_memoires
      best_bins <- bins
    }

  }

  # Créer une liste pour stocker les jeux dans chaque stockage
  jeux_dans_stockages <- list()
  for (bin in best_bins) {
    jeux_dans_stockages <- c(jeux_dans_stockages, list(bin))
  }

  return(list(memoire_minimale = length(jeux_dans_stockages), jeux_dans_stockages = jeux_dans_stockages))
}




borne_inf <- function(items, bin_size) {
  return(ceiling(sum(items) / bin_size))
}


bfd <- function(jeux, m) {
  n <- length(jeux)
  stockages <- list()
  for (i in 1:n) {
    if (length(stockages) == 0) {
      stockages[length(stockages) + 1] <- list(jeux[i])
      next
    }
    best_bin <- 0
    best_fit <- Inf
    for (j in 1:length(stockages)) {
      if (jeux[i] <= m - sum(unlist(stockages[[j]])) && sum(unlist(stockages[[j]])) < best_fit) {
        best_fit <- sum(unlist(stockages[[j]]))
        best_bin <- j
      }
    }
    if (best_fit == Inf) {
      stockages[length(stockages) + 1] <- list(jeux[i])
    } else {
      stockages[[best_bin]] <- c(stockages[[best_bin]], jeux[i])
    }
  }
  return(stockages)
}


#' branch and bound algorithm to optimize storage
#'
#' @description Optimize the storage of games using branch and bound algorithm
#' @param jeux a vector of storage facilities' sizes
#' @param m a vector of games' sizes
#' @return the minimum number of storages used
branch_and_bound <- function(jeux, m) {
  if(sum(jeux>m/2)==length(jeux)){
    return(length(jeux))
  }
  n <- length(jeux)
  jeux <- sort(jeux, decreasing = TRUE)
  best_sol <- n
  bfd_sol <- length(bfd(jeux, m))
  if (bfd_sol < best_sol) {
    best_sol <- bfd_sol
  }
  stack <- list()
  stack[[1]] <- list(level = 0, items = jeux, bins = numeric(n), num_bins = 0, bound = borne_inf(jeux, m))
  while (length(stack) > 0) {
    node <- stack[[length(stack)]]
    stack <- stack[-length(stack)]

    if (length(node$items) == 0) {
      next
    }
    # Exécuter l'algorithme BFD
    bfd_node <- bfd(node$items, m)
    if (length(bfd_node) < best_sol) {
      best_sol <- length(bfd_node)
    }
    new_bins <- node$bins
    new_num_bins <- node$num_bins
    for (i in 1:length(node$items)) {
      new_bins[i] <- new_bins[i] + node$items[1]
      new_num_bins <- new_num_bins + 1
      if (new_num_bins + borne_inf(node$items[-1], m) >= best_sol) {
        new_bins[i] <- new_bins[i] - node$items[1]
        new_num_bins <- new_num_bins - 1
        break
      }
      new_items <- node$items[-1]
      new_bound <- borne_inf(new_items, m)
      new_node <- list(level = node$level + 1, items = new_items, bins = new_bins, num_bins = new_num_bins, bound = new_bound)
      stack <- c(stack, list(new_node))
      new_bins[i] <- new_bins[i] - node$items[1]
      new_num_bins <- new_num_bins - 1

      if (node$bound >= best_sol) {
        next
      }
      if (node$num_bins + node$bound >= best_sol) {
        next
      }
      if (node$num_bins + node$bound < best_sol) {
        best_sol <- node$num_bins + node$bound
      }
    }
  }
  return(best_sol)
}
