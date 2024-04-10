## GPL-3 License
## Copyright (c) 2024 Yoann Bonnet & Victorien Leconte & Hugo Picard

#' naive storage optimisation algorithm
#'
#' @description Optimize the storage of games
#' @param jeux a vector of storage facilities' sizes
#' @param taille_memoire a vector of games' sizes
#' @return the matrix of storaged games
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

  return(list(memoire_minimale = memoire_minimale, jeux_dans_stockages = jeux_dans_stockages))
}
