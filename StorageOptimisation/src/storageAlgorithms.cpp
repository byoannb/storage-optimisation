#include <Rcpp.h> //to use the NumericVector object
using namespace Rcpp; //to use the NumericVector object
using namespace std;

#include <vector>
#include <algorithm>
#include <functional>
#include <tuple>
#include <deque>
#include <limits>
#include <numeric>

#include <cmath>


std::vector<std::vector<int>> generate_permutations(std::vector<int> elements) {
  if (elements.size() <= 1) {
    return {elements};
  } else {
    std::vector<std::vector<int>> perms;
    for (int i = 0; i < elements.size(); i++) {
      int current_element = elements[i];
      vector<int> remaining_elements;
      remaining_elements.reserve(elements.size() - 1);
      for (int j = 0; j < elements.size(); j++) {
        if (j != i) {
          remaining_elements.push_back(elements[j]);
        }
      }
      auto sub_perms = generate_permutations(remaining_elements);
      for (auto perm : sub_perms) {
        perm.insert(perm.begin(), current_element);
        perms.push_back(perm);
      }
    }
    return perms;
  }
}



//' Insertion sort algorithm using C++
//'
//' @param j an unsorted vector of numeric data
//' @param mem an integer corresponding to the memory size
//' @return a sorted vector
//' @export
// [[Rcpp::export]] //mandatory to export the function
std::vector<std::vector<int>> naive_storage_Rcpp(std::vector<int> j, int mem) {
  std::vector<std::vector<int>> permutations = generate_permutations(j);

  int memoire_minimale = numeric_limits<int>::max();
  std::vector<std::vector<int>> best_bins;

  for (const auto& permutation : permutations) {
    std::vector<int> memoires(j.size(), mem); // Initialise toutes les mémoires avec la même taille
    int nombre_memoires = 0;
    std::vector<vector<int>> bins(j.size());

    for (int i = 0; i < permutation.size(); i++) {
      int jeu = permutation[i];
      bool fitted = false;
      for (int k = 0; k < bins.size(); k++) {
        if (accumulate(bins[k].begin(), bins[k].end(), 0) + jeu <= mem) {
          bins[k].push_back(jeu);
          memoires[k] -= jeu;
          fitted = true;
          break;
      }
    }
      if (!fitted) {
        bins[nombre_memoires].push_back(jeu);
        memoires[nombre_memoires] -= jeu;
        nombre_memoires++;
    }
  }

    nombre_memoires = count_if(memoires.begin(), memoires.end(), [mem](int m) { return m < mem; });

    if (nombre_memoires < memoire_minimale) {
      memoire_minimale = nombre_memoires;
      best_bins = bins;
    }
  }

  best_bins.erase(remove_if(best_bins.begin(), best_bins.end(), [](const vector<int>& v) { return v.empty(); }), best_bins.end());

  return best_bins;
}



std::vector<std::vector<int>> bfd(const std::vector<int>& items, int m) {
  int n = items.size();
  std::vector<std::vector<int>> bins; // Stores items in each bin

  for (int i = 0; i < n; ++i) {
    int best_bin = -1;
    int best_fit = std::numeric_limits<int>::max(); // Initialize with positive infinity

    // Find the best fitting bin for the current item
    for (size_t j = 0; j < bins.size(); ++j) {
      int current_sum = std::accumulate(bins[j].begin(), bins[j].end(), 0);
      if (items[i] <= m - current_sum && current_sum < best_fit) {
        best_fit = current_sum;
        best_bin = j;
      }
    }

    // If no bin can fit the item, create a new bin
    if (best_bin == -1) {
      bins.push_back(std::vector<int>{items[i]});
    } else {
      // Add the item to the best fitting bin
      bins[best_bin].push_back(items[i]);
    }
  }

  return bins;
}


// Function to calculate a lower bound (replace with your actual implementation)
int lower_bound(const std::vector<int>& items, int bin_size) {
  int sum = 0;
  for (int item : items) {
    sum += item;
  }
  return std::ceil(static_cast<double>(sum) / bin_size);
}


 //' Insertion sort algorithm using C++
 //'
 //' @param jeux an unsorted vector of numeric data
 //' @param m an integer corresponding to the memory size
 //' @return a sorted vector
 //' @export
 // [[Rcpp::export]] //mandatory to export the function
int branch_and_bound_Rcpp(const std::vector<int>& jeux, int m) {
   int n = jeux.size();
   std::vector<int> sorted_jeux(jeux);
   // Trier jeux dans l'ordre décroissant
   std::sort(sorted_jeux.begin(), sorted_jeux.end(), std::greater<int>());

   int best_sol = n;

   std::deque<std::tuple<int, std::size_t, std::vector<int>, int, int>> stack;

   int current_depth = 0;
   std::size_t sorted_copy_size = sorted_jeux.size();
   int current_bound = lower_bound(sorted_jeux, m);

   std::tuple<int, std::size_t, std::vector<int>, int, int> state(current_depth, sorted_copy_size, std::vector<int>(n), 0, current_bound);
   stack.push_back(state);

   while (!stack.empty()) {
     std::tuple<int, std::size_t, std::vector<int>, int, int> node = stack.back();
     stack.pop_back();

     if (std::get<1>(node) < 1) {
       continue;
     }

     // Utiliser la fonction bfd fournie pour le bin packing
     std::vector<std::vector<int>> bins = bfd(std::get<2>(node), m);
     int bfd_sol = bins.size();

     if (bfd_sol < best_sol) {
       best_sol = bfd_sol;
     }

     std::vector<int> new_bins = std::get<2>(node);
     int new_num_bins = std::get<3>(node);
     for (int i = 0; i < std::get<2>(node).size(); ++i) {
       std::vector<int> new_bins = std::get<2>(node); // Copie des bacs actuels
       new_bins[i] += std::get<2>(node)[0]; // Ajout de l'élément actuel au bac i
       int new_num_bins = std::get<3>(node) + 1; // Mise à jour du nombre de bacs
       if (new_num_bins + lower_bound(std::vector<int>(std::get<2>(node).begin() + 1, std::get<2>(node).end()), m) >= best_sol) {
         continue; // Passer au bac suivant si la borne inférieure est trop élevée
       }
       std::vector<int> new_items(std::get<2>(node).begin() + 1, std::get<2>(node).end()); // Nouveaux éléments restants à traiter
       int new_bound = lower_bound(new_items, m); // Nouvelle borne inférieure
       std::tuple<int, std::size_t, std::vector<int>, int, int> new_node = {std::get<0>(node) + 1, std::get<1>(node), new_bins, new_num_bins, new_bound};
       stack.push_back(new_node); // Ajouter le nouveau nœud à la pile
     }


     if (std::get<4>(node) >= best_sol) {
       continue;
     }
     if (std::get<3>(node) + std::get<4>(node) >= best_sol) {
       continue;
     }
     if (std::get<3>(node) + std::get<4>(node) < best_sol) {
       best_sol = std::get<3>(node) + std::get<4>(node);
     }
   }
   return best_sol;
 }

