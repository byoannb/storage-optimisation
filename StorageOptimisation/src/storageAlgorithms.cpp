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




 // Taille maximale d'un objet
#define MAX_SIZE 100

 int n; // nombre d'objets
 vector<int> sizes; // tableau des tailles des objets
 vector<int> bins; // tableau des tailles des bacs
 vector<int> best_bins; // meilleure solution trouvée
 int best_num_bins; // nombre de bacs dans la meilleure solution

 // Trie les objets par taille décroissante
 void sort_items() {
   sort(sizes.begin(), sizes.end(), greater<int>());
 }

 // Vérifie si un objet peut être ajouté à un bac
 bool can_add_item(int bin, int item) {
   return bins[bin] + sizes[item] <= MAX_SIZE;
 }

 // Ajoute un objet à un bac
 void add_item(int bin, int item) {
   bins[bin] += sizes[item];
 }

 // Supprime un objet d'un bac
 void remove_item(int bin, int item) {
   bins[bin] -= sizes[item];
 }

 // Recherche la meilleure solution en utilisant l'algorithme de Branch and Bound
 void branch_and_bound(int item, int num_bins, int max_size) {
   // Cas de base : tous les objets ont été affectés à un bac
   if (item == n) {
     // Mise à jour de la meilleure solution
     if (num_bins < best_num_bins) {
       best_num_bins = num_bins;
       best_bins.assign(bins.begin(), bins.begin() + num_bins);
     }
     return;
   }

   // Bornes inférieure et supérieure pour le nombre de bacs nécessaires
   int lower_bound = num_bins;
   int upper_bound = best_num_bins;
   int total_size = 0;
   for (int i = item; i < n; i++) {
     total_size += sizes[i];
     upper_bound = max(upper_bound, (total_size + max_size - 1) / max_size);
   }

   // Vérification de la borne supérieure
   if (lower_bound >= upper_bound) return;

   // Affectation de l'objet courant au premier bac possible
   for (int bin = 0; bin < num_bins; bin++) {
     if (can_add_item(bin, item)) {
       add_item(bin, item);
       branch_and_bound(item + 1, num_bins, max_size);
       remove_item(bin, item);
       return;
     }
   }

   // Ajout d'un nouveau bac et affectation de l'objet courant à ce bac
   if (num_bins < best_num_bins) {
     bins[num_bins] = sizes[item];
     branch_and_bound(item + 1, num_bins + 1, max_size);
     bins[num_bins] = 0;
   }
 }

 //' Insertion sort algorithm using C++
 //'
 //' @param jeux an unsorted vector of numeric data
 //' @param m an integer corresponding to the memory size
 //' @return a sorted vector
 //' @export
 // [[Rcpp::export]] //mandatory to export the function
 std::vector<int> solve_bin_packing(std::vector<int> c, int max_bin_size) {
   n = c.size();
   sizes.assign(c.begin(), c.end());
   bins.assign(n, 0);
   best_bins.clear();
   best_num_bins = numeric_limits<int>::max();
   int max_size = max_bin_size;
   sort_items();
   branch_and_bound(0, 0, max_size);
   return best_bins;
 }

