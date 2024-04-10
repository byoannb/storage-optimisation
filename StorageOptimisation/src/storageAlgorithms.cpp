#include <Rcpp.h> //to use the NumericVector object
using namespace Rcpp; //to use the NumericVector object
using namespace std;

#include <vector>
#include <algorithm>
#include <numeric>
#include <limits>
#include <iterator>


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

  return best_bins;
}




int borne_inf(std::vector<int> j, int mem)
{
  double sum = 0;
  for (int i = 0; i < j.size(); ++i) {
    sum = sum + j[i];
  }
  return std::ceil(sum / mem);
}




// int bfd(std::vector<int> j, int mem) {
//   int n = j.size();
//   std::vector<int> stockages(n, 0);
//   int num_stockages = 0;
//   for (int i = 0; i < n; ++i) {
//     if (num_stockages == 0) {
//       stockages[num_stockages++] = j[i];
//       continue;
//     }
//     int best_bin = 0;
//     int best_fit = std::numeric_limits<int>::max();
//     for (int j = 0; j < num_stockages; ++j) {
//       if (j[i] <= mem - stockages[j] && stockages[j] < best_fit) {
//         best_fit = stockages[j];
//         best_bin = j;
//       }
//     }
//     if (best_fit == std::numeric_limits<int>::max()) {
//       stockages[num_stockages++] = j[i];
//     } else {
//       stockages[best_bin] += j[i];
//     }
//   }
//   return num_stockages;
// }



// int branch_and_bound(std::vector<int> jeux, int m) {
//   std::sort(jeux.begin(), jeux.end(), std::greater<int>());
//   int best_sol = jeux.size();
//   int bfd_sol = bfd(jeux, m);
//   if (bfd_sol < best_sol) {
//     best_sol = bfd_sol;
//   }
//   std::vector<std::vector<int>> stack;
//   stack.push_back({0, 0, 0, borne_inf(jeux, m)});
//   while (!stack.empty()) {
//     auto node = stack.back();
//     stack.pop_back();
//     if (node[3] >= best_sol) {
//       continue;
//     }
//     if (node[2] + node[3] >= best_sol) {
//       continue;
//     }
//     if (node[2] + node[3] < best_sol) {
//       best_sol = node[2] + node[3];
//     }
//     if (node[1] == jeux.size()) {
//       continue;
//     }
//     for (int i = 0; i < jeux.size(); ++i) {
//       int new_bins = node[0];
//       int new_num_bins = node[1];
//       new_bins += jeux[i];
//       ++new_num_bins;
//       if (new_num_bins + borne_inf(std::vector<int>(jeux.begin() + i + 1, jeux.end()), m) >= best_sol) {
//         new_bins -= jeux[i];
//         --new_num_bins;
//         break;
//       }
//       std::vector<int> new_node = {new_bins, new_num_bins, 0, borne_inf(std::vector<int>(jeux.begin() + i + 1, jeux.end()), m)};
//       stack.push_back(new_node);
//       new_bins -= jeux[i];
//       --new_num_bins;
//     }
//   }
//   return best_sol;
// }
//
