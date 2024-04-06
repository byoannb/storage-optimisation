#include <Rcpp.h> //to use the NumericVector object
using namespace Rcpp; //to use the NumericVector object
using namespace std;

#include<vector> //to use std::vector<double>


vector<vector<int>> generate_permutations(vector<int> elements) {
  if (elements.size() <= 1) {
    return {elements};
  } else {
    vector<vector<int>> perms;
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
//' @param mem
//' @return a sorted vector
//' @export
// [[Rcpp::export]] //mandatory to export the function
int naive_storage_Rcpp(std::vector<int> j, int mem)
{
  vector<vector<int>> permutations;
  permutations = generate_permutations(j);

  int memoire_minimale = numeric_limits<int>::max();

  for (const auto& permutation : permutations) {
    vector<int> memoires(j.size(), mem); // Initialise toutes les mémoires avec la même taille
    int nombre_memoires = 0;

    for (int jeu : permutation) {
      for (int i = 0; i < memoires.size(); i++) {
        if (jeu <= memoires[i]) {
          memoires[i] -= jeu;
          break;
        }
      }
    }

    nombre_memoires = count_if(memoires.begin(), memoires.end(), [mem](int m) { return m < mem; });

    memoire_minimale = min(memoire_minimale, nombre_memoires);
  }

  return memoire_minimale;
}

