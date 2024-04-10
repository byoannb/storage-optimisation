#include <Rcpp.h> //to use the NumericVector object
using namespace Rcpp; //to use the NumericVector object
using namespace std;

#include<vector> //to use std::vector<double>
#include <iostream>
#include <algorithm>
#include <numeric>

//' First-fit-decreasing bin packing algorithm
//' @param items A vector containing the sizes of items to be packed.
//' @param bin_size The maximum capacity of each bin.
//' @return A vector of vectors representing the bins, where each inner vector
//'         contains the sizes of items packed into a single bin.
//' @export
// [[Rcpp::export]] //mandatory to export the function
std::vector<std::vector<int>> ffd_bin_packing_Rcpp(std::vector<int>& games, int storage)
  {
    sort(games.begin(), games.end(), greater<int>());

    vector<vector<int>> bins;

    for (int game : games) {
        bool fitted = false;
        for (vector<int>& bin : bins) {
            if (accumulate(bin.begin(), bin.end(), 0) + game <= storage) {
                bin.push_back(game);
                fitted = true;
                break;
            }
        }

        if (!fitted) {
            bins.push_back({game});
        }
    }

    return bins;
}