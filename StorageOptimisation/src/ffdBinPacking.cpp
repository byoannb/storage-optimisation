#include <Rcpp.h> //to use the NumericVector object
using namespace Rcpp; //to use the NumericVector object
using namespace std;

#include<vector> //to use std::vector<double>
#include <iostream>
#include <algorithm>
#include <numeric>

//' @param items A vector containing the sizes of items to be packed.
//' @param bin_size The maximum capacity of each bin.
//' @return A vector of vectors representing the bins, where each inner vector
//'         contains the sizes of items packed into a single bin.
//' @export
// [[Rcpp::export]] //mandatory to export the function
std::vector<std::vector<int>> ffd_bin_packing(std::vector<int>& items, int bin_size) {
    sort(items.begin(), items.end(), greater<int>());

    vector<vector<int>> bins;

    for (int item : items) {
        bool fitted = false;
        for (vector<int>& bin : bins) {
            if (accumulate(bin.begin(), bin.end(), 0) + item <= bin_size) {
                bin.push_back(item);
                fitted = true;
                break;
            }
        }

        if (!fitted) {
            bins.push_back({item});
        }
    }

    return bins;
}