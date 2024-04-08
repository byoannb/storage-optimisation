#include <Rcpp.h> //to use the NumericVector object
using namespace Rcpp; //to use the NumericVector object
using namespace std;

#include<vector> //to use std::vector<double>

vector<vector<int>> ffd_bin_packing(vector<int>& items, int bin_size) {
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