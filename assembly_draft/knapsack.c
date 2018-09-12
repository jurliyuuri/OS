#include <stdio.h>


int knapsack(int n, int W, int *weight, int *value) {
  int buf1_[101] = {0};
  int buf2_[101];
  int *buf1 = buf1_;
  int *buf2 = buf2_;

  for (int i = 0; ; ++i) {
    for (int w = 0; w <= W; ++w) {
      if (w >= weight[i]) {
        int a = buf1[w-weight[i]] + value[i];
        int b = buf1[w];
        buf2[w] = a > b ? a : b;
      } else {
        buf2[w] = buf1[w];
      }
    }
    if (i == n-1) {
      return buf2[W];
    } else {
      int *tmp = buf1;
      buf1 = buf2;
      buf2 = tmp;
    }
  }
}

