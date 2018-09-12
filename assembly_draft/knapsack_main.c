#include <stdio.h>


int knapsack(int n, int W, int *weight, int *value);

int main(void)
{
  {
    int n = 5;
    int W = 15;
    int weight[] = {1, 3, 6, 9, 13};
    int value[] = {2000, 8400, 17200, 25800, 40000};
    printf("%d\n", knapsack(n, W, weight, value));
  }

  {
    int n = 5;
    int W = 18;
    int weight[] = {1, 3, 6, 9, 13};
    int value[] = {2000, 8400, 17200, 25800, 40000};
    printf("%d\n", knapsack(n, W, weight, value));
  }
  {
    int n = 6;
    int W = 8;
    int weight[] = {2,1,3,2,1,5};
    int value[] = {3,2,6,1,3,85};
    printf("%d\n", knapsack(n, W, weight, value));
  }
  return 0;
}