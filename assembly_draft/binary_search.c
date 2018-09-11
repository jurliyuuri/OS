#include <inttypes.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int32_t binary_search(int32_t *A, int32_t n, int32_t T){
    int32_t L = 0;
    int32_t R = n - 1;
    while (L <= R) {
        int32_t m = (L + R) / 2;
        if (A[m] < T) {
            L = m + 1;
        } else if (A[m] > T) {
            R = m - 1;
        } else {
            return 1;
        }
    }
    return 0;
}
