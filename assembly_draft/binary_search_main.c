#include <inttypes.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int32_t binary_search(int32_t *A, int32_t n, int32_t T);

int main()
{
    int arr[] = {1,2,3,5,6,7,8,9};
    int arr2[] = {1,2,3,5,6,7,8,9,11,16,17,32};
    if (1 == binary_search(arr, 8, 5) &&
        0 == binary_search(arr, 8, 4) &&
        0 == binary_search(arr2, 12, 4) &&
        0 == binary_search(arr2, 12, 15) &&
        1 == binary_search(arr2, 12, 16)
        ) {
        printf("success\n");
        return 0;
    } else {
        printf("FAIL\n");
        exit(EXIT_FAILURE);
    }
}