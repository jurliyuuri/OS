#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>

/* 
 * basic algorithm: 
 *  Let INPUT signify the original input.
 *  Before each iteration, the variable `input` contains INPUT - accum * accum.
 *  Within each iteration, 
 *   it checks whether INPUT - (accum+(1<<tbit)) * (accum+(1<<tbit)) becomes non-negative.
 *  That is, whether input >= (accum<<(tbit+1)) + (1<<(tbit*2)) is true or not 
 *   (for convenience, name the right-hand side `p`.)
 *  If false, tbit--;
 *  If true, 
 *   1. accum += 1<<tbit; 
 *   2. Since we want `input` to be INPUT - accum * accum, input -= p;
 *   3. tbit--;
 *
 */

/* returns floor(sqrt(input)). */
uint32_t lakormeru(uint32_t input)
{
	uint32_t accum = 0;
	int tbit = 15;

	while(tbit >= 0){
		uint32_t p = (accum<<(tbit+1)) + (1<<(tbit*2));
		if(input < p){
			tbit--;
			continue;
		} else {
			accum += (1 << tbit);
			input -= p;
			tbit--;
		}
	}
	return accum;
}


void test(uint32_t num)
{
	uint32_t a = num*num;
	printf("%" PRIu32 "\n", lakormeru(a-1));
	printf("%" PRIu32 "\n", lakormeru(a)); 
	printf("%" PRIu32 "\n", lakormeru(a+1));
}

int main()
{
	test(32453);
	test(10099);
	test(1589);
	
	return 0;
}