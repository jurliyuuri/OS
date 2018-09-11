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
	uint32_t f0 = 0;
	int32_t f1 = 15;
	uint32_t f3;

	f:
	if(f1 < 0) {
		goto F;
	}
	uint32_t f2 = f0;
	f1++;
	f2 <<= f1;
	f1--;
	f1 <<= 1;
	f3 = 1;
	f3 <<= f1;
	f2 += f3;
	f1 >>= 1;
	if(input < f2){
		goto t;
	}
	f3 = 1;
	f3 <<= f1;
	f0 += f3;
	input -= f2;
		
	t:
	f1--;
	goto f;

	F:
	return f0;
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
	test(9752);
	
	return 0;
}
