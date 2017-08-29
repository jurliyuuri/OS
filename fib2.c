#include <stdio.h>

int fib2(int a)
{
	register int f0 = a;
	register int f1;
	register int f2;

	if(f0 < 2) {
		goto fin;
	}

	f1 = f0;
	f1--;
	f0 = fib2(f1);
	f2 = f0;
	f1--;
	f0 = fib2(f1);
	f2 += f0;
	f0 = f2;

	fin:
	return f0;
}

int main()
{
	int a;
	scanf("%d",&a);
	printf("%d\n",fib2(a));
}
