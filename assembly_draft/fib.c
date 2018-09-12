#include <stdio.h>

int fib(int a)
{
	register int f0 = a;
	register int f1 = 0; 
	register int f2 = 1;
	register int f3;
	while(f0) {
		f0--;
		f3 = f1 + f2;
		f1 = f2;
		f2 = f3;
	}
	f0 = f1;
	return f0;
}

int main()
{
	int a;
	scanf("%d",&a);
	printf("%d\n",fib(a));
}