#include <stdio.h>

int tmp;

#define qss(fi, st, ir) (tmp = (st), (st) = (ir), (fi) = tmp)

void quicksort(int* const a, int l, int r)
{
	register int* const f0 = a;
	register int f1 = l; 
	register int f2 = r;
	register int f3;
	if(f1 >= f2) {
		goto fin;
	}
	f3 = f1;
	f3++;
	
panqa:
	if(f3 > r) {
		goto fistir;
	}
	f2 = l;
	f2 = f0[f2];
	if(f0[f3] >= f2) {
		goto iska;
	}
	f1++;
	qss(f0[f3],f0[f1],f0[f3]);
iska: 
	f3++;
	goto panqa;

fistir:
	f2 = l;
	qss(f0[f1],f0[f2],f0[f1]);

	int p = f1;
	quicksort(a,l,p-1);
	quicksort(a,p+1,r);
fin:	
	return;	
}

int dat[] = {3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3,2,3,8};

int main()
{
	quicksort(dat,0,18);
	for(int i=0;i<=18;i++){
		printf("%d, ",dat[i]);
	}
	printf("\n");
	return 0;
}
