#include  <iostream>

#define   FREE_ARG char*
#define   NR_END 1
#define	  NTAB 32
#define	  IA 16807
#define   IM 2147483647
#define   AM (1.0/IM)
#define   IQ 127773
#define   IR 2836
#define   NDIV (1+(IM -1)/NTAB)
#define   RNMX (1.0-EPS)
#define   EPS 1.2e-7
#define   seed_net   1000
#define	  INF 9999

using namespace std;

//*********************//
//***** FUNCTIONS *****//
//*********************//

//Parameter Input
//Initialization

// a routine that makes a matrix using pointer. (from "Numerical Recipes in C")
short int **matrix(long nrl, long nrh, long ncl, long nch)
{
	long i, nrow=nrh-nrl+1,ncol=nch-ncl+1;
	short int **m;
	//allocate pointers to rows
	m=(short int **) malloc((size_t)((nrow+NR_END)*sizeof(int*)));
	m += NR_END;
	m -= nrl;

	//allocate rows and set pointers to them
	m[nrl]=(short int *) malloc((size_t)((nrow*ncol+NR_END)*sizeof(int)));
	m[nrl] += NR_END;
	m[nrl] -= ncl;

	for(i=nrl+1;i<=nrh;i++) m[i]=m[i-1] + ncol;

	return m;
}

long *lvector(long nl , long nh)
{
	long *v;

	v=(long *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(long)));
	//if (!v) nrerror("allocation failure  in lvector()");
	return v-nl+NR_END;
}
int *ivector(long nl , long nh)
{
	int *v;

	v=(int *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(int)));
	//if (!v) nrerror("allocation failure  in ivector()");
	return v-nl+NR_END;
}
void free_lvector(long *v, long nl, long nh)
{
	free((FREE_ARG) (v+nl-NR_END));
}
void free_ivector(int *v, long nl, long nh)
{
	free((FREE_ARG) (v+nl-NR_END));
}
// free the memory which is held by matrix.(from "Numerical Recipes in C")
void free_matrix(short int **m, long nrl, long nrh, long ncl, long nch)
{
	free((FREE_ARG)(m[nrl]+ncl-NR_END));
	free((FREE_ARG)(m+nrl-NR_END));
}
// print out the matrix
void show_matrix (short int **m, long nrl, long nrh, long ncl, long nch)
{
	int i,j;
	
	cout<<endl<<"  ";
	for(i=ncl;i<=nch;i++) cout<<"  "<<i<<"  ";
	cout<<endl;
	for (i=nrl;i<=nrh;i++){
		cout<<i;
		for(j=ncl;j<=nch;j++) cout<<"  "<<m[i][j];
		cout<<endl;
	};
}
// set default 'value' to the matrix
void initialize_matrix (short int **m,long nrl,long nrh, long ncl, long nch, int value)
{
	int l,n;
	
	for (l=nrl;l<=nrh;l++)
			{
				for (n=ncl;n<=nch;n++)
				{
					m[l][n]=value;
				}

			}
}
void initialize_dmatrix (double **m,long nrl,long nrh, long ncl, long nch, double value)
{
	int l,n;
	
	for (l=nrl;l<=nrh;l++)
			{
				for (n=ncl;n<=nch;n++)
				{
					m[l][n]=value;
				}

			}
}
void initialize_f3tensor (float ***m,long nrl,long nrh, long ncl, long nch, long ndl,long ndh, float value)
{
	int l,n,q;
	
	for (l=nrl;l<=nrh;l++)
			{
				for (n=ncl;n<=nch;n++)
				{
					for (q=ndl;q<=ndh;q++)
					{
						m[l][n][q]=value;
					}
				}

			}
}

void shortestpath_matrix (short int **m, long nrl,long nrh, long ncl, long nch)
{
	int i,j,k;
	int count;
	cout<<endl<<"start min path length calculation.."<<endl;
	count=0;
	for (i=nrl;i<=nrh;i++)
		for(j=ncl;j<=nch;j++)
			for(k=nrl;k<=nrh;k++)
				if(m[j][i]+m[i][k]<m[j][k])
				{
					//cout<<endl<<m[j][i]<<" "<<m[i][k]<<"<?"<<m[j][k];
					m[j][k]=m[j][i]+m[i][k];
					count=count+1;
	//				cout<<endl<<count<<"  "<<i<<", "<<j<<"="<<m[j][k];
				}

	//set the diagonal value to be zero
	for (i=nrl;i<=nrh;i++) m[i][i]=0;

}




void Export_matrix (short int **m, long nrl,long nrh, long ncl,long nch, FILE *ofp)
{
	int i,j;
	
	for (i=1;i<=nrh;i++){
		for(j=i;j<=nch;j++){
			if(m[i][j]>=1){
				fprintf(ofp,"%d,%d\n",i,j);
			}
		}
	};
	for (i=1;i<=nrh;i++){
		for(j=i;j<=nch;j++){
			if(m[i][j]>=1){
				fprintf(ofp,"%d,%d\n",j,i);
			}
		}
	};


}


double **dmatrix(long nrl, long nrh, long ncl, long nch)
{
	long i, nrow=nrh-nrl+1,ncol=nch-ncl+1;
	double **m;
	//allocate pointers to rows
	m=(double **) malloc((size_t)((nrow+NR_END)*sizeof(double*)));
	m += NR_END;
	m -= nrl;

	//allocate rows and set pointers to them
	m[nrl]=(double *) malloc((size_t)((nrow*ncol+NR_END)*sizeof(double)));
	m[nrl] += NR_END;
	m[nrl] -= ncl;

	for(i=nrl+1;i<=nrh;i++) m[i]=m[i-1] + ncol;

	return m;
}

void free_dmatrix(double **m, long nrl, long nrh, long ncl, long nch)
{
	free((FREE_ARG)(m[nrl]+ncl-NR_END));
	free((FREE_ARG)(m+nrl-NR_END));
}

float ***f3tensor(long nrl, long nrh, long ncl, long nch, long ndl, long ndh)
{
	long i,j,nrow=nrh-nrl+1,ncol=nch-ncl+1,ndep=ndh-ndl+1;
	float ***t;

	t=(float ***) malloc ((size_t)((nrow+NR_END)*sizeof(float**)));
	if (!t) cout<<endl<<"allocation failure 1 in f3tensor()"<<endl;
	t+=NR_END;
	t-=nrl;

	t[nrl]=(float **) malloc ((size_t)((nrow*ncol+NR_END)*sizeof(float*)));
	if(!t[nrl]) cout<<endl<<"allocation failure 2 in f3tensor()"<<endl;

	t[nrl]+=NR_END;
	t[nrl]-=ncl;

	t[nrl][ncl]=(float *) malloc ((size_t)((nrow*ncol*ndep + NR_END)*sizeof(float)));
	if(!t[nrl][ncl]) cout<<endl<<"allocation failure 3 in f3tensor()"<<endl;

	t[nrl][ncl]+=NR_END;
	t[nrl][ncl]-=ndl;

	for(j=ncl+1;j<=nch;j++) t[nrl][j]=t[nrl][j-1]+ndep;
	for(i=nrl+1;i<=nrh;i++)
	{
		t[i]=t[i-1]+ncol;
		t[i][ncl]=t[i-1][ncl]+ncol*ndep;
		for (j=ncl+1;j<=nch;j++) t[i][j]=t[i][j-1]+ndep;
	}

	return t;
}

void free_f3tensor(float ***t,long nrl, long nrh, long ncl, long nch, long ndl, long ndh)
{
	free((FREE_ARG) (t[nrl][ncl]+ndl-NR_END));
	free((FREE_ARG) (t[nrl]+ncl-NR_END));
	free((FREE_ARG) (t+nrl-NR_END));
}
