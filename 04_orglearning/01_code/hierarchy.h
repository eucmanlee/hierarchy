#include "stat.h"
#include "determineSup.h"
#include "superiorBelief.h"
#include "matrix.h"
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
void initialize(short int** superior,short int** reality, short int** majorityView, short int** individuals, short int**correctAns, short int**neighbors, int m, int n)
{
	int i, j;
	
	//Reality
	for (j=1; j<m+1; j++)
	{
		reality[j][1] = rand()%2;
		//reality[j] = 1+(int)(2.0 * uniform()/(2.0));

		if(reality[j]==0)
		{reality[j][1] = -1;}
	}

	//Individuals
	for (i=1; i<n+1; i++)
	{
		for (j=1; j<m+1; j++)
		{
			individuals[i][j] = rand()%3 - 1;
			//This code will reduce the level of correct bits of each individuals
			if(individuals[i][j]!=0 && uniform()<0.0)
				{
					individuals[i][j]=0;
		
				}
			majorityView[i][j]=0;
			correctAns[i][j]=0;
		}
	}

	//Initialize Adjacency Matrix
	for (i=1; i<n+1; i++)
	{
		for (j=1; j<n+1; j++)
		{
			//adjacency[i][j]=0;
			superior[i][j]=0;
			neighbors[i][j]=0;
		}
	}

	cout<<endl<<"Initialized basic matrix";

}
void initialize2(short int ** superior, short int ** reality, short int ** majorityView, short int **individuals, short int **correctAns, double **score, double **score_lag, int m, int n)
{
	int i, j;

	//Reality
	for (j=1; j<m+1; j++)
	{
		reality[j][1] = rand()%2;
		if(reality[j][1]==0) {reality[j][1] = -1;}
	}//for end

	//Individuals
	for (i=1; i<n+1; i++)
	{
		for (j=1; j<m+1; j++)
		{
			individuals[i][j] = rand()%3 - 1;
			//This code will reduce the level of correct bits of each individuals
			if(individuals[i][j]!=0 && uniform()<0.0)
				{
					individuals[i][j]=0;
				
				}
			majorityView[i][j]=0;
			correctAns[i][j]=0;
		}//for end
	}//for end

	//Initialize Score and Superior Matrix
	for (i=1; i<n+1; i++)
	{
		score[i][1]=0;score_lag[i][1]=0;
		for (j=1; j<n+1; j++)
		{
			superior[i][j]=0;
		}//for end
	}//for end
}

void segcb(short int **individuals, short int **group, int m, int n, int seg_pr, int seg_gp)
{
	int i, j;

	
	//debug
	//cout<<endl<<"start erasing initial knowledge";
	
	//Individuals
	for (i=1; i<n+1; i++) {
		if (group[i][1]==seg_gp) {
		  //debug
		  
		  for(j=1;j<seg_pr+1;j++) {
		    //erase the first half
		    individuals[i][j]=0;
		  }
		}
		if (group[i][1]==seg_gp+1) {
		  for(j=seg_pr+1;j<m+1;j++) {
		    //erase the second half
		    individuals[i][j]=0;
		  }
		}
		if ((group[i][1]!=seg_gp) && (group[i][1]!=seg_gp)) {
		  for(j=1;j<m+1;j++) {
		    //erase all knowledge
		    individuals[i][j]=0;
		  }
		}
		//debug routine
		//cout<<endl<<"individual = "<<i<<" in group = "<<group[i][1]<<endl;
		//for(j=1;j<m+1;j++){
		//  cout<<individuals[i][j];
		//}
		//cout<<endl;
		//debug ends;
	}//for end

}

void initializeCorrectAns(short int** correctAns, int m, int n)
{
	//Reinitialize the Individual's Correct Answer
	for (int i=1; i<n+1; i++)
	{
		for (int j=1; j<m+1; j++)
		{
			correctAns[i][j]=0;
		}
	}
}

//Update Data
void updateLag(short int **superior, short int **individuals, short int **individuals_lag,double **score, double **score_lag,int m, int n)
{
	int i, j,k;

	//Update individuals_lag[i][j]
	for (i=1; i<n+1; i++)
	{
		score_lag[i][1] = score[i][1];
		for (j=1; j<m+1; j++)
		{
			individuals_lag[i][j] = individuals[i][j];
		}
		for (k=1; k<n+1; k++)
		{
			superior[i][k]=0;
		}
	}

}

void formHRG(short int **minpath, short int **adjacency, double pc, int numDraw, int tpop, int sc, int radius)
{
	int k=0, Dist_near=0,Dist_far=0, q=0, span_focal=0, \
		span_far=0, span_near=0, node_focal=0,node_far=0,node_near=0;
	double prob=0, rn=0;

	cout<<endl<<"Total number of random links to be drawn="<<numDraw;
	
	for (k=1;k<numDraw+1;k++){
		//Draw first nodes randomly
		node_focal=1+ (int)(tpop*(uniform())); 
		//j=1+ (int)(tpop*(uniform()));
		
		//Draw two others, one from the neighbors and the other from distant
		do{
			node_near=1+ (int)(tpop*(uniform()));
			Dist_near=minpath[node_focal][node_near];
		//	cout<<endl<<Dist_near;
			} while(Dist_near>radius && Dist_near==0);
			
		do{
			node_far =1+ (int)(tpop*(uniform()));
			Dist_far =minpath[node_focal][node_far ];
			} while (Dist_far <=radius && Dist_near==0);
			
		//cout<<endl<<"Focal node= "<<node_focal<<" neighbor ="<<node_near<<" and distant node = "<<node_far;

		//Calculate span of control of both node_focal, node_near and node_far
		span_focal=0;span_near=0; span_far=0;
		for (q=1;q<tpop+1;q++){
			if(adjacency[node_focal][q]==1) span_focal=span_focal+1;
			if(adjacency[node_near] [q]==1) span_near=span_near+1;
			if(adjacency[node_far ] [q]==1) span_far=span_far+1;
		}
		//random draw
		//Temporary fix
		span_focal= span_focal +1;
		span_near=  span_near  +1;
		span_far=   span_far   +1;
		rn=uniform();
		//debug
		//cout<<endl<<"upper limit="<<sc<<" span1="<<span1<<" span2="<<span2;
		//cout<<endl<<"Probability between "<<i<<","<<j<<" is "<<prob;
		//cout<<endl<<"the "<<k<<"th draw pair is already connected="<<adjacency[i][j];
		//cout<<endl<<"random draw = "<<rn;
		//add new connection if span is under limit
		//if no existing links
		//
		
		if(rn<=pc) {
			if(adjacency[node_focal][node_near]!=1){
				if((span_focal<=sc) &&(span_near<=sc)){
					adjacency[node_focal][node_near ]=1;
					adjacency[node_near ][node_focal]=1;
				}
			}
		}
		if(rn> pc) {
			if(adjacency[node_focal][node_far]!=1){
				if((span_focal<=sc) &&(span_far<=sc)){
					adjacency[node_focal][node_far ]=1;
					adjacency[node_far ][node_focal]=1;
				}
			}
		}
	}
}

void evolveNET( short int **adjacency, double pc, int tpop, int sc, short int** neighbors, short int**superior, double **score)
{
	int k=0, Dist_near=0,Dist_far=0, q=0, span_focal=0, \
		span_far=0, span_near=0, node_focal=0,node_far=0,node_near=0;
	int change;
	int node_old=0, node_new=0, i,j;
	int neighbor_count=0;
	int searchcount=0;
	double prob=0, rn=0;
	

	
	for(i=1;i<tpop+1;i++){
		//change=determineSup(i, superior,neighbors,score,tpop);
		cout<<endl<<"num superior of"<<i<<" = "<<superior[i][tpop+1];
		if(superior[i][tpop+1]<2){
			cout<<endl<<"node = "<<i;
			searchcount=0;
			if(uniform()<pc){
				do{
				searchcount++;
				node_old=neighbors[i][1+(int)(neighbors[i][tpop+1]*uniform())];
				node_new=1+ (int)(tpop*(uniform()));
					cout<<endl<<"new node = "<<node_new;
					cout<<endl<<"old node = "<<node_old;
					cout<<endl<<"adj old= "<<adjacency[i][node_old];
					cout<<endl<<"adj new= "<<adjacency[i][node_new];
					cout<<endl<<"new node sc = "<<neighbors[node_new][tpop+1];
				if (searchcount>10) break;
				} while(adjacency[i][node_new] !=0 || neighbors[node_new][tpop+1]>sc ||node_old==0||node_new==0);	
				
				
				if (searchcount<11){
					cout<<endl<<"now severing and wiring";
					cout<<endl<<"new node in choice sc = "<<neighbors[node_new][tpop+1];
					adjacency[i][node_old]=0;adjacency[node_old][i]=0; //severing
					adjacency[i][node_new]=1;adjacency[node_new][i]=1; //wiring
					cout<<endl<<i<<"  sever="<<node_old<<" wire="<<node_new;
					for(k=1;k<tpop+1;k++){
						for(j=1;j<tpop+1;j++){
							neighbors[k][j]=0;
						}
					}
					
					for (k=1;k<tpop+1;k++)
					{
						neighbor_count=0; //initialize the number of neighbors of individual i
						for (j=1;j<tpop+1;j++)
						{
							if (adjacency[k][j]==1)
							{
								neighbor_count++;
								neighbors[k][neighbor_count]=j; //put the neighbor name as a matrix value;
							}//if end
						}//for end
					neighbors[k][tpop+1]=neighbor_count; //put the total number of neighbors at the end of the matrix
					};//for end
				//end identifying middle managers	
				}
			}
		}
	}

}
void evolveNET2( short int **adjacency, double pc, int tpop, int sc, short int** neighbors, short int**superior, double **score)
{
	int k=0, Dist_near=0,Dist_far=0, q=0, span_focal=0, \
		span_far=0, span_near=0, node_focal=0,node_far=0,node_near=0;
	int change;
	int node_old=0, node_new=0, i,j;
	int neighbor_count=0;
	int searchcount=0;
	int nn=0, is_superior=0;
	double prob=0, rn=0;
	

	
	for(i=1;i<tpop+1;i++){
		//change=determineSup(i, superior,neighbors,score,tpop);
		cout<<endl<<"num superior of"<<i<<" = "<<superior[i][tpop+1];
		
		nn=	neighbors[i][tpop+1];
		for(j=1;j<nn+1;j++){
			is_superior=0;
			for(k=1;k<superior[i][tpop+1]+1;k++){
				if(j==superior[i][k]) is_superior=1;
			}
				
			if(uniform()<pc && is_superior==0){
				node_old=neighbors[i][j];
				searchcount=0;
				do{
					searchcount++;
					node_new=1+ (int)(tpop*(uniform()));
					if (searchcount>10) break;
				} while(adjacency[i][node_new] !=0 || neighbors[node_new][tpop+1]>sc ||node_new==0);	
			
		
				if (searchcount<11){
					cout<<endl<<"now severing and wiring";
					cout<<endl<<"new node in choice sc = "<<neighbors[node_new][tpop+1];
					adjacency[i][node_old]=0;adjacency[node_old][i]=0; //severing
					adjacency[i][node_new]=1;adjacency[node_new][i]=1; //wiring
					cout<<endl<<i<<"  sever="<<node_old<<" wire="<<node_new;
					for(k=1;k<tpop+1;k++){
						for(j=1;j<tpop+1;j++){
							neighbors[k][j]=0;
						}
					}
					
					for (k=1;k<tpop+1;k++)
					{
						neighbor_count=0; //initialize the number of neighbors of individual i
						for (j=1;j<tpop+1;j++)
						{
							if (adjacency[k][j]==1)
							{
								neighbor_count++;
								neighbors[k][neighbor_count]=j; //put the neighbor name as a matrix value;
							}//if end
						}//for end
					neighbors[k][tpop+1]=neighbor_count; //put the total number of neighbors at the end of the matrix
					};//for end
				//end identifying middle managers	
				}		
			}
		
		
		}
		
		
	}
}	

//EvalEvaluate Performance
void evalPerformance(short int ** reality, short int**individuals, short int **correctAns, double **score, int Step, double alpha, int m, int n)
{
	int linearScore, nonlinearScore, i, j;
	int product;
	int multiplier;

	
	
	//Individual's Performance
	multiplier=Step; //Issue001: There is no identifiable reason why the multiplier changes to zero
					 //even if it is defined at the beginning of this program.
	//cout<<endl<<"Multiplier= "<<multiplier<<endl;
	for(i=1; i<n+1; i++)
	{
		//Reinitialize the value
		linearScore = 0;
		nonlinearScore = 0;

		//Calculate a Linear Score
		for(j=1; j<m+1; j++)
		{
			if(individuals[i][j] == reality[j][1])
			{
				linearScore++;
				correctAns[i][j] = 1; //Update the correct answer vector
				//cout<<endl<<"Correct Answer("<<i<<","<<j<<") "<<correctAns[i][j];
			}
			else
			{
				correctAns[i][j]=0; //make sure that correct answer to set zero if it is wrong
			}
		}
		//cout<<endl<<"individual "<<i<<endl;
		//for(j=0;j<m;j++){
		//	cout<<correctAns[i][j];
		//}
		//score[i] = linearScore; //Assign individual i's linear perf.
		
		//Calculate a Nonlinear Score
		//cout<<endl<<"nonlinear score:  "<<i<<endl;
		for(j=1; j<m+1; j=j+Step)
		{
			product = 1; //Reinitialize

			//Scalable form of product term for the nonlinear function
			for (int x=1; x<Step+1; x++)
			{
				product = product*correctAns[i][j+x-1]; //x-1 increases from 0 to Step-1
			}
			//cout<<product;
			nonlinearScore = nonlinearScore + Step*product;
		}
	//	cout<<endl<<i<<"   Linear Score= "<<linearScore<<"  Non-Linear Score=  "<<nonlinearScore;
		score[i][1] = alpha*linearScore + (1 - alpha)*nonlinearScore; //Performance
	}
}


//Each Individual Learns from superior indivduals
int individualLearningfromBoss(short int** superior, short int ** majorityView, short int **individuals, short int **individuals_lag, short int **neighbors, double **score, double p,double align, int m, int n, int simTime, float ***groupinformationflow, float ***groupinformationflow2,short int **group, short int **LofComMtx)
{
	int change;
	int terminate=0;
	int j=0;
	int superiorNum;
	int superiorNeighbor;
	int fromGroup;
	int toGroup;
	int teacherNum=0;
	int BOSS=0;
	int learningFromBoss=0;
	
	
	for(int i=1; i<n+1; i++)
	{
		change=0; //Reinitialize this value
		
		//Find Superior Individuals
		change = determineSup(i, superior,neighbors,score,n); //modify !!
		terminate=terminate + change; // terminate is the number of sup individual
		toGroup = group[i][1];
		//determine BOSS
		for (int j=1;j<n+1;j++)
		{
			if((LofComMtx[j][i]==1) && (j>0)) BOSS=j;
		}
		
		//cout<<endl<<"individual="<<i<<" found his BOSS= "<<BOSS;
		//Change the individuals' beliefs
		//if the vertical alignment is set, i.e. align >0 then learn regardless
		//of the existence of superior.
		if (align>0){
			change=1;
		} 
		
		if (change > 0) //Changes only when superiors exist
		{
			for (int d=1; d<m+1; d++)
			{
				learningFromBoss=0; //this flag set 1 when i learns from his boss
				majorityView[i][d] = superiorBelief(i, d, superior,individuals_lag,n);
				
				//To some extend (indicated by align), 
				//take the BOSS's knowledge instead of majority view.
				if ((uniform()<align) && BOSS>0)
				{
					if (individuals_lag[BOSS][d]!=0) majorityView[i][d]=individuals_lag[BOSS][d];
 				  //cout<<endl<<"individual="<<i<<"  learn from the BOSS="<<BOSS<<" about m="<<d;
 				  learningFromBoss=1;
				}
				//Changes only when the majority view is not
				//zero and when there is a luck.
				
				
				if ((uniform() < p)&&(majorityView[i][d]!=0))
				{
					//debug
				//	if (individuals[i][d] != majorityView[i][d]){
				//		cout<<endl<<"individual="<<i<<"  learn and change "<<individuals[i][d]<<" to "<<majorityView[i][d]<<" in m="<<d;	
				//	}
					//degub end
					individuals[i][d] = majorityView[i][d];

					//update information flow quantity
					//the number of superiors when an individual i learns from them
					superiorNum=superior[i][n+1];  
					teacherNum=0;
					for (int k=1; k<superiorNum+1;k++)
					{
						superiorNeighbor=superior[i][k];
						if (individuals_lag[superiorNeighbor][d]==majorityView[i][d])
						{
							teacherNum=teacherNum+1;
						}
					}
					
					//if they learn from their boss, then set the teacher number 1;
					if(learningFromBoss==1) teacherNum=1;
					
					for (int k=1; k<superiorNum+1;k++)
					{
						superiorNeighbor=superior[i][k]; //pick one superior from the superior neighbors
						
						if (individuals_lag[superiorNeighbor][d]==majorityView[i][d]) //if the superior picked has the majority view in that dimension, I assume that our individual learned from him
						{
							fromGroup=group[superiorNeighbor][1]; //specify which group the superior neighbor belongs to
							groupinformationflow[fromGroup][toGroup][simTime]=groupinformationflow[fromGroup][toGroup][simTime]+ float (1/teacherNum); //add weighted score
							groupinformationflow2[fromGroup][toGroup][simTime]=groupinformationflow2[fromGroup][toGroup][simTime] + 1; //add weighted score
						}
					}
					//information flow update is finished
					
				}
				
			}
			//debug
			
			
		}//Close if-condition
	//debug
	/*
		if (change>0){
				cout<<endl<<i<<endl;
				for (int d=1;d<m+1;d++){
					cout<<individuals_lag[i][d]<<",";
				}
				cout<<endl;
				for (int k=1;k<superiorNum+1;k++){
					superiorNeighbor=superior[i][k];
					cout<<endl<<superiorNeighbor<<" score="<<score[superiorNeighbor][1]<<endl;
					for (int d=1;d<m+1;d++){
						cout<<individuals[superiorNeighbor][d]<<",";
					}
					cout<<endl;
				}
		
		}
	*/
	}//Close the first for-loop
	
	return terminate;
}
int individualLearningfromBoss_eq(short int** superior, short int ** majorityView, short int **individuals, short int **individuals_lag, short int **neighbors, double **score, double p,double align, int m, int n, int simTime, float ***groupinformationflow, float ***groupinformationflow2,short int **group, short int **LofComMtx)
{
	int change;
	int terminate=0;
	int j=0;
	int superiorNum;
	int superiorNeighbor;
	int fromGroup;
	int toGroup;
	int teacherNum=0;
	int BOSS=0;
	int learningFromBoss=0;
	int nosuperior=0; //flag turn 1 when there is no superior in nb
	
	for(int i=1; i<n+1; i++)
	{
		change=0; //Reinitialize this value
		nosuperior=0;
		//Find Superior Individuals
		change = determineSup(i, superior,neighbors,score,n); //modify !!
		terminate=terminate + change; // terminate is the number of sup individual
		toGroup = group[i][1];
		//determine BOSS
		for (int j=1;j<n+1;j++)
		{
			if((LofComMtx[j][i]==1) && (j>0)) BOSS=j;
		}
		
		//cout<<endl<<"individual="<<i<<" found his BOSS= "<<BOSS;
		//Change the individuals' beliefs
		//if the vertical alignment is set, i.e. align >0 then learn regardless
		//of the existence of superior.
		if (align>0 && change==0){
			nosuperior=1;
			change=1;
		} 
		
		if (change > 0) //Changes only when superiors exist
		{
			for (int d=1; d<m+1; d++)
			{
				learningFromBoss=0; //this flag set 1 when i learns from his boss
				majorityView[i][d] = superiorBelief(i, d, superior,individuals_lag,n);
				
				//To some extend (indicated by align), 
				//take the BOSS's knowledge instead of majority view.
				//only when there is no superior (performance) to learn
				if ((uniform()<align) && BOSS>0 && nosuperior>0)
				{
					if (individuals_lag[BOSS][d]!=0) majorityView[i][d]=individuals_lag[BOSS][d];
 				  //cout<<endl<<"individual="<<i<<"  learn from the BOSS="<<BOSS<<" about m="<<d;
 				  learningFromBoss=1;
				}
				//Changes only when the majority view is not zero.
				//with prob = p

				if ((uniform() < p)&&(majorityView[i][d]!=0))
				{
					
					individuals[i][d] = majorityView[i][d];

					//update information flow quantity
					//the number of superiors when an individual i learns from them
					superiorNum=superior[i][n+1];  
					teacherNum=0;
					for (int k=1; k<superiorNum+1;k++)
					{
						superiorNeighbor=superior[i][k];
						if (individuals_lag[superiorNeighbor][d]==majorityView[i][d])
						{
							teacherNum=teacherNum+1;
						}
					}
					
					//if they learn from their boss, then set the teacher number 1;
					if(learningFromBoss==1) teacherNum=1;
					
					for (int k=1; k<superiorNum+1;k++)
					{
						superiorNeighbor=superior[i][k]; //pick one superior from the superior neighbors
						
						if (individuals_lag[superiorNeighbor][d]==majorityView[i][d]) //if the superior picked has the majority view in that dimension, I assume that our individual learned from him
						{
							fromGroup=group[superiorNeighbor][1]; //specify which group the superior neighbor belongs to
							groupinformationflow[fromGroup][toGroup][simTime]=groupinformationflow[fromGroup][toGroup][simTime]+ float (1/teacherNum); //add weighted score
							groupinformationflow2[fromGroup][toGroup][simTime]=groupinformationflow2[fromGroup][toGroup][simTime] + 1; //add weighted score
						}
					}
					//information flow update is finished
					
				}
				
			}
			
		}//Close if-condition
	
	}//Close the first for-loop
	
	return terminate;
}
//Determine Superiority


//Find Superior Belief



//Calculate Summary Statistic for Dissimilarity

void calculatelocalDissimilarity_by_group2(short int **reality,short int **individuals,short int ** individuals_lag, short int **group,double **GroupDissimilarity,double **GroupCorrectbit,double **GroupCorrectbit_fh,double **GroupCorrectbit_sh,double **GroupCorrectbit_gain,double **GroupCorrectbit_loss,int seg_pr, int count[],int groupNum,int m, int tpop,int simTime, short int **neighbors, short int **superior, double **score,int timevaryingCount[])
{
	int		i, j,k,friend1,neighborNum,target_groupNumber=0,change,g,sumCorrectBit=0,sumCorrectBit_fh=0,sumCorrectBit_sh=0,sumCorrectBit_gain=0, sumCorrectBit_loss=0;
	double	dissimilarity;
	int		sum_one=0,sum_zero=0,sum_minus=0,sum = 0;
	short int ** correctBit;
	short int ** correctBit_fh;  //first half of cbt
	short int ** correctBit_sh;  //second half of cbt
	short int ** correctBit_gain;
	short int ** correctBit_loss;
	
	correctBit		  =	matrix(1,m,1,groupNum);
	correctBit_fh		  =	matrix(1,m,1,groupNum);
	correctBit_sh		  =	matrix(1,m,1,groupNum);
	correctBit_gain	=	matrix(1,m,1,groupNum);
	correctBit_loss	=	matrix(1,m,1,groupNum);
	initialize_matrix (correctBit,1,m,1,groupNum,0);
	initialize_matrix (correctBit_fh,1,m,1,groupNum,0);
	initialize_matrix (correctBit_sh,1,m,1,groupNum,0);
	initialize_matrix (correctBit_gain,1,m,1,groupNum,0);
	initialize_matrix (correctBit_loss,1,m,1,groupNum,0);
	
	for (i=1;i<tpop+1;i++)
	{
		
		neighborNum=neighbors[i][tpop+1]; //the number of neighbors of each agent
		change = determineSup(i, superior,neighbors,score,tpop); //modify !!
		//neighborNum=superior[i][tpop+1];
		sum=0; //set zero agent i dissimilarity calculation
		target_groupNumber=0;
		for (j=1;j<m+1;j++)
		{
			sum_one=0;sum_zero=0;sum_minus=0;
			//individual i's knowledge difference should be added first
			
			if (individuals[i][j]== 1) sum_one   = sum_one+1;
			if (individuals[i][j]== 0) sum_zero  = sum_zero+1;
			if (individuals[i][j]==-1) sum_minus = sum_minus+1;
			
			for (k=1;k<neighborNum+1;k++)
			{
				//friend1=superior[i][k]; //kth superior neighbor of agent i
				friend1=neighbors[i][k]; //kth neighbor of agent i
				//agent i's neighbors are defined as friend1.
				if (individuals[friend1][j]== 1) sum_one   = sum_one+1;
				if (individuals[friend1][j]== 0) sum_zero  = sum_zero+1;
				if (individuals[friend1][j]==-1) sum_minus = sum_minus+1;
			}
			sum=sum + (sum_one * sum_zero) + (sum_one*sum_minus) + (sum_zero*sum_minus);
			
			//Calculate Correct bits
			if(individuals[i][j]==reality[j][1])
			  {
			    correctBit[j][group[i][1]]=1;
			    if(j<seg_pr+1){
			      correctBit_fh[j][group[i][1]]=1;
			    }
			    else if (j>seg_pr){
			      correctBit_sh[j][group[i][1]]=1;
			    }
				  correctBit_loss[j][group[i][1]]=0;
			    if(individuals_lag[i][j]!=reality[j][1] )
			      {correctBit_gain[j][group[i][1]]=1;}
			    if(individuals_lag[i][j]==reality[j][1] )
			      {correctBit_gain[j][group[i][1]]=0;}
			  };
			if(individuals[i][j]!=reality[j][1])
			  {
				if(individuals_lag[i][j]==reality[j][1] )
				{correctBit_loss[j][group[i][1]]=1;}
			  };
		}
		target_groupNumber = neighborNum+1; // number of neighbors plus agent i
		
		if(target_groupNumber-1>0) //to prevent dividing by Zero
		{
			dissimilarity= (double) (2*sum)/(target_groupNumber*(target_groupNumber-1));
			dissimilarity=dissimilarity/m;
		}
		else
		{
			dissimilarity=0;
		}
				
		GroupDissimilarity[simTime][group[i][1]]=GroupDissimilarity[simTime][group[i][1]]+dissimilarity;
		
		
  }
  //Calculate total number of correct bits by group
  for (g=1;g<groupNum+1;g++)
  {
		sumCorrectBit=0;
		sumCorrectBit_fh=0;
		sumCorrectBit_sh=0;
		sumCorrectBit_gain=0;
		sumCorrectBit_loss=0;
		for (j=1;j<m+1;j++)
		{
			sumCorrectBit=sumCorrectBit+correctBit[j][g];
			sumCorrectBit_fh=sumCorrectBit_fh+correctBit_fh[j][g];
			sumCorrectBit_sh=sumCorrectBit_sh+correctBit_sh[j][g];
			sumCorrectBit_gain=sumCorrectBit_gain+correctBit_gain[j][g];
			sumCorrectBit_loss=sumCorrectBit_loss+correctBit_loss[j][g];
		}
		
		GroupCorrectbit[simTime][g]=GroupCorrectbit[simTime][g]+sumCorrectBit;
		GroupCorrectbit_fh[simTime][g]=GroupCorrectbit_fh[simTime][g]+sumCorrectBit_fh;
		GroupCorrectbit_sh[simTime][g]=GroupCorrectbit_sh[simTime][g]+sumCorrectBit_sh;
		GroupCorrectbit_gain[simTime][g]=GroupCorrectbit_gain[simTime][g]+sumCorrectBit_gain;
		GroupCorrectbit_loss[simTime][g]=GroupCorrectbit_loss[simTime][g]+sumCorrectBit_loss;
		//cout<<endl<<"Group="<<g<<"  Group Correct Bit="<<sumCorrectBit;
		//cout<<endl<<"Group="<<g<<"  Group Correct Bit gain="<<sumCorrectBit_gain;
		//cout<<endl<<"Group="<<g<<"  Group Correct Bit loss="<<sumCorrectBit_loss;
		//cout<<endl<<"Group="<<g<<"  Cum Group Correct Bit="<<GroupCorrectbit[simTime][g];
		
  }
  free_matrix(correctBit,1,m,1,groupNum);
  free_matrix(correctBit_fh,1,m,1,groupNum);
  free_matrix(correctBit_sh,1,m,1,groupNum);
  free_matrix(correctBit_gain,1,m,1,groupNum);
  free_matrix(correctBit_loss,1,m,1,groupNum);
	
}




void reportNetwork(char* resultnetworkfile, int tpop, short int **adjacency)
{
	//Export adjacency matrix
	//Assumption: adjacency matrix is symetric
	//Therefore, export right upper triangle
		
	int i=0,j=0;
	
	ofstream outFile1;
	outFile1.open(resultnetworkfile, ios::app); //Opening the data file
	outFile1.setf(ios::showpoint);
	if(!outFile1){
		cerr << "File could not be opened" <<endl;
		exit(1); //prototype in stadlib.h
	}
  
	outFile1<<"Source,"<<"Target";
	for (i=1;i<tpop+1;i++){ 
		for (j=i+1;j<tpop+1;j++){
			if(adjacency[i][j]==1) outFile1<<endl<<i<<","<<j;
		}
	};
	outFile1<<endl;
}

//Write the "Debug.txt" File into the disk

void reportResults (int scenario_no, double p, double align, int intv_policy, char* resultfile,int Step, double beta,\
double biasedP, double **GroupAveragePerformance, double **AveragePerformance, double **FreqofEq, \
int count[], int trialNum, int jobRotation, int groupNum, int simTime,int tpop, \
double ** GroupDissimilarity, double ** GroupCorrectbit, double ** GroupCorrectbit_fh, double ** GroupCorrectbit_sh,double ** GroupCorrectbit_gain,\
double ** GroupCorrectbit_loss,int timevaryingCount[], double sumofavglength, double rateLayoff, double rate_envChange, double rate_turnover)
{
	int g=0,j;
	
	ofstream outFile1;
	outFile1.open(resultfile, ios::app); //Opening the data file
	outFile1.setf(ios::showpoint);
	if(!outFile1){
		cerr << "File could not be opened" <<endl;
		exit(1); //prototype in stadlib.h
	}
	

	if (jobRotation==1) outFile1<<"scenario_no,"<<"LearningP,"<<"align,"<<"intvp,"<<"S,"<<"Beta,"<<"BiasedLP,"<<"rateLayoff,"<<"echange,"<<"turnover,"<<"time,"<<"group,"<<"AvgPerformance,"<<"change,"<<"Delta,"<<"dissimilarity,"<<"GroupCbit,"<<"GCbit_gain,"<<"GCbit_loss,"<<"GCbit_fh,"<<"GCbit_sh";
	for (g=1;g<groupNum+1;g++){
		for (j=2;j<simTime+1;j++){
			//outFile1<<endl<<p<<","<<Step<<","<<beta<<","<<biasedP<<","<<j<<","<<g<<","<<GroupAveragePerformance[j][g]/count[g]/trialNum<<","<<(GroupAveragePerformance[j][g]-GroupAveragePerformance[j-1][g])/count[g]/trialNum<<","<<GroupDissimilarity[j][g]/count[g]/trialNum;
			outFile1<<endl<<scenario_no<<","<<p<<","<<align<<","<<intv_policy<<","<<Step<<","<<beta<<","<<biasedP<<","<<rateLayoff<<","<<rate_envChange<<","<<rate_turnover<<","<<j<<","<<g<<","<<GroupAveragePerformance[j][g]/trialNum<<","<<FreqofEq[j][1]/trialNum<<","<<(GroupAveragePerformance[j][g]-GroupAveragePerformance[j-1][g])/trialNum<<","<<GroupDissimilarity[j][g]/timevaryingCount[g]/trialNum<<","<<GroupCorrectbit[j][g]/trialNum<<","<<GroupCorrectbit_gain[j][g]/trialNum<<","<<GroupCorrectbit_loss[j][g]/trialNum<<","<<GroupCorrectbit_fh[j][g]/trialNum<<","<<GroupCorrectbit_sh[j][g]/trialNum;
		}
	};
	
	for (j=2;j<simTime+1;j++){
			outFile1<<endl<<scenario_no<<","<<p<<","<<align<<","<<intv_policy<<","<<Step<<","<<beta<<","<<biasedP<<","<<rateLayoff<<","<<rate_envChange<<","<<rate_turnover<<","<<j<<","<<100<<","<<AveragePerformance[j][1]/trialNum<<","<<FreqofEq[j][1]/trialNum<<",,,"<<sumofavglength/trialNum;
	}
	outFile1<<endl;
	outFile1.close();
}
void indi_reportResults (int scenario_no, int loop, double p, double align, int intv_policy, char* resultfile,int Step, double beta,\
double biasedP, double **GroupAveragePerformance, double **AveragePerformance, double **FreqofEq, \
int count[], int trialNum, int jobRotation, int groupNum, int simTime,int tpop, \
double ** GroupDissimilarity, double ** GroupCorrectbit, double ** GroupCorrectbit_fh, double ** GroupCorrectbit_sh,double ** GroupCorrectbit_gain,\
double ** GroupCorrectbit_loss,int timevaryingCount[], double sumofavglength, double rateLayoff, double rate_envChange, double rate_turnover)
{
	int g=0,j;
	
	ofstream outFile1;
	outFile1.open(resultfile, ios::app); //Opening the data file
	outFile1.setf(ios::showpoint);
	if(!outFile1){
		cerr << "File could not be opened" <<endl;
		exit(1); //prototype in stadlib.h
	}
	

	if (jobRotation==1) outFile1<<"scenario_no,"<<"trial_no,"<<"LearningP,"<<"align,"<<"intvp,"<<"S,"<<"Beta,"<<"BiasedLP,"<<"rateLayoff,"<<"echange,"<<"turnover,"<<"time,"<<"group,"<<"AvgPerformance,"<<"change,"<<"Delta,"<<"dissimilarity,"<<"GroupCbit,"<<"GCbit_gain,"<<"GCbit_loss,"<<"GCbit_fh,"<<"GCbit_sh";
	for (g=1;g<groupNum+1;g++){
		for (j=2;j<simTime+1;j++){
			//outFile1<<endl<<p<<","<<Step<<","<<beta<<","<<biasedP<<","<<j<<","<<g<<","<<GroupAveragePerformance[j][g]/count[g]/trialNum<<","<<(GroupAveragePerformance[j][g]-GroupAveragePerformance[j-1][g])/count[g]/trialNum<<","<<GroupDissimilarity[j][g]/count[g]/trialNum;
			outFile1<<endl<<scenario_no<<","<<loop<<","<<p<<","<<align<<","<<intv_policy<<","<<Step<<","<<beta<<","<<biasedP<<","<<rateLayoff<<","<<rate_envChange<<","<<rate_turnover<<","<<j<<","<<g<<","<<GroupAveragePerformance[j][g]/trialNum<<","<<FreqofEq[j][1]/trialNum<<","<<(GroupAveragePerformance[j][g]-GroupAveragePerformance[j-1][g])/trialNum<<","<<GroupDissimilarity[j][g]/timevaryingCount[g]/trialNum<<","<<GroupCorrectbit[j][g]/trialNum<<","<<GroupCorrectbit_gain[j][g]/trialNum<<","<<GroupCorrectbit_loss[j][g]/trialNum<<","<<GroupCorrectbit_fh[j][g]/trialNum<<","<<GroupCorrectbit_sh[j][g]/trialNum;
		}
	};
	
	for (j=2;j<simTime+1;j++){
			outFile1<<endl<<scenario_no<<","<<loop<<","<<p<<","<<align<<","<<intv_policy<<","<<Step<<","<<beta<<","<<biasedP<<","<<rateLayoff<<","<<rate_envChange<<","<<rate_turnover<<","<<j<<","<<100<<","<<AveragePerformance[j][1]/trialNum<<","<<FreqofEq[j][1]/trialNum<<",,,"<<sumofavglength/trialNum;
	}
	outFile1<<endl;
	outFile1.close();
}
void reportResults_informationflow (int scenario_no,double p, char* resultfile,int Step, double beta, double biasedP, float ***groupinformationflow,float ***groupinformationflow2, int count[], int trialNum, int jobRotation, int groupNum, int simTime,int tpop)
{
	int g,j,i;
	int cum_groupinformationflow,cum_groupinformationflow2,k;
	
	ofstream outFile1;
	outFile1.open(resultfile, ios::app); //Opening the data file
	outFile1.setf(ios::showpoint);
	if(!outFile1){
		cerr << "File could not be opened" <<endl;
		exit(1); //prototype in stadlib.h
	}
	

	if (jobRotation==1) outFile1<<"scenario_no,"<<"LearningP,"<<"S,"<<"Beta,"<<"BiasedLP,"<<"time,"<<"fromGroup,"<<"toGroup,"<<"WeightedAvgFlowQty,"<<"CumWeightedAvgFlowQty,"<<"AvgFlowQty,"<<"CumAvgFlowQty";
	
	for (j=1;j<simTime+1;j++){
		for (g=1;g<groupNum+1;g++){
			for (i=1;i<groupNum+1;i++){
				cum_groupinformationflow=0;
				cum_groupinformationflow2=0;
				for (k=1;k<j+1;k++)
				{
					cum_groupinformationflow=cum_groupinformationflow+groupinformationflow[g][i][k];
					cum_groupinformationflow2=cum_groupinformationflow2+groupinformationflow2[g][i][k];
				}
				
				outFile1<<endl<<scenario_no<<","<<p<<","<<Step<<","<<beta<<","<<biasedP<<","<<j<<","<<g<<","<<i<<","<<groupinformationflow[g][i][j]/trialNum<<","<<cum_groupinformationflow/trialNum<<","<<groupinformationflow2[g][i][j]/trialNum<<","<<cum_groupinformationflow2/trialNum;
			}
		}
	};
	
	outFile1.close();
}

////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
///////////    Functions for Various Graphs   //////////////
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////

//Create an Array for degree for node i
void countDegree(short int **l, short int **DEGREE, int n)
{

	int i,j;
    int sum;


	for (i=1; i<n+1; i++)
	{
		sum=0;
		for (j=1; j<n+1; j++)
		{
			sum = sum+l[i][j];
		}
		DEGREE[i][1]=sum;
	}
}




void show_count(int count[],short int **group,int groupNum, int n)
{
	int i;
	cout<<endl<<"This is the group matrix"<<endl;
	for(i=0;i<n;i++)cout<<"  "<<i+1<<"  ";
	cout<<endl;
	for(i=1;i<n+1;i++) cout<<"  "<<group[i][1]<<"  ";
	cout<<endl;
	
	cout<<endl<<"This is the group distribution matrix"<<endl;
	for(i=1;i<groupNum+1;i++) cout<<"  "<<i<<"  ";
	cout<<endl;
	for(i=1;i<groupNum+1;i++) cout<<"  "<<count[i]<<"  ";
	cout<<endl;
}

void show_adj(int **m, int n)
{
	int i,j;
	cout<<endl<<"This is the adj matrix";
	cout<<endl<<"  ";
	for(i=0;i<n+1;i++) cout<<"  "<<i<<"  ";
	cout<<endl;
	for (i=0;i<n+1;i++){
		cout<<i;
		for(j=0;j<n+1;j++) cout<<"  "<<m[i][j];
		cout<<endl;
	};
}

//counting the size of each group
int *dist_count(short int **group,int groupNum, int n)
{
	int *count;
	int i;
	count=(int*)malloc(sizeof(int)*(groupNum+1));
	for (i=1;i<groupNum+1;i++) count[i]=0;
	for(i=1;i<n+1;i++){
		count[group[i][1]]=count[group[i][1]]+1;
	}
	return count;
	free(count);
}



// set default 'value' to the matrix

void set_probfield (short int **m,short int **n,long nrl,long nrh, long ncl, long nch)
{
	//cout<<endl<<"set_probfiled"<<endl;
	
	int i,j,k,Dik,Djk;
	
	for (i=nrl;i<=nrh;i++)
		for (j=ncl;j<=nch;j++)
			for (k=nrl;k<=nrh;k++){
				Dik=m[i][k]; Djk=m[j][k];
				if(Dik<INF && Djk<INF) {
					if(n[i][j] > Dik*Dik + Djk*Djk){
						n[i][j]=Dik*Dik + Djk*Djk;
					}
				}
			}

}

void copyformalnet (short int** impMtx,short int** pathMtx,short int** LofComMtx, short int**minpath,short int tpop)
{
	int i,j;
	
	for(i=1;i<tpop+1;i++)
	{
		for(j=1;j<tpop+1;j++)
		{
			if(impMtx[i][j]>=1){
				pathMtx[i][j]=1;
			};
			if(LofComMtx[i][j]>=1) {
				minpath[i][j]=1;minpath[j][i]=1;
			};
		}
	}
}
void copyimptonetmtx (short int** impMtx,short int** NetMtx,short int tpop)
{
	int i,j;
	
	for (i=1;i<tpop+1;i++)
	{
		for(j=1;j<tpop+1;j++)
		{
			NetMtx[i][j]=impMtx[i][j];
			if (i==j) NetMtx[i][j]=0;
		}
	}
}

void initialize_permtx(double **AveragePerformance,double **FreqofEq,double **GroupAveragePerformance,int simTime, int groupNum)
{
	int i,j;
	for (i=1;i<simTime+1;i++){
		AveragePerformance[i][1]=0;
		FreqofEq[i][1]=0;
		for (j=1;j<groupNum+1;j++){
			GroupAveragePerformance[i][j]=0;
		};//for end
	};//for end
	//end Initialize average performance matrix
}

int jobrotation(short int **pathMtx, short int **NetMtx, int tpop, double beta)
{
	int newlink=0;
	int i,j;
	int node_A, node_B;
	double temp;
	for (i=1;i<tpop+1;i++){
		node_A = i;
		//randomly pick a node (called B) as a target position
		//temp=(double)ran1(&idum_net);
		  temp=uniform();
		if (temp<beta){
			do {
				
				node_B = 1+ (int)(tpop*(uniform()));
			} while ((node_A == node_B) || abs(pathMtx[1][node_A]-pathMtx[1][node_B])>1);
		// cout<<node_A<<"  --> "<<node_B<<endl;
		// copy the neighbor of node_B
		// this one assumes a symetric metrix
			for (j=1;j<tpop+1;j++){
				if (NetMtx[node_B][j]==1){
					if ((NetMtx[node_A][j]!=1) && (node_A !=j)){
						NetMtx[node_A][j]=2;NetMtx[j][node_A]=2;
					}//if end
					newlink++;
				}//if end
			};//for end
		};//if end
	};//for end
	return newlink;
}

void copyadjtonet(short int** adjacency,short int**NetMtx,int tpop) 
{
	int i,j;
	for (i=1;i<tpop+1;i++)
		for(j=1;j<tpop+1;j++)
			adjacency[i][j]=NetMtx[i][j];
}

void initialize_adj(short int** adjacency,int tpop)
{
	int i,j;
	for (i=1;i<tpop+1;i++){
		for (j=0;j<tpop+1;j++){
			if (adjacency[i][j]==2){
				adjacency[i][j]=1;adjacency[j][i]=1;
			}//if end
		}//for end
	};//for end
	// end initializing adjacency matrix
}

void biasedlearning(short int ** group, int biasedG,double biasedP, short int** adjacency, short int** LofComMtx, int tpop)
{
	int i,j;
	for (i=1;i<tpop+1;i++){
		for (j=i;j<tpop+1;j++){
			if (group[i][1]==biasedG){
				if (uniform()<biasedP && adjacency[i][j]==1){
					if (LofComMtx[i][j]!=1){
						adjacency[i][j]=2;adjacency[j][i]=2;
						//cout<<endl<<i<<"  "<<j<<" is changed to  "<<adjacency[i][j];
					}//if end
				}//if end
			}//if end
		}//for end
	};//for end
	//end biased learning
}

void alterteamnetwork (int teamgroupNum,short int** teamgroup,short int** adjacency,short int** LofComMtx,int tpop)
{
		int subteamindex;
	int subteam_size;
	int i,j,g;
	int subagentindex;

	//A.divide team group
	for (g=1;g<teamgroupNum+1;g++)
	{
//		cout<<endl<<"teamgroup="<<g;
		subteamindex=0;
		subagentindex=0;
		subteam_size=4+int (6*uniform()); //first draw team size
	
		//Note
		// 1.teamgroup[i][1]==g represents the team group code which is given(imported from the data set)
		// 2.teamgroup[i][2] represents the index of the agent i within a newly defined subgroup
		// 3.teamgroup[i][3] represents the ID of subgroup within the given team group g
		
 
		for (i=1;i<tpop+1;i++)
		{
			if(teamgroup[i][1]==g) //teamgroup[i][1] represents the code of teamgroup agent i belongs to
			{
				subagentindex=subagentindex+1;    //subteamindex increases upto the subteam size
				if (subagentindex<subteam_size+1) //within the subteam, the index increases
				{
					teamgroup[i][2]=subagentindex;
				}
				else							  //if the index is bigger than team size, draw new subteam size and start new subagentindexing
				{
					subagentindex=1;
					subteam_size=4+int (6*uniform());
					teamgroup[i][2]=subagentindex;
				}
				if (subagentindex==1) subteamindex=subteamindex+1;
				teamgroup[i][3]=subteamindex;     //subteamindex identifies the subteam within the teamgroup, g. subteamindenx would be increases as the subagendindext set 1 (start new subteam)
//				cout<<endl<<"agent="<<i<<" teamgroup="<<teamgroup[i][1]<<" subteamsize="<<subteam_size<<" agentindex="<<subagentindex<<" teamindex="<<subteamindex;
			}
		}
	}
	//end dividing team groups


	//B.adjust adjacency network
	
	for (i=1;i<tpop+1;i++){
		for(j=1;j<tpop+1;j++){
			if ((teamgroup[i][1] ==teamgroup[j][1])&& (teamgroup[i][3] !=teamgroup[j][3])) //if agent i and j belongs to the same teamgroup g but located in separate subgroups (teamgroup[i][3])
			{
				adjacency[i][j]=2; //the connection is deleted (adjacency==2 would not considered in the learning process. Effectively disconnected
//				cout<<endl<<i<<","<<teamgroup[i][1]<<","<<teamgroup[i][3]<<","<<j<<","<<teamgroup[j][1]<<","<<teamgroup[j][3]<<" altered";
			}
			if ((LofComMtx[i][j]==1)&&(teamgroup[i][1]>0)) //if i is a subordinate who belongs to a teamgroup
			{
				if (teamgroup[i][2] !=1) //team lead i: iff teamgroup[i][2]==1
				{
					adjacency[i][j]=2; adjacency[j][i]=2;//if i is not a team lead, the line of command would be disabled. Effectively disconnected during the course of learning process.
//					cout<<endl<<i<<","<<j<<" line of command altered";
				}
			}
		}
	}
		

}

void identifymiddlemanagers(int tpop,short int** group,short int**mgtgroup,short int**middlemanagers,double **score, short int**neighbors, short int**adjacency)
{
	int NumMiddlemanagers=0;

	int Total_NumMiddlemanagers=0;
	int i,j;
	//A.identify middle managers
	//(2009.09.20) The middle managers of the simple test data set could be identified by the group identification where group==3
	
	for (i=1;i<tpop+1;i++)
	{
		if (group[i][1]==3)
		{
				mgtgroup[i][1]=1;mgtgroup[i][2]=0;mgtgroup[i][3]=0; // initialize the flag of middle manager
				//NumMiddlemanagers=NumMiddlemanagers+1;
				//cout<<endl<<i<<"in group="<<group[i][1]<<"  is a middle manager.";
		};
	};
	
	//(2009.09.20)
	//(2009.10.18) end
	for (i=1;i<tpop+1;i++){
		if (mgtgroup[i][1]==1){
			NumMiddlemanagers=NumMiddlemanagers+1;
			middlemanagers[NumMiddlemanagers][1]=i;  //the id of middle manager
			middlemanagers[NumMiddlemanagers][2]=group[i][1]; //the level of the middle manager
			middlemanagers[NumMiddlemanagers][3]=score[i][1]; //the performance of the middle manager
			middlemanagers[NumMiddlemanagers][4]=0; //middlemanagers[i][4]=1 represents that i is involved in reorganization activity
		}
	};
	Total_NumMiddlemanagers=NumMiddlemanagers;
	middlemanagers[tpop+1][1]=Total_NumMiddlemanagers; //at the end of this list, total number of middle managers are recorded.
	//cout<<endl<<"Total Number of middle managers= "<<Total_NumMiddlemanagers;
	//A.End identifying middle managers
	



}

void setneibormatrix(int tpop,short int** neighbors,short int** adjacency)
{
	
	int i,j,neighbor_count;
	//B.Initialize Neighbor Matrix
	for (i=1;i<tpop+1;i++){
		for(j=1;j<tpop+1;j++){
			neighbors[i][j]=0;
		}//for end
	}//for end
	//B.End initializing neighbor matrix

	//C.Set Neighbor Matrix
	//learning will consider the neighbors matrix only.
	//so, if the adjacency is not '1', then, it is not an valid network for learning
	for (i=1;i<tpop+1;i++)
	{
		neighbor_count=0; //initialize the number of neighbors of individual i
		for (j=1;j<tpop+1;j++)
		{
			if (adjacency[i][j]==1)
			{
				neighbor_count++;
				neighbors[i][neighbor_count]=j; //put the neighbor name as a matrix value;
			}//if end
		}//for end
		neighbors[i][tpop+1]=neighbor_count; //put the total number of neighbors at the end of the matrix
	};//for end
	//end identifying middle managers	
}

void initiate_envChange(int m,short int ** reality, double rate_envChange){
	int i,j;
	//Reality
				for (j=1; j<m+1; j++)
				{
					if(uniform()<rate_envChange){
						cout<<endl<<"change of reality";
						reality[j][1] = rand()%2;
						//reality[j] = 1+(int)(2.0 * uniform()/(2.0));
				
						if(reality[j]==0)
						{reality[j][1] = -1;}
					}
				}
	
	
}

void turnover_individuals (int tpop,double rate_turnover,int m,short int** individuals){
	int i,j;
	for (i=1; i<tpop+1; i++)
				{
					if(uniform()<rate_turnover){
						//cout<<endl<<"new individual = "<<i;
						for (j=1; j<m+1; j++)
						{
							
							individuals[i][j] = rand()%3 - 1;
							
							//This code will reduce the level of correct bits of each individuals
					
						}
					}
				}

	
}
	