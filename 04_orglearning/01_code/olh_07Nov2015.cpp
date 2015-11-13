
//Date: 2014.12.09
//Author: Eucman Lee
//Description:
//Correct bit analysis has been deployed in order to demonstrate the flow of knowledge
//over the network.
//Top down learning over the hierarchy:
//It appears that organization cannot fully utilize existing correct bits
//and often suffer from low performance. In order to overcome knowledge stickyness,
//managers may enforce a certain type of knowledge lower level of the hierarchy.
//In this scenario, the degree of vertical alignment indicates the tendency to
//follow managers instructions and recombine them with existing knowledge.
//Given that managers have no external source of knowledge infusion, it is expected
//that too much alignment may reduce overall performance.

//Date: 2014.12.23
//Author: Eucman
//Description:
//Unique correct bits are used to test the flow of knowledge across groups

//Date: 2014.12.24
//Author: Eucman
//Description:
//Different learning rate for managers
//managers due to their authority or given belief about their superiority,
//may stuburn and less likely to learn from their subordinates.
//or they may spend less time in learning but more time in strategy formation

//Date: 2015.011.07
//Author: Eucman
//It has been long time since I have revised this program. 


#include  <stdio.h>
#include  <stdlib.h>
#include  <iostream>
#include  <iomanip>
#include  <cstdlib>
#include  <stddef.h>
#include  <math.h>
#include  <fstream>
#include  <sys/types.h>
#include  <string.h>
#include  <cmath>
#include  <time.h>
#include  <unistd.h>
#include  <ctype.h>
#include  "hierarchy.h"  //This is an important 'head' which contains custom functions.

//Followings are ones from the numerical receipy of C, Not sure whether those are still relevant.
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

//following code for iostram namspace issues
using namespace std;

ofstream outFile1; //Creating a data file
ofstream outFile2; //Creating a data file
ofstream outFile3; //Creating a data file


//***** MAIN *****//
//****************//
int main(int argc, char *argv[])
{


//File Names
char *resultfile,*resultfile2, *resultnetworkfile, *indi_resultfile;
char *lineofcommand, *teamnetwork;
//Model Parameters: default values unless there is additional inputs.
int m=99;   				  //Dimension of problem: each individual is evaluated comparing with this given problem set
int tpop = 4420;			//Total number of nodes: This parameter represents total number of individuals within the organization.
int MaxT=2000;				//Max Time period: Currently, the learning procedure is conducted within a fixed time period due to performance reason. Stoping rules are not implemented at this moment
int Step = 9;				//Parameter for the nonlinear step function: This parameter determines the degree of complexity of the problem set (m)
int multiplier = Step;		//Multiplier for the nonlinear performance
double p;					//Learning probability: the parameter is supposed to be imported in the input specificiation script outside of the program
double align;
double beta;				//Rewiring probability: the parameter is supposed to be imported in the input specificiation script outside of the program
double network_beta;		//Rewiring probability, the network characteristic. just for output report: the parameter is supposed to be imported in the input specificiation script outside of the program
double alpha = 0;			//Weight for the linear combination

//Variables
int a,b,i,j,k,neighbor_count;
int groupNum,teamgroupNum,t=0,loop,trialNum,biasedG=999;
int terminate=0;            //flips to 1 when there is no superior individual in the population
int jobRotation=0;          //Flog=1 when there is job rotation
int *count,*teamgroupcount; //the size of groups
int *timevaryingCount;		//the size of groups at each period, why we need this count?
int simTime;				//Simulation time
int seed;					//Seed for random number generator
int round=1;				//the number of routine for testing
int timeLayoff=0;			//start time of reorganization
int newlink=0;
int netvalue=0;
int node_A,node_B;
int total_prob=0;
int opt;
double sumofavglength=0;
double sumofdyads=0;
double sumoflength=0;

//Reorganization Related Parameters
float draw=0;
int NumoflayedoffManagers=0;
int manager1=0;
int manager2=0;
int groupofmanager1=0;
int n=0;
//end reorganization variables

//Specify the names and functions of following variables
long  idum_net;
long double temp;
double startTime, endTime;
double time1=0,time2=0,time3=0,time4=0,time5=0;
double biasedP=0;
double CutLOC=0;
double rateLayoff=0;
double pc=0; //Prob of connection
double rate_envChange=0; //environmental change
double rate_turnover=0;  //employee turnover
int numDraw=0; //num of draws while generating team network
int radius=0;  //neighbor of focal node
int sc=20; //span of control
int seg_cb=0; //segregating correct bits
int seg_pr=0;
int seg_gp=2; //In knowledge endowment setting, the first group to hold the first half.
int mgt_learn=0;
int mgt_p=0;
int envChange=0;
int empTurn=0;
int intv_policy=1;
int scenario_no=1; //this indicates the test scenario index
int indi_output=0; //output result of each trial if indi_output==1
int export_network=0; //expert the current network if set 1
//Data Variables: list of network matrix
short int	**NetMtx;
short int	**impMtx,**pathMtx,**LofComMtx;
short int	**minpath; //minimum path matrix
short int	**group;
short int	**superior;
short int	**Degree;
short int	**reality;
short int	**majorityView;
short int	**individuals;
short int	**individuals_lag;
short int	**correctAns;
short int	**neighbors;
short int	**middlemanagers;
short int	**teamgroup;// the team network of the subordinates.
short int	**mgtgroup;// the middle manager group.
double		**GroupAveragePerformance;
double		**indi_GroupAveragePerformance;
double		**GroupDissimilarity;
double		**GroupCorrectbit;
double		**GroupCorrectbit_fh;
double		**GroupCorrectbit_sh;
double		**GroupCorrectbit_gain;
double		**GroupCorrectbit_loss;
double		**AveragePerformance;
double		**indi_AveragePerformance;
double		**FreqofEq;
double		**score;
double		**score_lag;
short int	**adjacency;
float		***groupinformationflow;
float		***groupinformationflow2;


cout<<endl<<"Initialize Array";


//File IO Variables
FILE *ifp_lineofcommand;
FILE *ifp_teamnetwork;
FILE *ifp_group;
FILE *ifp_teamgroup;
ofstream outFile1;

//Just set static matrix at this moment. It should be changed using dynamic matrix
//gives a warning if there are less than 1 argument.
//The argument is the line of command data

////////////////////////////////////////////////////////////////////////////////////////////

    while ((opt = getopt(argc, argv, "I:f:t:s:p:T:b:o:i:g:n:r:l:G:W:N:M:S:P:X:L:R:V:C:D:d:k:a:y:z:c:v:e:h:F:")) != -1) {
        switch (opt) {
		case 'I':
			scenario_no = atoi(optarg);
			break;
		case 'f':
			lineofcommand = optarg;
			break;
		case 't':
			teamnetwork = optarg;
			break;
		case 's':
			seed=atoi(optarg);											// 3.seed number
			break;
		case 'p':
			p=atof(optarg);											      // 4.Learning probability
			break;
		case 'T':
			simTime=atoi(optarg);										        // 5.simulation period
			break;
		case 'b':
			network_beta=atof(optarg);									      // 6.Network Char (Beta)
			break;
		case 'o':
			resultfile=optarg;											          // 7.output file name
			break;
		case 'i':
			resultfile2=optarg;										          // 8.Individual Performance
			break;
		case 'g':
			ifp_group = fopen(optarg,"rt");							    // 9.Group information
			break;
		case 'n':
			groupNum =atoi(optarg);									        //10.Number of groups: This groups are levels within the organization
			break;
		case 'r':
			trialNum =atoi(optarg);									        //11.the Number of trials
			break;
		case 'l':
			biasedP  =atof(optarg);									        //12.the biased Learning probability	<??? what is the biased learning?
			break;
		case 'G':
			biasedG	 =atoi(optarg);									        //13.the biased Learning group - informal networks of the biased group would be deleted by the biased learning probability.
			break;
		case 'W':
			ifp_teamgroup = fopen(optarg,"rt");						  //14.team group information
			break;
		case 'N':
			teamgroupNum=atoi(optarg);								      //15.the number of team groups
			break;
		case 'M':
			m=atoi(optarg);                                 //16.Size of knowledge tuple
			break;
		case 'S':
			Step=atoi(optarg);                              //17.Non linear performance parameter
			break;
		case 'P':
			tpop=atoi(optarg);                              //18.Total number of nodes
			break;
		case 'X':
			CutLOC=atof(optarg);										        //19.The ratio of cutting line of command
			break;
		case 'L':
			timeLayoff=atoi(optarg);									      //20.The starting period of layoff
			break;
		case 'R':
			rateLayoff=atof(optarg);									      //21.The rate of lay off in the group of middle managers
			break;
		case 'V':
			align=atof(optarg); //22. Deg of vertical alignment
			break;
		case 'C':
			pc=atof(optarg); //23.probability of connection
			break;
		case 'D':
			numDraw=atoi(optarg); //24.number of draw
			break;
		case 'd':
			radius=atoi(optarg);
			break;
		case 'k':
			export_network=1;
			resultnetworkfile=optarg; //25.network output files
			break;
		case 'a':
			sc=atoi(optarg); //26. span of control
			break;
		case 'y':
			seg_cb=1; //27. flag for segregating correct bits
			seg_pr=atoi(optarg); //this value may indicate the size of first half.
			break;
		case 'z':
			mgt_learn=1; //28. learning prob of mgt differs
			mgt_p=atoi(optarg);
			break;
		case 'c':
			seg_gp=atoi(optarg); //29. the knowledge endowment group number.
			break;
		case 'v':
			intv_policy=atoi(optarg); //30. top down intervention policy 1=intervine randomly 2=only when the subs are in equilibrium.
			break;
		case 'e':
			envChange = 1;
			rate_envChange=atof(optarg); //31. The reality vector will be altered with prob of rate_envChange.
			break;
		case 'h':
			empTurn = 1;
			rate_turnover=atof(optarg); //31. The reality vector will be altered with prob of rate_envChange.
			break;
		case 'F':
			indi_output=1; //32. When the output is needed for each individual trials.
			indi_resultfile=optarg;
			break;
    default: /* '?' */
            fprintf(stderr, "Usage: %s [-t nsecs] [-n] name\n",
                    argv[0]);
            exit(EXIT_FAILURE);
        }
    }


	//Display the Experiment parameters
	
	cout<<endl<<"Simulation Parameters";
	cout<<endl<<"formal network= "<<lineofcommand;
	cout<<endl<<"team network= "<<teamnetwork;
	cout<<endl<<"seed= "<<seed;
	cout<<endl<<"Learning p= "<<p;
	cout<<endl<<"periods= "<<simTime;
	cout<<endl<<"beta= "<<network_beta;
	cout<<endl<<"output file1= "<<resultfile;
	cout<<endl<<"output file2= "<<resultfile2;
	cout<<endl<<"Number of groups(Levels)= "<<groupNum;
	cout<<endl<<"Number of team groups= "<<teamgroupNum;
	cout<<endl<<"Trials= "<<trialNum;
	cout<<endl<<"Biased Learning P= "<<biasedP;
	cout<<endl<<"Biased Group= "<<biasedG;
	cout<<endl<<"Population= "<<tpop;
	cout<<endl<<"Line of command cutting ratio= "<<CutLOC;
	cout<<endl<<"Time of layoff= "<<timeLayoff;
	cout<<endl<<"Lay off rate  = "<<rateLayoff;
	cout<<endl<<"Deg of Alignment = "<<align;
	cout<<endl<<"HRG parameter = "<<pc;
	cout<<endl<<"num of rand draw = "<<numDraw;
	cout<<endl<<"neighbor radius = "<<radius;
	cout<<endl<<"Max span of control = "<<sc;
	cout<<endl<<"segregate correct bits by group = "<<seg_cb;
	cout<<endl<<"segregate correct bits by group p = "<<seg_pr;
	cout<<endl<<"Separate learning prob for mgt (yes=1) = "<<mgt_learn;
	cout<<endl<<"Separate learning prob for mgt = "<<mgt_p;
	cout<<endl<<"Top down intervention (= 1 randome intervention, 2=int when eq) = " <<intv_policy;
	
	ifp_lineofcommand=fopen(lineofcommand,"rt");
	if(!ifp_lineofcommand){ printf("\n Please input formal network data\n");	return 0;};
	ifp_teamnetwork=fopen(teamnetwork,"rt");
	if(!ifp_teamnetwork  ){ printf("\n Please input team network data\n");		return 0;};
	
	cout<<endl<<"Parameter Input Finished";

	//Initialize matrix for organization
	NetMtx		=	matrix(1,tpop,1,tpop);		// Network matrix
	impMtx		=	matrix(1,tpop,1,tpop);		// Imported data
	LofComMtx	=	matrix(1,tpop,1,tpop);		// Line of command network which should not be modified by network rewiring or biased learning
	pathMtx		=	matrix(1,tpop,1,tpop);		//
	minpath		=	matrix(1,tpop,1,tpop);		// the minimum path between two indiivduals.
	group		=	matrix(1,tpop,0,1);			// The group tag of each individual
	teamgroup	=	matrix(1,tpop,0,3);			// the team group of the subordinates
	mgtgroup	=	matrix(1,tpop,0,3);			// This is a group of middle managers for reorganization scenario test (2009.09.04)
	middlemanagers = matrix (1,tpop+1,0,4);		// The list of middle managers with their attributes
	adjacency	=	matrix(1,tpop+1,1,tpop+1);  // Adjacency network
	superior	=	matrix(1,tpop+1,1,tpop+1);  // Superior member of each individual
	Degree		=	matrix(1,tpop,0,1);			// Degree information. currently not in use. reserved for future scenario
	reality		=	matrix(1,m,0,1);			// Land scape of the reality, drawn randomly.
	majorityView=	matrix(1,tpop,1,m);			// Store majority view of each knowledge dimension.
	individuals =	matrix(1,tpop,1,m);			// Individual knowledge at current period
	individuals_lag =	matrix(1,tpop,1,m);		// Individual knowledge at previous period
	neighbors	=	matrix(1,tpop+1,1,tpop+1);
	correctAns	=	matrix(1,tpop,1,m);
	GroupAveragePerformance =	dmatrix(1,simTime,1,groupNum);  //The average performance by group
	indi_GroupAveragePerformance =	dmatrix(1,simTime,1,groupNum);  //The average performance by group
	GroupDissimilarity		=	dmatrix(1,simTime,1,groupNum);  //The average dissimilarity by grdoup
	GroupCorrectbit			=	dmatrix(1,simTime,1,groupNum);  //The number of correct bits by group
	GroupCorrectbit_fh			=	dmatrix(1,simTime,1,groupNum);  //The number of correct bits by group
	GroupCorrectbit_sh			=	dmatrix(1,simTime,1,groupNum);  //The number of correct bits by group
	GroupCorrectbit_gain			=	dmatrix(1,simTime,1,groupNum);  //The number of correct bits gain by group
	GroupCorrectbit_loss			=	dmatrix(1,simTime,1,groupNum);  //The number of correct bits loss fuby group
	AveragePerformance		=	dmatrix(1,simTime,0,1);			//The average performance of each individual
	indi_AveragePerformance		=	dmatrix(1,simTime,0,1);			//The average performance of each individual
	FreqofEq		=	dmatrix(1,simTime,0,1);			//The average equilibrium frequency of each individual
	score		=	dmatrix(1,tpop,0,1);						//Individual Score, evaluated by comparing individual knowledge with reality
	score_lag	=	dmatrix(1,tpop,0,1);						//Individual Score, at previous period


	groupinformationflow = f3tensor(1,groupNum,1,groupNum,1,simTime); //Weighted information flow quantity for each periods, averaged over trials
	groupinformationflow2 = f3tensor(1,groupNum,1,groupNum,1,simTime);
	cout<<endl<<"Matrices are Created";

	//end

	//Initialize (set zero for each matrix elements)
	initialize_matrix (NetMtx,1,tpop,1,tpop,0);
	initialize_matrix (impMtx,1,tpop,1,tpop,0);
	initialize_matrix (pathMtx,1,tpop,1,tpop,INF);
	initialize_matrix (minpath,1,tpop,1,tpop,INF);
	initialize_matrix (group,1,tpop,0,1,0);
	initialize_matrix (teamgroup,1,tpop,0,3,0);
	initialize_matrix (mgtgroup,1,tpop,0,3,0);
	initialize_matrix (LofComMtx,1,tpop,1,tpop,0);
	initialize_dmatrix (GroupAveragePerformance,1,simTime,1,groupNum,0);
	initialize_dmatrix (indi_GroupAveragePerformance,1,simTime,1,groupNum,0);
	initialize_dmatrix (GroupDissimilarity,1,simTime,1,groupNum,0);
	initialize_dmatrix (GroupCorrectbit,1,simTime,1,groupNum,0);
	initialize_dmatrix (GroupCorrectbit_gain,1,simTime,1,groupNum,0);
	initialize_dmatrix (GroupCorrectbit_loss,1,simTime,1,groupNum,0);
	initialize_dmatrix (AveragePerformance,1,simTime,0,1,0);
	initialize_dmatrix (indi_AveragePerformance,1,simTime,0,1,0);
	initialize_dmatrix (FreqofEq,1,simTime,0,1,0);
	initialize_f3tensor (groupinformationflow,1,groupNum,1,groupNum,1,simTime,0);
	initialize_f3tensor (groupinformationflow2,1,groupNum,1,groupNum,1,simTime,0);
	cout<<endl<<"Matrices are Initialized";
	//information flow initialization
	
	
	//Setting initial value 0 for each matrix below
	initialize(superior,reality,majorityView,individuals,correctAns,neighbors,m,tpop);
	
	///////////////////////////////////////////////////////////////////////
	//Import data:
	//import line of command networks to the network matrix with value 1
	//import team networks to the network matrix with value 1
	//The matrix is symmetric as it is 'undirected' network
	///////////////////////////////////////////////////////////////////////
	
	while(1){
			if(fscanf(ifp_lineofcommand,"%d,%d",&a,&b)==EOF)break;
		   //Note that the member code starts from 1
		   //and network is undirected therefore, the matrix is symmetric
		   impMtx[a][b]=1; impMtx[b][a]=1;
		   LofComMtx[a][b]=1;LofComMtx[b][a]=2;
	}
	fclose(ifp_lineofcommand);
	
	while(1){
		   if(fscanf(ifp_teamnetwork,"%d,%d",&a,&b)==EOF)break;
		   impMtx[a][b]=1; impMtx[b][a]=1;
		   //ParentMtx[b][a]=1;
	   }
	fclose(ifp_teamnetwork);
	//end
  
	//Import the group information
	//Group is determined by the level of hierarchy within the organization
	if(!ifp_group){ printf("Please import Level information.\n");	return 0;};
	while(1){
		   if(fscanf(ifp_group,"%d,%d",&a,&b)==EOF)break;
		   group[a][1]=b;
	}
	fclose(ifp_group);
	//end import group information

	//import the team group information
	//team group represents the team information in employee level
	//(not a managerial level)
	//Thus, it would be related with the agents located at level 6 or level 7
	
	if(!ifp_teamgroup){ printf("Please import team information.\n");	return 0;};
	while(1){
		   if(fscanf(ifp_teamgroup,"%d,%d",&a,&b)==EOF)break;
		   teamgroup[a][1]=b;
	}
	fclose(ifp_teamgroup);
  
  
	///////////////////////////////////////////////////////////////////////
	//* calculate the network parameters
	//  average path length and clustering coefficient */
	///////////////////////////////////////////////////////////////////////
	
	//  copy the current formal network to the pathmtx and minpath matrix
	copyformalnet (impMtx,pathMtx,LofComMtx, minpath,tpop);

	//Note that calculating network parameters take very long time.
 	//Using minpath, identify distance between two individuals over the formal networks
	//shortestpath_matrix (pathMtx,1,tpop,1,tpop);
	shortestpath_matrix (minpath,1,tpop,1,tpop);
	//show_matrix (pathMtx,1,tpop,1,tpop);
		
	//set the seed for random number generation
	//Important!!!. The seed number should be put before the 'for' loop at this moment
	//if not, the random # generater will create redundant random numbers
	idum_net=(long)seed_net*(-1)-(3);
	//end

	//Calculate the size of each groups
	count=dist_count(group, groupNum,tpop);
	
	teamgroupcount=dist_count(teamgroup, teamgroupNum,tpop);
	timevaryingCount=dist_count(group, groupNum,tpop); //(2009.09.10) the reorganizational model requires time varying group numbers
	//show_count(teamgroupcount,teamgroup,teamgroupNum,tpop);



	
	//******************************************************//
	// mail loop starts here								//
	//******************************************************//

	jobRotation=0;

	//start the job rotation routine
	for(beta=0.0;beta<=0;beta=beta+10){	// current setting of job rotation is disabled by setting beta<=0
		jobRotation++;
		//Initialize average performance and frequency of eq matrix
		initialize_permtx(AveragePerformance,FreqofEq,GroupAveragePerformance,simTime, groupNum);
		//reinitializae matrix
		initialize_matrix (NetMtx,1,tpop,1,tpop,0);
		//copy the imported data to the NetMtx
		//only NetMtx will be used for learning simulation.
		copyimptonetmtx(impMtx,NetMtx,tpop);
	
	

	
		//Job Rotation begins from here
		if (beta>0){
			newlink =jobrotation(pathMtx, NetMtx, tpop, beta);
		}
	
		/***** Put here copy from NetMtx to adjacency***/
		//copy modified (After Job Rotation) network to adjacency Matrix
		//In every trial, only adjacency matrix would be changed in order to keep the original network structure
		//Thus, the adjacency will be initiated in every trials
		copyadjtonet(adjacency,NetMtx,tpop);
	


	
		///////////////////////////////////////////////////////////////////////////////
		// Forming hierarchical network
		// 1.use the distance over the line of command network(=D)
		// 2.with a certain level of probability >0, two randomly picked agents will be connected
		//   i.e. prob(i,j) = probability of agents i and j are connected
		// 3.prob(i,j) is inversely correlated with the distance between agents within a
		//   line of command. i.e prob(i,j)=1/[1+pc*D(i,j)] where pc is drawn from [0 1];
		// 4.Total number of draw of a random pairi(drwPair) should be calculated (see Newman)
		//
		///////////////////////////////////////////////////////////////////////////////
		
		if (numDraw>0) {
				cout<<endl<<"start random generation";
				formHRG(minpath, adjacency, pc, numDraw,tpop, sc, radius);
				}
		if (export_network==1) reportNetwork (resultnetworkfile, tpop, adjacency);

	for (loop=1;loop<trialNum+1;loop++){	//Main GA loop begins
	
		//set new seed number for each trial
		seed=seed+1;srand(seed);

		//Initialize Performance Matrix
		initialize2(superior, reality,majorityView,individuals,correctAns,score, score_lag, m,tpop);
		initialize_adj(adjacency,tpop);
	
		//Segregating correct bits into two different groups
		//group=1 will have the left half and group=2 will have the other
		//Individuals do not belong to either will have zero initial knowledge
		if (seg_cb==1){
		  segcb(individuals, group,m,tpop,seg_pr,seg_gp);
		}
			
		//put biased learning factor with probability BiasedP;
		//the line of command will not be altered
		if (biasedP > 0)
		{
			biasedlearning(group, biasedG,biasedP, adjacency, LofComMtx,tpop);
		}
	
		//Initialize the size of groups and mgtgroup for reorganizational scenario. The size should be recalculated in every turn
		timevaryingCount=dist_count(group, groupNum,tpop); //(2009.09.10) the reorganizational model requires time varying group numbers
		initialize_matrix (mgtgroup,1,tpop,0,3,0);
	
		//Put team network alteration here
		//1.pick one team group
		//2.draw team size (from 4 to 10)
		//3.divide each group into smaller teams
		//4.set adjacency 2 for inter small team network
		//(2009.09.20) This team size adjustment routine is temporarily disabled in order to use simpler data set scenrio
		if (teamgroupNum>0){
			alterteamnetwork (teamgroupNum,teamgroup,adjacency,LofComMtx,tpop);
		}
		
		//(2009.09.04)
		//Reorganization scenario
		//1.Middle managers are identified based on the information of line of command as well as teagroup[][]
		//  information which identifies subordinates who belongs to one of the teamgroup[][] within the organization.
		//  The basic idea is that the middle managers are the managers who is supervising one of the teamgroup memebers
		//  within the organization.
		//2.individual i is a middle manager iff mgtgroup[i][1]==1
		//3.The middle managers might have (could have) several attributes such as performance, number of subordinates etc.
		//  Those attributes are set in mgtgroup[i][2] or mgtgroup[i][3]
		//(2009.09.20)
		//middle managers are identified as a group==3, as the simple test data set has 4 levels and level3 is the middle managers.
		identifymiddlemanagers(tpop,group,mgtgroup,middlemanagers,score, neighbors,adjacency);
		setneibormatrix(tpop,neighbors,adjacency);
		//some old routines that should be re-evaluated
		countDegree(adjacency, Degree,tpop); //evaluate again.
		initializeCorrectAns(correctAns,m,tpop);
		evalPerformance(reality,individuals,correctAns,score,Step,alpha,m,tpop);
	

	
	
	
	
    //Start Learning Dynamics
	//After finishing the initial network set up, start learning (the time clock start to increase after this)
	for (int t=1; t<simTime+1; t++){
	
	
		updateLag(superior,individuals,individuals_lag,score,score_lag,m,tpop); //step1

		if (intv_policy==2) {
			terminate=individualLearningfromBoss_eq(superior,majorityView,individuals,individuals_lag,neighbors,score,p,align,m,tpop,t,groupinformationflow,groupinformationflow2,group,LofComMtx);
		}
		
		if (intv_policy==1) {
			terminate=individualLearningfromBoss(superior,majorityView,individuals,individuals_lag,neighbors,score,p,align,m,tpop,t,groupinformationflow,groupinformationflow2,group,LofComMtx);
		}
	
		//cout<<endl<<"now, start learning."<<endl; //debug
		initializeCorrectAns(correctAns,m,tpop);
		evalPerformance(reality,individuals,correctAns,score,Step,alpha,m,tpop);
		calculatelocalDissimilarity_by_group2(reality,individuals,individuals_lag,group,GroupDissimilarity,GroupCorrectbit,GroupCorrectbit_fh,GroupCorrectbit_sh, GroupCorrectbit_gain,GroupCorrectbit_loss,seg_pr,count,groupNum,m,tpop,t,neighbors,superior,score,timevaryingCount);
	
		//recording the organization and group performance at this given time period.
		//the performance is an average score divided by total valid population as well as valid size of each group
		//the point is that the poplation in concern would change over time due to reorganization.
		for (i=1;i<tpop+1;i++){
			if ((mgtgroup[i][2] >1 )||(mgtgroup[i][2] <1 )) //if the individual i is not a layed-off manager
			{
				AveragePerformance[t][1]=AveragePerformance[t][1]+score[i][1]/(tpop);
				GroupAveragePerformance[t][group[i][1]]=GroupAveragePerformance[t][group[i][1]]+score[i][1]/(timevaryingCount[group[i][1]]);
				indi_AveragePerformance[t][1]=indi_AveragePerformance[t][1]+score[i][1]/(tpop);
				indi_GroupAveragePerformance[t][group[i][1]]=indi_GroupAveragePerformance[t][group[i][1]]+score[i][1]/(timevaryingCount[group[i][1]]);
			}
		}//for end
	FreqofEq[t][1]=FreqofEq[t][1]+terminate/(tpop); //put the number of equilibrium
	//evolveNET2( adjacency, pc, tpop, sc, neighbors, superior, score);
	
	if(envChange==1)initiate_envChange(m,reality, rate_envChange);
	if(empTurn==1)turnover_individuals (tpop,rate_turnover,m,individuals);
	
	//cout<<endl<<"Performance is recorded"<<endl; //debug
	//performance recording finishes
	
	}//for end: end of a single period
	
	if (export_network==1) reportNetwork (resultnetworkfile, tpop, adjacency);
	
	
	if (indi_output==1) //export results of individual trials
	{
		indi_reportResults (scenario_no,loop, p, align,intv_policy,indi_resultfile, Step, beta, biasedP, indi_GroupAveragePerformance, indi_AveragePerformance,FreqofEq, count, 1, jobRotation, groupNum, simTime,tpop,GroupDissimilarity,GroupCorrectbit,GroupCorrectbit_fh,GroupCorrectbit_sh,GroupCorrectbit_gain,GroupCorrectbit_loss, timevaryingCount,sumofavglength,rateLayoff,rate_envChange,rate_turnover);
		initialize_dmatrix (indi_AveragePerformance,1,simTime,0,1,0);
		initialize_dmatrix (indi_GroupAveragePerformance,1,simTime,1,groupNum,0);
	}


}//main time loop ends
	
	
	reportResults (scenario_no,p, align,intv_policy,resultfile, Step, beta, biasedP, GroupAveragePerformance, AveragePerformance,FreqofEq, count, trialNum, jobRotation, groupNum, simTime,tpop,GroupDissimilarity,GroupCorrectbit,GroupCorrectbit_fh,GroupCorrectbit_sh,GroupCorrectbit_gain,GroupCorrectbit_loss, timevaryingCount,sumofavglength,rateLayoff,rate_envChange,rate_turnover);
	reportResults_informationflow (scenario_no,p, resultfile2, Step, beta, biasedP, groupinformationflow, groupinformationflow2, count, trialNum, jobRotation, groupNum, simTime,tpop);
	// Export Average Performance
	/*
	int g=0;
	if (jobRotation==1) cout<<endl<<"LearningP,"<<"S,"<<"Beta,"<<"BiasedLP,"<<"time,"<<"group,"<<"AvgPerformance";
	for (g=0;g<groupNum;g++){
		for (j=1;j<simTime+1;j++){
			cout<<endl<<p<<","<<Step<<","<<beta<<","<<biasedP<<","<<j<<","<<g<<","<<GroupAveragePerformance[j][g]/count[g]/trialNum;
		}
	};
	
	for (j=1;j<simTime+1;j++){
		cout<<endl<<p<<","<<Step<<","<<beta<<","<<biasedP<<","<<j<<","<<100<<","<<AveragePerformance[j][1]/tpop/trialNum;
	}
	*/



}//for end (beta routine)

	free_matrix(NetMtx,1,tpop,1,tpop);
	free_matrix(impMtx,1,tpop,1,tpop);
	free_matrix(pathMtx,1,tpop,1,tpop);
	free_matrix(minpath,1,tpop,1,tpop);
	free_matrix(group,1,tpop+1,0,1);
	free_matrix(teamgroup,1,tpop+1,0,2);
	free_matrix(mgtgroup,1,tpop+1,0,2);
	free_matrix(adjacency,1,tpop+1,1,tpop+1);
	free_matrix(superior,1,tpop+1,1,tpop+1);
	free_matrix(Degree,1,tpop,0,1);
	free_matrix(individuals,1,tpop,1,m);
	free_matrix(individuals_lag,1,tpop,1,m);
	free_matrix(LofComMtx,1,tpop,1,tpop);
	free_matrix(reality,1,m,0,1);
	free_matrix(majorityView,1,tpop,1,m);
	free_matrix(neighbors,1,tpop+1,1,tpop+1);
	free_matrix(correctAns,1,tpop,1,m);
	free_dmatrix(GroupAveragePerformance,1,MaxT,1,groupNum);
	free_dmatrix(GroupDissimilarity,1,MaxT,1,groupNum);
	free_dmatrix(GroupCorrectbit,1,MaxT,1,groupNum);
	free_dmatrix(GroupCorrectbit_fh,1,MaxT,1,groupNum);
	free_dmatrix(GroupCorrectbit_sh,1,MaxT,1,groupNum);
	free_dmatrix(GroupCorrectbit_gain,1,MaxT,1,groupNum);
	free_dmatrix(GroupCorrectbit_loss,1,MaxT,1,groupNum);
	free_dmatrix(AveragePerformance,1,MaxT,0,1);
	free_dmatrix(FreqofEq,1,MaxT,0,1);
	free_dmatrix(score,1,tpop,0,1);
	free_dmatrix(score_lag,1,tpop,0,1);
	//free_f3tensor(informationflow,1,tpop,1,tpop,1,simTime);
	free_f3tensor(groupinformationflow,1,tpop,1,tpop,1,simTime);
	free_f3tensor(groupinformationflow2,1,tpop,1,tpop,1,simTime);
	
 return 0;

}//main ends


