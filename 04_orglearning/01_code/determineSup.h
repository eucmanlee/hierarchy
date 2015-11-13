int determineSup(int i,short int **superior, short int **neighbors,double **score, int n)
{
	int change = 0;
	int neighbor_count=0;
	int neighbor=0;

	neighbor_count=neighbors[i][n+1];

	for (int j=1; j<neighbor_count+1; j++)
	{
		neighbor=neighbors[i][j];

		if(score[neighbor][1] > score[i][1]) //score[neighbor] = superior's perf
			{
				change = change + 1;
				superior[i][change]=neighbor;
				//debug
				//cout<<endl<<"member="<<i<<" has "<<change<<"th superior="<<neighbor<<" by "<<score[neighbor][1];
			}
	}
	superior[i][n+1]=change; //the number of superior neighbors
	return change;
}
