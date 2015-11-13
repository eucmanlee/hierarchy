int superiorBelief(int i, int d, short int ** superior, short int ** individuals_lag, int n)
{
	int majority = 0;
	int positive = 0;
	int negative = 0;
	int num_superior_neighbors=superior[i][n+1]; //the total number of superior neighbors are store at this location
	int superior_neighbor=0;

	for (int j=1; j<num_superior_neighbors+1; j++)
	{
			superior_neighbor=superior[i][j];
			if (individuals_lag[superior_neighbor][d]==1)
			{ positive = positive + 1;}

			if(individuals_lag[superior_neighbor][d]==-1)
			{ negative = negative + 1;}

			//Eliminate the consideration of zeros
			//becasue the value indicates no knowledge
		
	}

	if (positive > negative)
	{ majority = 1;}

	if (negative > positive)
	{ majority = -1;}

	//If none of these conditions are met, this function
	//will return 0, which will not affect an individual
	//e.g.) if (positive == negative) majority = 0;
	
	//2015.01.02
	//additional condition say, if both are the same, then flip the coin
	//if((positive == negative) && positive>0)
	//{
	//	if (uniform() > 0.5) majority =1;
	//	else majority = -1;
	//}
	
	return majority;
}
