/*
 * fss_biocham.c
 *
 *  Created on: 27/03/2012
 *      Author: anthonylins
 */
#include <stdio.h>
#include <string.h> /* strncmp */
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include "fss_interface.h"
#include "gprolog.h"

double **OrthogonalBasisFss(int DIM);
double f_biocham_multiFss(double const *x, int dimensions);
double f_biochamFss(double const *x, int dimensions);
void set_random_paramsFss(int dim);
int is_feasible_Fitness(double const *x);

void getSearchSpaceRange(int, double **, double **);

int fss_termination(double **, TFssData **, TFssParameters **, int , int , double , double );

int simulFss(double const *x, int dim);

static double fCost = 0;

Bool getFssCost(double *k);

double * optimizeFss(double(*pFun)(double const *, int), int number_of_simulations,
		int dim, int agents, double *maximumSearchSpace, double *minimumSearchSpace,
		int maxevals_b, double stopfitness_b, int cseed, double deltaBarycentre);

extern void fssrandom_init(random_t *, long unsigned seed /*=0=clock*/);
extern void fssrandom_exit(random_t *);
extern double fssrandom_Gauss(random_t *); /* (0,1)-normally distributed */

Bool call_main_fss(int dim, int fun, int maxsimuls, int maxevals_b, PlTerm pl_stopfitness_b, int cseed, double std, int agents, int debugCode) {

	typedef double (*pfun_t)(double const *, int);
	pfun_t rgpFun[99]; /* array (range) of pointer to objective function */

	int nb = 0;

	time_t initialtime;
	time_t finaltime;

	debug = debugCode;

	printf("Fish School Search (FSS) is running...\n");
	fflush(NULL);
	initialtime = time(NULL);

	//printf("call: Rd_Number_Check \n ");
	double stopfitness_b = Rd_Number_Check(pl_stopfitness_b);

	if (debug == TRUE) {
		printf("dimensions: %d \n", dim);
		printf("school size: %d \n", agents);
		printf("barycentre radius: %f \n", std);
		printf("max simulations: %d \n", maxsimuls);
		printf("max evaluations: %d \n", maxevals_b);
		printf("stop fitness value: %f \n", stopfitness_b);
		fflush(NULL);
	}

	double *x; //best
	double deltaBarycentre = std;
	//printf("end: Rd_Number_Check\n ");
	/* Put together objective functions */
	rgpFun[0] = f_biochamFss;

	nb = 0;

	double *minSearch = (double *) calloc(dim, sizeof(double));
	double *maxSearch = (double *) calloc(dim, sizeof(double));

	getSearchSpaceRange(dim,&minSearch,&maxSearch);

	if (debug == TRUE) {
		int d;
		for (d = 0; d < dim; d++) {
			printf("space range: max[%d]: %f :: min[%d]: %f \n ", d, maxSearch[d], d, minSearch[d]);
		}
		fflush(NULL);
	}

	x = optimizeFss(rgpFun[nb], maxsimuls, dim, agents, maxSearch,
			minSearch, maxevals_b, stopfitness_b, -1,deltaBarycentre);

	/* here we could utilize the solution x, and finally free memory */
	printf("end of FSS optimization process. \n ");

	finaltime = time(NULL);

	if (debug == TRUE) {
		long totaltime = finaltime - initialtime;
		printf("total time (seconds): %ld\n", totaltime);
	}
	simulFss(x, dim);

	free(x);

	return TRUE;

} /* main() */

Bool getFssCost(double *k) {
	*k = fCost;
	return TRUE;
}

double* optimizeFss(double(*pFun)(double const *, int), int number_of_simulations,
		int dimensions, int schoolSize, double *maximumSearchSpace,
		double *minimumSearchSpace, int maxevals_b, double stopfitness_b, int cseed,
		double deltaBarycentre) {
	//	double * const *pop; /* sampled population */
	double fitvals[maxevals_b]; /* objective function values of sampled population */
	double fbestever = 1000000;
	double *xbestever = NULL; /* store best solution */
	int irun = 0;
	int countevals = 0; /* used to set for restarts */

	int count_success_ind;
	int count_success_ins;
	int count_success_vol;

	positions = NULL;
	stepInd = NULL;
	stepIndAdaptive = NULL;
	stepIndPrev = NULL;
	weight= NULL;
	fitness= NULL;

	deltaX= NULL;
	oldWeight= NULL;
	deltaFitness= NULL;
	deltaFitnessNormalized= NULL;
	fitnessNeighbor= NULL;
	individual_move_success= NULL;
	volitive_move_success= NULL;
	stagnancyCount = NULL;
	flagStagnancy = NULL;

	time_t iterationInitialTime;
	time_t iterationFinalTime;


	for (irun = 0; irun < number_of_simulations; ) /* restarts */
	{
		int cont = 0;
		int stop = 0;

		TFssData *data = fss_allocationMemoryData(dimensions);

		TFssParameters *parameters = fss_allocationMemoryParameters(dimensions, schoolSize, maximumSearchSpace, minimumSearchSpace, maxevals_b);

		count_success_ind = 0;
		count_success_ins = 0;
		count_success_vol = 0;

		countevals = 0;

		fss_init_data(&data, &parameters, cseed); //, &xbestever); /* allocs fitvals */

		parameters->janecek = TRUE;

		fss_init_positions(&positions, parameters->initialRangeLeft,
				parameters->initialRangeRight, data->rand,
				parameters->dimensions, parameters->schoolSize);

		fss_init_steps(&stepInd, &stepIndPrev, &stepIndAdaptive,
				parameters->minimumSearchSpace,
				parameters->maximumSearchSpace, data->rand,
				parameters->dimensions, parameters->schoolSize);

		fss_init_weights(&weight, &oldWeight, parameters->initialWeightMax,
				parameters->initialWeightMin, data->rand,
				parameters->schoolSize);

		// call objective function - calculate the fitness function for the initial positions.
		fss_fitness((*pFun), &positions, &fitness, &fitnessNeighbor,
				&flagStagnancy, &stagnancyCount, &data, &parameters);

		parameters->countevals += schoolSize;

		while (stop == 0) {
			iterationInitialTime = time(NULL);
			// Movement: Individual Operator
			count_success_ind = fss_individual_operator((*pFun), &positions,
					&deltaX, &fitness, &fitnessNeighbor, &deltaFitness,
					&flagStagnancy, &stagnancyCount, &individual_move_success,
					&stepInd, &stepIndPrev, &stepIndAdaptive, &data,
					&parameters);
			// Feeding Operator
			fss_feeding_operator(&weight, &oldWeight, &deltaFitness,
					&deltaFitnessNormalized, &parameters);

			if (parameters->janecek) {
				/* Janecek's proposals - S1 */
				//weightUpdateStrategiesS1(&weight,&parameters);
				/* Janecek's proposals - S3 */
				stepSizeUpdateStrategiesS3(&data,&parameters,parameters->countevals);
			}


			// Movement: Instinctive Operator
			count_success_ins = fss_instinctive_operator((*pFun), &positions,
					&fitness, &fitnessNeighbor, &deltaX,
					&deltaFitnessNormalized, &individual_move_success,&instinctive_move_success, &data,
					&parameters);
			// Movement: Volitive Operator
			count_success_vol = fss_volitive_operator((*pFun), &positions,
					&deltaX, &fitness, &fitnessNeighbor, &weight, &oldWeight,
					&volitive_move_success, &data, &parameters);

			if (!parameters->janecek) {
				fss_update_steps(&data, &parameters);
			}
			fitvals[cont] = data->gbestFitness[0];
			cont++;

			iterationFinalTime = time(NULL);

			if (debug == TRUE) {
				printf("time of iteration (seconds): %ld \n", (iterationFinalTime - iterationInitialTime));
				fflush(NULL);
			}
			/*
			 * stop conditions
			 */
			stop = fss_termination(&positions, &data, &parameters, cont, maxevals_b, stopfitness_b, deltaBarycentre);

		}

		countevals = parameters->countevals;

		if (debug == TRUE) {
			/* print some "final" output */
			printf(
					"## %d fevals \n %d individual operator success \n %d instinctive operator success \n %d volitive operator success \n :: best f(x)=%g\n",
					countevals, count_success_ind, count_success_ins,
					count_success_vol, data->gbestFitness[0]/*fitvals[irun]*/);
		}

		irun++;

		/*
		 * get the best ever
		 */
		if ((irun >= number_of_simulations) || (stop == 1) ){ ///*fitvals[irun]*/ data->gbestFitness[0] <= fbestever) {
			if (debug == TRUE) {
				printf("read the best of all\n");
				fflush(NULL);
			}
//			if (data->gbestFitness[0] < fbestever) {
				fbestever = data->gbestFitness[0]; //fitvals[irun];
				int gbest = data->gbest[0];
				int dim = parameters->dimensions;
				xbestever = (double*) malloc(dim*sizeof(double));
				int v;
				for (v = 0; v < dim; v++) {
					xbestever[v] = positions[gbest * dimensions + v];
				}
				if (debug == TRUE) {
					printf("best of all was read - OK\n");
					fflush(NULL);
				}
//			}
		}

		// free memory variables and structures
		fss_exit(&positions, &deltaX, &fitness,
				&fitnessNeighbor, &deltaFitness,
				&deltaFitnessNormalized, &weight, &oldWeight,
				&stepInd, &stepIndPrev, &stepIndAdaptive,
				&individual_move_success, &volitive_move_success,
				&stagnancyCount, &flagStagnancy, &data, &parameters); //(&evo); /* does not effect the content of stop string and xbestever */


	} /* for restarts */
	printf("\nFinal cost : %g\n\n", fbestever);
	fCost = fbestever;
	fflush(NULL);

	return xbestever; /* was dynamically allocated, should be freed in the end */
}


/* ----------------------------------------------------------------------- */
double f_biochamFss(double const *x, int dimensions) {
	if (debug == TRUE) {
		printf("f_biochamFss\n"); /* positions:: %p\n", x);*/
		fflush(NULL);
	}
	int i;
	double dist = 0.00001;
	int DIM = dimensions;//(int) (x[-1]);
	PlTerm plist[DIM];
	PlTerm cost;
	PlTerm args[2];
	int result;

	Pl_Query_Begin(TRUE);
	/*make vars */

	cost = Mk_Variable();
	for (i = 0; i < DIM; ++i) {
		if (debug == TRUE) {
			printf("f_biochamFss:: position values: %f \n",x[i]);
			fflush(NULL);
		}
		plist[i] = Mk_Float(x[i]);
	}

	args[0] = Mk_Proper_List(DIM, plist);
	args[1] = cost;

	result = Pl_Query_Call(Find_Atom("distance_fss"), 2, args);

	dist = Rd_Float_Check(args[1]);

	if (debug == TRUE) {
		printf("f_biochamFss:: dist is infinity: %d \n", ((dist == INFINITY || dist == -INFINITY)?1:0) );
		printf("f_biochamFss:: dist: %f \n",dist);
		fflush(NULL);
	}

	Pl_Query_End(PL_RECOVER);

	return dist;
}

int simulFss(double const *x, int DIM) {
	int i;
	PlTerm plist[DIM];
	PlTerm args[1];
	int result;

	if (debug == TRUE) {
		printf("simulFSS:: graph plot \n");
		fflush(NULL);
	}
	Pl_Query_Begin(TRUE);
	/*make vars */

	for (i = 0; i < DIM; ++i) {
		plist[i] = Mk_Float(x[i]);
	}
	args[0] = Mk_Proper_List(DIM, plist);

	result = Pl_Query_Call(Find_Atom("simul_fss"), 1, args);

	Pl_Query_End(PL_RECOVER);

	return 1;
}

/* ----------------------------------------------------------------------- */
/* a hack, memory is never released */
double **OrthogonalBasisFss(int DIM) {
	static int b_dim;
	static double **b;
	double sp;
	int i, j, k;
	random_t R;

	if (b_dim != 0) { /* Initialization was done */

		if (b_dim != DIM) {
			printf(
					"function OrthogonalBasis cannot change dimensionality in file example2.c");

			exit(0);
		}

		return b;
	}

	/* Otherwise initialize basis b */
	fssrandom_init(&R, 2); /* TODO: choose not always the same basis? */

	/* allocate b */
	b = (double **) calloc((unsigned) DIM, sizeof(double*));
	if (!b) {
		printf("calloc failed in function OrthogonalBasis in file example2.c");

		exit(0);
	}
	for (i = 0; i < DIM; ++i) {
		b[i] = (double *) calloc((unsigned) DIM, sizeof(double));
		if (!b[i]) {
			printf(
					"calloc failed in function Orthogonalbasis in file example2.c");

			exit(0);
		}
	}
	b_dim = DIM;

	/* generate orthogonal basis */
	for (i = 0; i < DIM; ++i) {
		/* sample components gaussian */
		for (j = 0; j < DIM; ++j)
			b[i][j] = fssrandom_Gauss(&R);
		/* substract projection of previous vectors */
		for (j = i - 1; j >= 0; --j) {
			for (sp = 0., k = 0; k < DIM; ++k)
				sp += b[i][k] * b[j][k]; /* scalar product */
			for (k = 0; k < DIM; ++k)
				b[i][k] -= sp * b[j][k]; /* substract */
		}
		/* normalize */
		for (sp = 0., k = 0; k < DIM; ++k)
			sp += b[i][k] * b[i][k]; /* squared norm */
		for (k = 0; k < DIM; ++k)
			b[i][k] /= sqrt(sp);
	}
	fssrandom_exit(&R);

	return b;

} /* OrthogonalBasis(int DIM) */

int is_feasible_Fitness(double const *x) {

	int i;
	int res = 1;
	int result;
	double dmin, dmax;
	PlTerm min, max;
	PlTerm args[3];

	int DIM = (int) (x[-1]);
	for (i = 0; i < DIM; i++) {

		Pl_Query_Begin(TRUE);
		min = Mk_Variable();
		max = Mk_Variable();
		args[0] = Mk_Integer(i);
		args[1] = min;
		args[2] = max;
		result = Pl_Query_Call(Find_Atom("bounds"), 3, args);
		dmin = Rd_Float_Check(args[1]);
		dmax = Rd_Float_Check(args[2]);
		//printf("dmin : %f dmax : %f xi : %f\n",dmin,dmax,x[i]);
		if ((x[i] < dmin) || (x[i] > dmax))
			res = 0;
		Pl_Query_End(PL_RECOVER);

	}
	return res;
}

/* ----------------------------------------------------------------------- */
double f_biocham_multiFss(double const *x, int dimensions) {

	int i;
	double dist = 0.;
	int DIM = dimensions;//(int) (x[-1]);
	PlTerm plist[DIM];
	PlTerm cost;
	PlTerm args[2];
	int result;

	Pl_Query_Begin(TRUE);
	/*make vars */

	cost = Mk_Variable();
	for (i = 0; i < DIM; ++i) {

			printf("f_biochamFssMulti:: position values: %f \n",x[i]);
			fflush(NULL);

		plist[i] = Mk_Float(x[i]);
	}
	args[0] = Mk_Proper_List(DIM, plist);
	args[1] = cost;

		printf("f_biochamFssMulti:: chamando a func multi \n");
		fflush(NULL);

	result = Pl_Query_Call(Find_Atom("distance_fss_multi"), 2, args);

		printf("f_biochamFssMulti:: retornando da fun‹o :: result %d\n", result);
		fflush(NULL);

	dist = Rd_Float_Check(args[1]);

	printf("f_biochamFssMulti:: dist: %f \n", dist);
	fflush(NULL);
	Pl_Query_End(PL_RECOVER);

	// printf("cdone\n");
	return dist;
}

void set_random_paramsFss(int dim) {

	PlTerm args[1];
	int result;

	Pl_Query_Begin(TRUE);
	args[0] = Mk_Integer(dim);

	result = Pl_Query_Call(Find_Atom("fss_restart"), 1, args);

	Pl_Query_End(PL_RECOVER);

}

void getSearchSpaceRange(int dimensions, double **minSearchSpace, double **maxSearchSpace)
{

  int i;
  int result;
  double dmin,dmax;
  PlTerm min,max;
  PlTerm args[3];

  for( i=0; i<dimensions; i++){

    Pl_Query_Begin(TRUE);
    min=Mk_Variable();
    max=Mk_Variable();
    args[0]=Mk_Integer(i);
    args[1]=min;
    args[2]=max;
    result=Pl_Query_Call(Find_Atom("fssbounds"),3,args);
    dmin=Rd_Float_Check(args[1]);
    dmax=Rd_Float_Check(args[2]);
    (*minSearchSpace)[i] = (double)dmin;
    (*maxSearchSpace)[i] = (double)dmax;

    if (debug == TRUE) {
		printf("dimension: %d :: dmin : %f dmax : %f\n",i,dmin,dmax);
		fflush(NULL);
    }
    Pl_Query_End(PL_RECOVER);

  }

}

int fss_termination(double **positions, TFssData **data, TFssParameters **parameters, int cont, int maxevals_b, double stopfitness_b, double delta) {
	int retorno = 0;
	int j;

    if (debug == TRUE) {
    	printf("termination :: begin \n");
    	fflush(NULL);
    }

	int dimensions = (*parameters)->dimensions;
	double distance = 0;

	if (debug == TRUE) {
    	printf("termination :: number of evaluations %d >= maxevals_b %d \n", cont, maxevals_b);
    	fflush(NULL);
    }
	if ((*parameters)->countevals >= maxevals_b) {
	    	printf("Stop condition :: number of evaluations:%d >= maxevals:%d \n",(*parameters)->countevals, maxevals_b);
	    	fflush(NULL);
		return retorno = 1;
	}

    if (debug == TRUE) {
    	printf("termination :: (*data)->gbestFitness[0]: %f :: stopfitness_b %f \n", (*data)->gbestFitness[0], stopfitness_b);
    	fflush(NULL);
    }

	if ((*data)->gbestFitness[0] <= stopfitness_b) {
	    	printf("Stop condition :: gbestFitness:%f <= stop fitness:%f \n",(*data)->gbestFitness[0], stopfitness_b);
	    	fflush(NULL);
		return retorno = 1;
	}

    if (debug == TRUE) {
    	printf("termination :: radius\n");
    	fflush(NULL);
    }

	int indexGbest = (*data)->gbest[0];
	for (j = 0; j < dimensions; j++) {
		distance += (pow(
				(*positions)[indexGbest * dimensions + j]
						- (*data)->barycentre[j], 2));
	}
    if (debug == TRUE) {
    	printf("termination :: e \n");
    	fflush(NULL);
    }

	distance = sqrt(distance);
    if (debug == TRUE) {
    	printf("termination :: f \n");
    	fflush(NULL);
    }

	if (distance <= delta) {
	    	printf("Stop condition :: barycentre radius: %f <= delta: %f \n", distance, delta);
	    	fflush(NULL);
		retorno = 1;
	}

    if (debug == TRUE) {
    	printf("termination :: end \n");
    	fflush(NULL);
    }

	return retorno;
}


