/*
 * fss.c
 *
 *  Created on: 23/03/2012
 *      Author: anthonylins
 */

#include <math.h>   /* sqrt() */
#include <stddef.h> /* size_t */
#include <stdlib.h> /* NULL, free */
#include <string.h> /* strlen() */
#include <stdio.h>  /* sprintf(), NULL? */
#include "fss_interface.h" /* time.h and all structures via cmaes.h*/

/* --- initialization, constructors, destructors --- */
long fssrandom_init(random_t *, long unsigned seed /* 0==clock */);
void fssrandom_exit(random_t *);
double fssrandom_Gauss(random_t *); /* (0,1)-normally distributed */
double fssrandom_Uniform(random_t *);
long unsigned fssrandom_Start(random_t *, long unsigned *seed /* 0==1 */);

//static void fss_memory_allocation(TSchool **, int, int);

static TFssData * init_data(int);

static TFssParameters * init_parameters(int, int, double *, double *, int);

//allocation memory
static void allocating_memory_uint(int **);

static void allocating_memory_double1D(double **, int);

static double * new_doubleFss(int n);
static double ** new_double2DFss(int n);
static void * new_voidFss(int n, size_t size);

static int randomXOR(int id);

TFssData * fss_allocationMemoryData(int dimensions) {
	TFssData *data = init_data(dimensions);
	return data;
}

TFssParameters * fss_allocationMemoryParameters(int dimensions, int schoolSize,
		double *maximumSearchSpace, double *minimumSearchSpace,
		int maxIterations) {
	TFssParameters *parameters = NULL;
	parameters = init_parameters(dimensions, schoolSize, maximumSearchSpace,
			minimumSearchSpace, maxIterations);
	//printf("fss_allocationMemoryParameters :: parameters:%p \n", *parameters);

	return parameters;
}

// initialization
// - fss attributes (position, weight, fitness)
void fss_init_data(TFssData **data, TFssParameters **parameters,
		int seed) { //, double *xbestever) {
	int j;

	if (seed < 1) {
		while ((int) (clock() - clock()) == 0)
			;
		/* TODO: remove this for time critical applications!? */
		seed = (int) abs(100 * time(NULL) + clock());
	}

	random_t t;

	long s = fssrandom_init(&t, (unsigned) seed);

	(*data)->rand = &t;

	(*parameters)->seed = s;

	(*data)->version = "0.00.01.alpha";
	for (j = 0; j < (*parameters)->dimensions; j++) {
		(*data)->stepInd[j] = (*parameters)->stepIndividualInitial[j];
		(*data)->stepVol[j] = (*parameters)->stepVolitiveInitial[j];
		if (debug == TRUE) {
			printf("fss_init :: step Ind: %f\n", (*data)->stepInd[j]);
			fflush(NULL);
			printf("fss_init :: step Vol: %f\n", (*data)->stepVol[j]);
			fflush(NULL);
		}
	}

}


static TFssData * init_data(int dimensions) {
	//memory allocation for data structure
	if (debug == TRUE) {
		printf("init_data - TFssData structure\n");
		fflush(NULL);
	}
	TFssData *sdata = (TFssData *) malloc(sizeof(TFssData));

	allocating_memory_double1D(&(sdata->schoolWeight), 1); //total weight of the fish school
	allocating_memory_double1D(&(sdata->gbestFitness), 1); //global best fitness value
	allocating_memory_uint(&(sdata->gbest)); // fish number with the GBest value
	allocating_memory_uint(&(sdata->cycleGbest)); //cycle number which GBest was reached
	allocating_memory_double1D(&(sdata->stepInd), dimensions); // step value for the individual operator.
	allocating_memory_double1D(&(sdata->stepVol), dimensions);
	allocating_memory_double1D(&(sdata->barycentre), dimensions);

	sdata->version = (char *) malloc(20 * sizeof(char));
	if (debug == TRUE) {
		printf("init_data - TFssData structure initialized\n");
		fflush(NULL);
	}

	return sdata;
}

static TFssParameters * init_parameters(int dimensions, int schoolSize,
		double *maximumSearchSpace, double *minimumSearchSpace,
		int maxIterations) {

	/*	clock_t cloc = clock();*/
	int j;
	if (debug == TRUE) {
		printf("init_parameters - TFssParameters\n");
		fflush(NULL);
	}

	TFssParameters *parameters = (TFssParameters *) malloc(
			1 * sizeof(TFssParameters));

	parameters->dimensions = dimensions;
	parameters->schoolSize = schoolSize;

	parameters->maxNumberOfIterations = maxIterations;

	/*adjustment to be compliance with new FSS version features */
	parameters->maximumSearchSpace = (double *) calloc((unsigned) dimensions,
			sizeof(double));
	parameters->minimumSearchSpace = (double *) calloc((unsigned) dimensions,
			sizeof(double));

	parameters->initialRangeLeft = (double *) calloc((unsigned) dimensions,
			sizeof(double));
	parameters->initialRangeRight = (double *) calloc((unsigned) dimensions,
			sizeof(double));
	parameters->stepIndividualInitial = (double *) calloc(
			(unsigned) dimensions, sizeof(double));
	parameters->stepIndividualFinal = (double *) calloc((unsigned) dimensions,
			sizeof(double));
	parameters->stepVolitiveInitial = (double *) calloc((unsigned) dimensions,
			sizeof(double));
	parameters->stepVolitiveFinal = (double *) calloc((unsigned) dimensions,
			sizeof(double));

	for (j = 0; j < dimensions; j++) {
		if (debug == TRUE) {
			printf("init_parameters - max[%d]: %f :: min[%d]: %f\n", j,
				maximumSearchSpace[j], j, minimumSearchSpace[j]);
			fflush(NULL);
		}

		parameters->maximumSearchSpace[j] = maximumSearchSpace[j];
		parameters->minimumSearchSpace[j] = minimumSearchSpace[j];

		/* initialization position range as kohonen's approach (50% search space) */
		//parameters->initialRangeLeft[j] = (parameters->maximumSearchSpace[j]
		//		- parameters->minimumSearchSpace[j]) / 2;
		//parameters->initialRangeRight[j] = (parameters->maximumSearchSpace[j]);

		/* box initialization */
		parameters->initialRangeLeft[j] = (parameters->maximumSearchSpace[j] * 0.25);
		parameters->initialRangeRight[j] = (parameters->maximumSearchSpace[j] * 0.75);


		parameters->stepIndividualInitial[j]
				= (parameters->maximumSearchSpace[j] * 2.0) * 0.1;
		parameters->stepIndividualFinal[j] = (((parameters->maximumSearchSpace[j] - parameters->minimumSearchSpace[j]) / 2) * 2.0) * 0.0001;
				//(parameters->maximumSearchSpace[j] * 2.0) * 0.0001;
		parameters->stepVolitiveInitial[j]
				= (parameters->stepIndividualInitial[j] * 2) * 0.1;
		parameters->stepVolitiveFinal[j] = (parameters->stepIndividualFinal[j]
				* 2) * 0.1;

	}

	parameters->weight_scale = maxIterations / 2;
	parameters->initialWeightMax = parameters->weight_scale;
	parameters->initialWeightMin = parameters->weight_scale / 2;

	parameters->stagnancyMaxLimit = (int) (maxIterations * (maxIterations > 10000? 0.01:0.1));

	parameters->deltaLinearWeight = (maxIterations > 5000)?2:1.5; /*(double) maxIterations * 0.01;*/

	parameters->countevals = 0;

	if (debug == TRUE) {
		printf(
				"init_parameters - TFssParameters initialized :: dimensions: %d :: schoolSize: %d\n",
				parameters->dimensions, parameters->schoolSize);
		fflush(NULL);
	}

	return parameters;

}

void fss_init_positions(double **positions, double *initialLeftRange,
		double *initialRightRange, random_t *rand, int dimensions,
		int schoolSize) {
	int i = 0;
	int j = 0;
	if (debug == TRUE) {
		printf("fss_init_positions :: initialization start :: schoolSize: %d\n", schoolSize);
		fflush(NULL);
	}

	*positions = (double *) malloc((schoolSize * dimensions) * sizeof(double));

	for (i = schoolSize - 1; i >= 0; i--) {

		for (j = 0; j < dimensions; j++) {
			int index = i * dimensions + j;
			double r = (double) randomXOR(j);//fssrandom_Uniform(&rand);

			double p = (double) ((initialRightRange[j] - initialLeftRange[j])
					* ((double) (r / 0x7fffffff))) + initialLeftRange[j];

			if (debug == TRUE) {
				printf("fss_init_positions :: fish[%d]: position[%d]: %f\n", i, j, p);
				fflush(NULL);
			}

			if (p < initialLeftRange[j]) {
				p = initialLeftRange[j];
			}
			if (p > initialRightRange[j]) {
				p = initialRightRange[j];
			}

			(*positions)[index] = p;

			if (debug == TRUE) {
				printf("fss_init_positions :: pos after bounds[%d]: %f\n", j,
						(*positions)[index]);
				fflush(NULL);
			}

		}

	}
	if (debug == TRUE) {
		printf("fss_init_positions :: initialization :: end\n");
		fflush(NULL);
	}

}

void fss_init_steps(double **stepInd, double **stepIndPrev,
		double **stepAdaptive, double *minimumRange, double *maximunRange,
		random_t *rand, int dimensions, int schoolSize) {
	int i = 0;
	int j = 0;
	if (debug == TRUE) {
		printf("fss_init_steps start :: schoolSize: %d\n", schoolSize);
		fflush(NULL);
	}
	*stepInd = malloc(schoolSize * dimensions * sizeof(double));
	*stepIndPrev = malloc(schoolSize * dimensions * sizeof(double));
	*stepAdaptive = malloc(schoolSize * dimensions * sizeof(double));
	int index;
	for (i = 0; i < schoolSize; i++) {
		//f = (*school[i]).fish;
		//printf("fss_init_steps :: fish: %p\n", f);
		//fflush(NULL);
		for (j = 0; j < dimensions; j++) {
			double r = randomXOR(j); /*fssrandom_Uniform(&rand);*/

			/* 50% of the range length */
			double minDimRange = (double) (maximunRange[j] - minimumRange[j])
					/ 2;
			double maxDimRange = (double) (maximunRange[j]); // - minimumRange[j])
					//* 0.75;

			/*actual step*/
			double step = (double) (((maxDimRange - minDimRange)
					* ((double) (r / 0x7fffffff))) + (double) minDimRange);

			if (step < minDimRange) {
				step = minDimRange;
			}
			if (step > maxDimRange) {
				step = maxDimRange;
			}
			if (debug == TRUE) {
				printf("fss_init_steps:: fish[%d] step[%d]: %f\n", i, j, step);
				fflush(NULL);
			}

			index = (i * dimensions + j);
			//f->stepInd[j] = step;
			(*stepInd)[index] = step;
			if (debug == TRUE) {
				printf("fss_init_steps:: step atual: %f\n", (*stepInd)[index]);
				fflush(NULL);
			}

			/*previous step*/
			r = randomXOR(j);
			step = (double) (((maxDimRange - minDimRange) * ((double) (r
					/ 0x7fffffff))) + (double) minDimRange);
			if (debug == TRUE) {
				printf("fss_init_steps:: fish[%d] stepPrev[%d]: %f\n", i, j, step);
				fflush(NULL);
			}
			if (step < minDimRange) {
				step = minDimRange;
			}
			if (step > maxDimRange) {
				step = maxDimRange;
			}

			(*stepIndPrev)[index] = step;

			if (debug == TRUE) {
				printf("fss_init_steps :: step Prev[%d]: %f\n", j,
					(*stepIndPrev)[index]);
				fflush(NULL);
			}
		}

		(*stepAdaptive)[index] = 0;

	}
	if (debug == TRUE) {
		printf("fss_init_steps :: end\n");
		fflush(NULL);
	}

}

void fss_init_weights(double **weight, double **oldweight,
		double weightInitialMax, double weightInitialMin, random_t *rand,
		int schoolSize) {

	int i = 0;
	double w;
	double r;

	*weight = (double *) malloc(schoolSize * sizeof(double));
	*oldweight = (double *) malloc(schoolSize * sizeof(double));

	if (debug == TRUE) {
		printf("fss_init_weights start\n");
		fflush(NULL);
	}
	for (i = 0; i < schoolSize; i++) {
		r = randomXOR(i);
		w = (double) (weightInitialMin + ((r / 0x7fffffff) * (weightInitialMax
				- weightInitialMin)));

		if (debug == TRUE) {
			printf("weight: %f\n", w);
			fflush(NULL);
		}

		if (w < weightInitialMin) {
			w = weightInitialMin;
		}
		if (w > weightInitialMax) {
			w = weightInitialMax;
		}

		if (debug == TRUE) {
			printf("weight after bounds: %f\n", w);
			fflush(NULL);
		}

		(*weight)[i] = w;
		if (debug == TRUE) {
			printf("weight after bounds: %f\n", (*weight)[i]);
			fflush(NULL);
		}
		(*oldweight)[i] = w;

	}
	if (debug == TRUE) {
		printf("fss_init_weights :: end\n");
		fflush(NULL);
	}
}

void fss_fitness(double(*pFun)(double const *, int), double **positions,
		double **fitness, double **fitnessNeighbor, int **flagStagnancy,
		int **stagnancyCount, TFssData **data, TFssParameters **parameters) {
	// call objective function - calculate the fitness function for the initial positions.
	int i, j;
	int schoolSize;
	int dimensions;
	double f;

	schoolSize = (*parameters)->schoolSize;
	dimensions = (*parameters)->dimensions;
	*fitness = (double *) malloc(schoolSize * sizeof(double));
	*fitnessNeighbor = (double *) malloc(schoolSize * sizeof(double));
	*flagStagnancy = (int *) malloc(schoolSize * sizeof(int));
	*stagnancyCount = (int *) malloc(schoolSize * sizeof(int));

	double *pos = (double *) malloc(dimensions * sizeof(double));

	if (debug == TRUE) {
		printf("fss_fitness start\n");
		printf("schoolSize: %d\n", (*parameters)->schoolSize);
		fflush(NULL);
	}

	for (i = schoolSize - 1; i >= 0; i--) {
		for (j = 0; j < dimensions; j++) {
			if (debug == TRUE) {
				printf("fish: %d :: posicao inicial: %f\n", i, (*positions)[i * dimensions + j]);
				fflush(NULL);
			}
			pos[j] = (*positions)[i * dimensions + j];
		}
		f = (double) (*pFun)(pos, dimensions);
		if (debug == TRUE) {
			printf("fitness calculated[%d]: %f\n", i, f);
			fflush(NULL);
		}
		if (f == INFINITY || f <= 0.0) {
			f = (*parameters)->maximumSearchSpace[0];
		}

		(*fitness)[i] = f;
		(*fitnessNeighbor)[i] = f;

		if (debug == TRUE) {
			printf("fitness initial[%d]: %f\n", i, (*fitness)[i]);
			fflush(NULL);
		}
		if (i == 0) {
			if (debug == TRUE) {
				printf("first fitness: %f \n",(*fitness)[i]);
				fflush(NULL);
			}
			(*data)->gbest[0] = 0;
			(*data)->gbestFitness[0] = (*fitness)[i];
		} else {
			if ((*fitness)[i] < (*data)->gbestFitness[0]) {
				(*data)->gbest[0] = i;
				(*data)->gbestFitness[0] = (*fitness)[i];
			}
		}
		(*flagStagnancy)[i] = 0;
		(*stagnancyCount)[i] = 0;
	}

	if (debug == TRUE) {
		printf("fss_fitness:: ended\n");
		fflush(NULL);
	}

}

// movement: individual operator
int fss_individual_operator(double(*pFun)(double const *, int),
		double **positions, double **deltaX, double **fitness,
		double **fitnessNeighbor, double **deltaFitness, int **flagStagnancy,
		int **stagnancyCount, int **individual_move_success, double **stepInd,
		double **stepIndPrev, double **stepIndAdaptive, TFssData **data,
		TFssParameters **parameters) {
	int count_success = 0;
	int i, j;
	int dimensions;
	int schoolSize;
	int index;
	double elastic;

	dimensions = (*parameters)->dimensions;
	schoolSize = (*parameters)->schoolSize;
	double *neighbor = (double *) malloc(dimensions * sizeof(double));
	*deltaX = (double *) malloc(schoolSize * dimensions * sizeof(double));
	*deltaFitness = (double *) malloc(schoolSize * sizeof(double));
	*individual_move_success = (int *) malloc(schoolSize * sizeof(int));

	if (debug == TRUE) {
		printf("fss_individual_operator begin :: dim: %d :: peixes: %d\n",
			dimensions, schoolSize);
		fflush(NULL);
	}

	for (i = schoolSize - 1; i >= 0; i--) {
		//use current as template for neighbor
		//clear displacement
		for (j = 0; j < dimensions; j++) {
			index = (i * dimensions + j);
			elastic = ((*parameters)->maximumSearchSpace[j] - (*parameters)->minimumSearchSpace[j]) * 0.01;
			if (debug == TRUE) {
				printf("index: %d\n", index);
				fflush(NULL);
				printf("positions pointer: %p\n", (*positions));
				fflush(NULL);
			}
			neighbor[j] = (*positions)[index];
			if (debug == TRUE) {
				printf("current position: %f\n", neighbor[j]);
				fflush(NULL);
			}
			(*deltaX)[index] = 0;
			double r = randomXOR(j); //((double) fssrandom_Uniform(&((*data)->rand)))
			double noise = (double) (r / 0x7fffffff);

			r = randomXOR(j);
			double direction = (double) 1 - 2 * (double) (r / 0x7fffffff);

			double nearPosition = 0.;

			if (debug == TRUE) {
				printf("is Stagned: %d\n", (*flagStagnancy)[i]);
				fflush(NULL);
			}
			if ((*flagStagnancy)[i] == 0) {
				if (debug == TRUE) {
					printf("data:: dim[%d]:: stepInd: %f \n",j,(*data)->stepInd[j]);
					fflush(NULL);
				}
				nearPosition = (double) (*data)->stepInd[j] * noise
						* direction;
			} else { /* adaptive individual update step */
				if ((*stepIndAdaptive)[index] != 0) {
					nearPosition = (double) (*stepIndAdaptive)[index] * noise
							* direction;
				}
			}

			if (debug == TRUE) {
				printf("adjust to near position: %f\n", nearPosition);
				fflush(NULL);
			}

			neighbor[j] += nearPosition;

			if (debug == TRUE) {
				printf("calculated position: %f\n",  neighbor[j]);
				fflush(NULL);
			}

			//take care about bounds of search space

 	 	 	//bounce
			if (neighbor[j] < ((*parameters)->minimumSearchSpace[j] - elastic)) {
				neighbor[j] = sqrt(pow((neighbor[j] - (*parameters)->minimumSearchSpace[j]),2.0)) / 2;
				if (debug == TRUE) {
					printf("fss_individual_operator :: elastico min::neighbor[%d]: %f\n",
						j, neighbor[j]);
					fflush(NULL);
				}
			}
			if (neighbor[j] > ((*parameters)->maximumSearchSpace[j] + elastic)) {
				neighbor[j] = sqrt(pow((neighbor[j] - (*parameters)->maximumSearchSpace[j]),2.0)) / 2;
				if (debug == TRUE) {
					printf("fss_individual_operator :: elastico max::neighbor[%d]: %f\n",
						j, neighbor[j]);
					fflush(NULL);
				}
			}
/*
			if (neighbor[j] < (*parameters)->minimumSearchSpace[j])
				neighbor[j] = (*parameters)->minimumSearchSpace[j];
			if (neighbor[j] > (*parameters)->maximumSearchSpace[j])
				neighbor[j] = (*parameters)->maximumSearchSpace[j];

*/
			//calculate displacement
			(*deltaX)[index] = neighbor[j] - (*positions)[index];

			if (debug == TRUE) {
				printf(
						"		fss_individual_operator fim dimension :: neighbor[%d]: %f\n",
						j, neighbor[j]);
				fflush(NULL);
				printf(
						"		fss_individual_operator fim dimension :: deltaX[%d]: %f\n",
						index, (*deltaX)[index]);
				fflush(NULL);
			}
		}

		//evaluate new current solution
		double fit = (double) (*pFun)(neighbor, dimensions);

		if (fit == INFINITY || fit == -INFINITY) {
			fit = (*parameters)->maximumSearchSpace[0];
		}

		(*fitnessNeighbor)[i] = fit;
		(*parameters)->countevals++;

		//calculate fitness gain
		//on minimization (current - neighbor)
		//on maximization (neighbor - current)
		(*deltaFitness)[i] = (*fitness)[i] - (*fitnessNeighbor)[i];
		if (debug == TRUE) {
			printf("fss_individual_operator :: deltafitness: %f\n", (*deltaFitness)[i]);
			fflush(NULL);
		}

		//update current if neighbor is best
		(*individual_move_success)[i] = 0;
		if (debug == TRUE) {
			printf("		fss_individual_operator fitness %f\n", (*fitnessNeighbor)[i]);
			fflush(NULL);
			printf("ind 0 \n");
			fflush(NULL);
		}
		if ((*fitnessNeighbor)[i] < (*fitness)[i]) {
			if (debug == TRUE) {
				printf("ind 1 :: neighbor better\n");
				fflush(NULL);
			}
			//"neighbor" better
			for (j = 0; j < dimensions; j++) {
				index = (i * dimensions + j);
				if (debug == TRUE) {
					printf("ind 1a :: i:: %d j: %d :: pos: %f \n", i, j,
							(*positions)[index]);
					fflush(NULL);
					printf("ind 1aa :: neigh: %f \n", neighbor[j]);
					fflush(NULL);
				}
				(*positions)[index] = neighbor[j];
			}
			if (debug == TRUE) {
				printf("ind 1b \n");
				fflush(NULL);
			}
			(*fitness)[i] = (*fitnessNeighbor)[i];

			if (debug == TRUE) {
				printf("ind 1c \n");
				fflush(NULL);
			}
			//if need replace best solution
			if ((*fitness)[i] < (*data)->gbestFitness[0]) {
				if (debug == TRUE) {
					printf("ind 1d \n");
					fflush(NULL);
				}
				//"current fish" global best);
				(*data)->gbest[0] = i;
				(*data)->gbestFitness[0] = (*fitness)[i];
				if (debug == TRUE) {
					printf("ind 1e \n");
					fflush(NULL);
				}
			}

			if (debug == TRUE) {
				printf("ind 1f \n");
				fflush(NULL);
			}
			//school[i]->positions = neighbor;
			(*individual_move_success)[i] = 1;
			if (debug == TRUE) {
				printf("ind 1g \n");
				fflush(NULL);
			}
			count_success++;
			if (debug == TRUE) {
				printf("ind 1h \n");
				fflush(NULL);
			}
			(*flagStagnancy)[i] = 0;
			if (debug == TRUE) {
				printf("ind 1i \n");
				fflush(NULL);
			}
			(*stagnancyCount)[i] = 0;
			if (debug == TRUE) {
				printf("ind 1j \n");
				fflush(NULL);
			}
		} else {
			if (debug == TRUE) {
				printf("ind 2 \n");
				fflush(NULL);
			}
			/* adaptive individual step */
			if ((*flagStagnancy)[i] == 0) {
				if (debug == TRUE) {
					printf("ind 3 \n");
					fflush(NULL);
				}
				(*stagnancyCount)[i] += 1;
				if (debug == TRUE) {
					printf("ind 4 \n");
					fflush(NULL);
				}
				if ((*stagnancyCount)[i] > (*parameters)->stagnancyMaxLimit) {
					if (debug == TRUE) {
						printf("ind 5 \n");
						fflush(NULL);
					}
					(*flagStagnancy)[i] = 1;
					if (debug == TRUE) {
						printf("ind 6 \n");
						fflush(NULL);
					}
					(*stagnancyCount)[i] = 0;
					if (debug == TRUE) {
						printf("ind 7 \n");
						fflush(NULL);
					}

				}
				if (debug == TRUE) {
					printf("ind 8 \n");
					fflush(NULL);
				}
			}
			if (debug == TRUE) {
				printf("ind 9 \n");
				fflush(NULL);
			}
		}

		if (debug == TRUE) {
			printf("ind 10 \n");
			fflush(NULL);
		}
		if ((*flagStagnancy)[i] == 1) {
			if (debug == TRUE) {
				printf("ind 11 \n");
				fflush(NULL);
			}
			for (j = 0; j < dimensions; j++) {
				index = (i * dimensions + j);
				if (debug == TRUE) {
					printf("ind 12 \n");
					fflush(NULL);
					printf("si[%d]:%f ::  fi:%f \n so[%d]:%f :: fo:%f \n", j,
							(*stepInd)[index], (*fitnessNeighbor)[i], j,
							(*stepIndPrev)[index], (*fitness)[i]);
					fflush(NULL);
				}
				if (((*fitnessNeighbor)[i] != 0) && ((*fitness)[i] != 0)) {
					(*stepIndAdaptive)[index] = ((*stepInd)[index]
							/ ((*fitnessNeighbor)[i])) / ((*stepIndPrev)[index]
							/ ((*fitness)[i]));
					(*stepIndPrev)[index] = (*stepInd)[index];
					(*stepInd)[index] = (*stepIndAdaptive)[index];
				} else {
					(*stepIndAdaptive)[index] = 0;
				}
			}
		}

		if (debug == TRUE) {
			printf("	fss_individual_operator peixe - end\n");
			fflush(NULL);
		}
	}

	if (debug == TRUE) {
		printf("fss_individual_operator end \n");
		fflush(NULL);
	}
	//	return count_success;
	free(neighbor);
	neighbor = NULL;

	return count_success;

}

void fss_feeding_operator(double **weight, double **oldweight,
		double **deltaFitness, double **deltaFitnessNormalized,
		TFssParameters **parameters) {
	//sort school by fitness gain
	int i;
	int schoolSize;

	if (debug == TRUE) {
		printf("fss_feeding_operator begin\n");
		fflush(NULL);
	}
	//check the highest value of fitness gain.
	schoolSize = (*parameters)->schoolSize;
	i = schoolSize - 1;

	*deltaFitnessNormalized = (double *) malloc(schoolSize * sizeof(double));

	double maxFitnessGain = abs((*deltaFitness)[i]);
	i--;

	for (; i >= 0; i--) {
		if (debug == TRUE) {
			printf("		fss_feeding_operator delta fitness[%d]: %f\n", i,(*deltaFitness)[i]);
			fflush(NULL);
		}
		if (maxFitnessGain < abs((*deltaFitness)[i])) {
			maxFitnessGain = abs((*deltaFitness)[i]);
		}
	}
	if (debug == TRUE) {
		printf("		fss_feeding_operator maxFitnessGain: %f\n", maxFitnessGain);
		fflush(NULL);
	}

	//calculate normalized gain
	for (i = schoolSize - 1; i >= 0; i--) {
		if (maxFitnessGain != 0) {
			(*deltaFitnessNormalized)[i] = (*deltaFitness)[i] / maxFitnessGain;
		} else {
			(*deltaFitnessNormalized)[i] = 0;
		}
	}

	//feed all fishes
	for (i = schoolSize - 1; i >= 0; i--) {
		(*oldweight)[i] = (*weight)[i];
		(*weight)[i] += (*deltaFitnessNormalized)[i];

		//take care about min and max weight
		if ((*weight)[i] < (*parameters)->initialWeightMin)
			(*weight)[i] = (*parameters)->initialWeightMin;
		if ((*weight)[i] > (*parameters)->initialWeightMax)
			(*weight)[i] = (*parameters)->initialWeightMax;

		if (debug == TRUE) {
			printf("	fss_feeding_operator :: fish [%d]: weight: %f\n", i,
					(*weight)[i]);
			fflush(NULL);
		}
	}

	if (debug == TRUE) {
		printf("fss_feeding_operator end\n");
		fflush(NULL);
	}

}

int fss_instinctive_operator(double(*pFun)(double const *, int),
		double **positions, double **fitness, double **fitnessNeighbor,
		double **deltaX, double **deltaFitnessNormalized,
		int **individual_move_success, int **instinctive_move_success, TFssData **data,
		TFssParameters **parameters) {
	//
	double school_instinctive[(*parameters)->dimensions];

	double sum_prod[(*parameters)->dimensions];

	double sum_fitness_gain = 0;

	int i, j;
	int dimensions;
	int schoolSize;

	dimensions = (*parameters)->dimensions;
	schoolSize = (*parameters)->schoolSize;

	if (debug == TRUE) {
		printf("fss_instinctive_operator :: begin \n");
		fflush(NULL);
	}

	*instinctive_move_success = (int *) malloc(schoolSize * sizeof(int));
	int count_success = 0;
	//clear
	for (j = 0; j < dimensions; j++) {
		sum_prod[j] = 0;
		school_instinctive[j] = 0;
	}

	//for each fish contribute with your direction scaled by your fitness gain
	for (i = schoolSize - 1; i >= 0; i--) {
		//only good fishes
		if ((*individual_move_success)[i] != 0) {
			//sum product of solution by fitness gain
			for (j = 0; j < dimensions; j++) {
				sum_prod[j] += ((*deltaX)[i * dimensions + j]) * ((*deltaFitnessNormalized)[i]);
				if (debug == TRUE) {
					printf("instinctive: sum_prod[%d]: %f\n", j,sum_prod[j]);
					fflush(NULL);
				}
			}
			//sum fitness gains
			sum_fitness_gain += (*deltaFitnessNormalized)[i];
		}
	}

	if (debug == TRUE) {
		printf("instinctive: sum_fitness_gain: %f\n", sum_fitness_gain);
		fflush(NULL);
	}
	//calculate global direction of good fishes
	for (j = 0; j < dimensions; j++) {
		//take care about zero division
		if (sum_fitness_gain != 0) {
			school_instinctive[j] = sum_prod[j] / sum_fitness_gain;
		} else {
			school_instinctive[j] = 0;
		}
		if (debug == TRUE) {
			printf("instinctive: school_instinctive[%d]: %f\n", j, school_instinctive[j]);
			fflush(NULL);
		}
	}

	if (debug == TRUE) {
		printf("	fss_instinctive_operator :: calculating new positions \n");
		fflush(NULL);
	}
	//
	double *neighbor = (double *) calloc(dimensions, sizeof(double));
	//	int count_success=0;
	for (i = schoolSize - 1; i>=0; i--) {

		for (j = 0; j < dimensions; j++) {
			neighbor[j] = (*positions)[i * dimensions + j];
			if (debug == TRUE) {
				printf("instinctive: pos[%d]: %f\n", j, neighbor[j]);
				fflush(NULL);
			}
			neighbor[j] += school_instinctive[j];
			//take care about bounds of search space

			if (neighbor[j] < (*parameters)->minimumSearchSpace[j])
				neighbor[j] = (*parameters)->minimumSearchSpace[j];
			if (neighbor[j] > (*parameters)->maximumSearchSpace[j])
				neighbor[j] = (*parameters)->maximumSearchSpace[j];


			if (debug == TRUE) {
				printf("instinctive: new pos[%d]: %f\n", j, neighbor[j]);
				fflush(NULL);
			}
		}

		//		school[i]->positions = neighbor;

		if (debug == TRUE) {
			printf("		fss_instinctive_operator :: call objective function \n");
			fflush(NULL);
		}

		for (j = 0; j < dimensions; j++) {
			(*positions)[i * dimensions + j] = neighbor[j];
		}

	}

	free(neighbor);
	neighbor = NULL;
	if (debug == TRUE) {
		printf("fss_instinctive_operator :: end \n");
		fflush(NULL);
	}


	return count_success;

}

int fss_volitive_operator(double(*pFun)(double const *, int),
		double **positions, double **deltaX, double **fitness,
		double **fitnessNeighbor, double **weight, double **oldWeight,
		int **volitive_move_success, TFssData **data,
		TFssParameters **parameters) {

	double school_barycentre[(*parameters)->dimensions];
	double normalized_barycentre[(*parameters)->dimensions];
	double sum_prod[(*parameters)->dimensions];
	double sum_weight_now = 0;
	double sum_weight_past = 0;
	int i;
	int j;
	int count_success = 0;
	int dimensions;
	int schoolSize;
	double elastic;

	dimensions = (*parameters)->dimensions;
	schoolSize = (*parameters)->schoolSize;

	*volitive_move_success = (int *) malloc(schoolSize * sizeof(int));
	double *neighbor = (double *) calloc(dimensions, sizeof(double));//fss_NewDouble((*parameters)->dimensions);

	if (debug == TRUE) {
		printf("fss_volitive_operator :: begin \n");
		fflush(NULL);
	}

	//clear
	for (j = 0; j < dimensions; j++) {
		sum_prod[j] = 0;
		school_barycentre[j] = 0;
	}

	if (debug == TRUE) {
		printf("	fss_volitive_operator :: calculating contribution of each fish \n");
		fflush(NULL);
	}

	//for each fish contribute with your position and weight
	for (i = schoolSize - 1; i >= 0; i--) {
		//sum of deltaX and fish weight product;
		for (j = 0; j < dimensions; j++) {
			//"volitive for fish deltax"
			sum_prod[j] += ((*deltaX)[i * dimensions + j]) * ((*weight)[i]);
		}
		//sum weight
		sum_weight_now += ((*weight)[i]);
		sum_weight_past += ((*oldWeight)[i]);
	}

	double direction = 0;

	if (debug == TRUE) {
		printf("weight now: %f\n", sum_weight_now);
		printf("weight old: %f\n", sum_weight_past);
		fflush(NULL);
	}
	if (sum_weight_now >= sum_weight_past) {
		//weight gain -> contract
		direction = -1;
	} else {
		/*weight loss -> dilate */
		if ((*parameters)->janecek) {
			direction = (*parameters)->deltaLinearWeight;
			/* reseting weights */
			for (i = schoolSize - 1; i >= 0; i--) {
				(*weight)[i] = 1.0;
			}
		} else {
			direction = 1.0;
		}
	}

	if (debug == TRUE) {
		printf("	fss_volitive_operator :: calculating barycentre \n");
		fflush(NULL);
	}

	//calculate barycentre
	for (j = 0; j < dimensions; j++) {
		school_barycentre[j] = sum_prod[j] / sum_weight_now;
		if (debug == TRUE) {
			printf("baricentre[%d]: %f\n", j, school_barycentre[j]);
			fflush(NULL);
		}
		(*data)->barycentre[j] = school_barycentre[j];
	}

	if (debug == TRUE) {
		printf("		fss_volitive_operator :: move \n");
		fflush(NULL);
	}

	for (i = schoolSize - 1; i >= 0; i--) {
		for (j = 0; j < dimensions; j++) {
			//normalizing barycentre
			normalized_barycentre[j] = ((*positions)[i * dimensions + j]) - school_barycentre[j];
		}

		double soma = 0;
		for (j = 0; j < dimensions; j++) {
			soma += pow(normalized_barycentre[j], 2);
		}

		double modulo = (double) abs(sqrt(soma));

		for (j = 0; j < dimensions; j++) {
			if (modulo > 0) {
				normalized_barycentre[j] /= modulo;
			}
			if (debug == TRUE) {
				printf("normalized baricentre[%d]: %f\n", j,
					normalized_barycentre[j]);
				fflush(NULL);
			}
		}

		for (j = 0; j < dimensions; j++) {
			neighbor[j] = (*positions)[i * dimensions + j];
			double r = randomXOR(j);
			r /= 0x7fffffff;
			if (debug == TRUE) {
				printf("volitivo r: %f \n", r);
				fflush(NULL);
			}
			if (debug == TRUE) {
				printf("step vol: %f\n", (*data)->stepVol[j]);
				fflush(NULL);
			}
			double valueStep = (*data)->stepVol[j] * r;
			if (debug == TRUE) {
				printf("valueStep: %f\n", valueStep);
				fflush(NULL);
			}
			double sum = (neighbor[j] + normalized_barycentre[j]);
			if (debug == TRUE) {
				printf("sum: %f\n", sum);
				printf("volitivo neighbor[%d]: %f\n", j, neighbor[j]);
				printf(
						"volitivo:: \n dim: %d \n sum: %f \n valueStep:%f \n :: direct: %f\n",
						j, sum, valueStep, direction);
				fflush(NULL);
			}
			neighbor[j] = (double) ((sum * valueStep) * direction);
			//fish.neighbor.variables[i] += step_size * direction * r.between(0,1) * (fish.neighbor.variables[i]-school_barycentre[i]);

			if (debug == TRUE) {
				printf("volitive: neighbor[%d]: %f\n", j, neighbor[j]);
				fflush(NULL);
			}

			elastic = ((*parameters)->maximumSearchSpace[j] - (*parameters)->minimumSearchSpace[j]) * 0.01;
 	 	 	//bounce
			if (neighbor[j] < ((*parameters)->minimumSearchSpace[j] - elastic)) {
				neighbor[j] = sqrt(pow((neighbor[j] - (*parameters)->minimumSearchSpace[j]),2.0)) / 2;
				if (debug == TRUE) {
					printf("volitive :: elastico min::neighbor[%d]: %f\n",
							j, neighbor[j]);
					fflush(NULL);
				}
			}
			if (neighbor[j] > ((*parameters)->maximumSearchSpace[j] + elastic)) {
				neighbor[j] = sqrt(pow((neighbor[j] - (*parameters)->maximumSearchSpace[j]),2.0)) / 2;
				if (debug == TRUE) {
					printf("volitive :: elastico max::neighbor[%d]: %f\n",
							j, neighbor[j]);
					fflush(NULL);
				}
			}
/*
			if (neighbor[j] < (*parameters)->minimumSearchSpace[j])
				neighbor[j] = (*parameters)->minimumSearchSpace[j];
			if (neighbor[j] > (*parameters)->maximumSearchSpace[j])
				neighbor[j] = (*parameters)->maximumSearchSpace[j];

*/
			if (debug == TRUE) {
				printf("volitive after bounds: neighbor[%d]: %f\n", j, neighbor[j]);
				fflush(NULL);
			}
		}
		//evaluate new current solution
		double fit = (double) (*pFun)(neighbor, dimensions);

		if (fit == INFINITY || fit == -INFINITY) {
			fit = (*parameters)->maximumSearchSpace[0];
		}

		(*fitnessNeighbor)[i] = fit;
		(*parameters)->countevals++;

		//update current if neighbor is best
		(*volitive_move_success)[i] = 0;

		if (debug == TRUE) {
			printf("volitivo: fitness[%d]: %f \n", i, (*fitness)[i]);
			printf("volitivo: fitneighbor[%d]: %f \n", i, (*fitnessNeighbor)[i]);
			fflush(NULL);
		}

		if ((*fitnessNeighbor)[i] < (*fitness)[i]) {
			if (debug == TRUE) {
				printf("## IMPROVED ##\n");
				fflush(NULL);
			}
			for (j = 0; j < dimensions; j++) {
				(*positions)[i * dimensions + j] = neighbor[j];
			}
			(*fitness)[i] = (*fitnessNeighbor)[i];
			(*volitive_move_success)[i] = 1;
			count_success++;
		}

		//if need replace best solution
		if ((*fitness)[i] < (*data)->gbestFitness[0]) {
			//"current fish" global best);
			(*data)->gbest[0] = i;
			(*data)->gbestFitness[0] = (*fitness)[i];
		}
	}

	free(neighbor);
	neighbor = NULL;

	if (debug == TRUE) {
		printf("fss_volitive_operator :: end \n");
		fflush(NULL);
	}

	return count_success;

}

void fss_update_steps(TFssData **data, TFssParameters **parameters) {
	int j;
	int dimensions;

	dimensions = (*parameters)->dimensions;

	if (debug == TRUE) {
		printf("fss_update_steps :: begin \n");
		fflush(NULL);
	}

	double step;

	/* steps updating - linear decay */
	for (j = 0; j < dimensions; ++j) {
		if (debug == TRUE) {
			printf("fss_update_steps :: ind final [%d]: %f \n", j, (*parameters)->stepIndividualFinal[j]);
			printf("fss_update_steps :: ind[%d]: %f \n", j, (*data)->stepInd[j]);
			fflush(NULL);
		}
		step = (*data)->stepInd[j];

		if (step > (*parameters)->stepIndividualFinal[j]) {
			step = step - (double) (((*parameters)->stepIndividualInitial[j]
							- (*parameters)->stepIndividualFinal[j])
							/ (*parameters)->countevals);

		}
		if (step < (*parameters)->stepIndividualFinal[j]) {
			step = (*parameters)->stepIndividualFinal[j];
		}
		(*data)->stepInd[j] = step;
		if (debug == TRUE) {
			printf("fss_update_steps :: ind[%d] calculado: %f \n", j, (*data)->stepInd[j]);

			printf("fss_update_steps :: vol final [%d]: %f \n", j, (*parameters)->stepVolitiveFinal[j]);

			printf("fss_update_steps :: vol[%d]: %f \n", j, (*data)->stepVol[j]);
			fflush(NULL);
		}

		step = (*data)->stepVol[j];
		if (step > (*parameters)->stepVolitiveFinal[j]) {
			step = step - (double) (((*parameters)->stepVolitiveInitial[j]
							- (*parameters)->stepVolitiveFinal[j])
							/ (*parameters)->countevals);
		}
		if (step < (*parameters)->stepVolitiveFinal[j]) {
			step = (*parameters)->stepVolitiveFinal[j];
		}
		(*data)->stepVol[j] = step;
		if (debug == TRUE) {
			printf("fss_update_steps :: vol[%d] calculado: %f \n", j, (*data)->stepVol[j]);
			fflush(NULL);
		}

	}

	if (debug == TRUE) {
		printf("fss_update_steps :: end \n");
		fflush(NULL);
	}

}

void weightUpdateStrategiesS1(double **weight, TFssParameters **parameters) {
	int i;
	int schoolSize;
	int dimensions;

	schoolSize = (*parameters)->schoolSize;
	dimensions = (*parameters)->dimensions;

	if (debug == TRUE) {
		printf("weightUpdateStrategiesS1 :: begin \n");
		fflush(NULL);
	}
	/* Janecek and Tan weight update strategy (S1) */
	for (i = 0; i < schoolSize; i++) {
		(*weight)[i] -= (*parameters)->deltaLinearWeight;
		if ((*weight)[i] < 1.0) {
			(*weight)[i] = 1.0;
		}
	}
	if (debug == TRUE) {
		printf("weightUpdateStrategiesS1 :: end \n");
		fflush(NULL);
	}

}

void weightUpdateStrategiesS2(double **fitness, double **weight,
		TFssParameters **parameters) {
	/* Janecek and Tan weight update strategy (S2) */
	int i;
	int schoolSize;
	int dimensions;
	double min; /* Minimum value in your interval */
	double max; /* Maximum value in your interval */
	double deltafitNormal;

	double cFit = 4;

	schoolSize = (*parameters)->schoolSize;
	dimensions = (*parameters)->dimensions;

	min = 9999.0;
	max = -1;
	if (debug == TRUE) {
		printf("weightUpdateStrategiesS2 :: begin \n");
		fflush(NULL);
	}

	/* min and max */
	for (i = 0; i < schoolSize; i++) {
		if ((*fitness)[i] < min) {
			min = (*fitness)[i];
		}
		if ((*fitness)[i] > max) {
			max = (*fitness)[i];
		}
	}

	/* weight update */
	for (i = 0; i < schoolSize; i++) {
		deltafitNormal = ((*fitness)[i] - min) / (max - min);

		deltafitNormal = pow(deltafitNormal, 2) / cFit;

		(*weight)[i] -= deltafitNormal;

		if ((*weight)[i] < 1.0) {
			(*weight)[i] = 1.0;
		}
	}
	if (debug == TRUE) {
		printf("weightUpdateStrategiesS2 :: end \n");
		fflush(NULL);
	}
}

void stepSizeUpdateStrategiesS3(TFssData **data, TFssParameters **parameters,
		int iteration) {
	/* Janecek and Tan - step size update strategy (S3) */
	int j;
	int dimensions;

	dimensions = (*parameters)->dimensions;

	double a2;
	double b2;
	double t2;
	if (debug == TRUE) {
		printf("stepSizeUpdateStrategiesS3 :: begin \n");
		fflush(NULL);
	}

	a2 = pow((*parameters)->maxNumberOfIterations, 2);
	if (debug == TRUE) {
		printf("stepSizeUpdateStrategiesS3 :: a2: %f \n",a2);
		fflush(NULL);
	}
	t2 = pow(iteration, 2);
	if (debug == TRUE) {
		printf("stepSizeUpdateStrategiesS3 :: t2: %f \n",t2);
		fflush(NULL);
	}

	double step;

	/* steps updating - linear decay */
	for (j = 0; j < dimensions; ++j) {
		b2 = (*parameters)->stepIndividualInitial[j]
						- (*parameters)->stepIndividualFinal[j];
		b2 = pow(b2, 2);
		if (debug == TRUE) {
			printf("stepSizeUpdateStrategiesS3 :: b2: %f\n", b2);
		}

		if (debug == TRUE) {
			printf("stepSizeUpdateStrategiesS3 :: stepInd: %f :: (*parameters)->stepIndividualFinal[%d]: %f\n", (*data)->stepInd[j], j, (*parameters)->stepIndividualFinal[j]);
		}

		if ((*data)->stepInd[j] > (*parameters)->stepIndividualFinal[j]) {
			if (debug == TRUE) {
				printf("stepSizeUpdateStrategiesS3 :: stepInd: %f\n", (*data)->stepInd[j]);
			}

			step = (*data)->stepInd[j];

			double calc = (double) ((*parameters)->stepIndividualInitial[j] - sqrt(
					((1 - (t2 / a2)) * b2)));

			if (debug == TRUE) {
				printf("stepSizeUpdateStrategiesS3 :: stepInd: calc: %f\n", calc);
			}

			step -= calc;

			if (step < (*parameters)->stepIndividualFinal[j]) {
				step = (*parameters)->stepIndividualFinal[j];
			}

			(*data)->stepInd[j] = step;
		}
		if (debug == TRUE) {
			printf("stepSizeUpdateStrategiesS3 :: stepInd: %f \n", (*data)->stepInd[j]);
		}

		if ((*data)->stepVol[j] > (*parameters)->stepVolitiveFinal[j]) {
			step = (*data)->stepInd[j] * 0.1;
			(*data)->stepVol[j] = (double) 2.0 * step;
		}
		if (debug == TRUE) {
			printf("stepSizeUpdateStrategiesS3 :: stepVol: %f\n", (*data)->stepVol[j]);
		}

	}
	if (debug == TRUE) {
		printf("stepSizeUpdateStrategiesS3 :: end \n");
		fflush(NULL);
	}

}

static void allocating_memory_uint(int **item) {
	// Now allocate memory space
	int *it = (int *) calloc(1, sizeof(int));
	*item = it;
}

static void allocating_memory_double1D(double **item, int columns) {
	// Now allocate memory space
	double *it = fss_NewDouble(columns);
	*item = it;
	//printf("allocating_memory_double1D :: item: %p\n", item);
}

void fss_exit(double **positions, double **deltaX, double **fitness,
		double **fitnessNeighbor, double **deltaFitness,
		double **deltaFitnessNormalized, double **weight, double **oldWeight,
		double **stepInd, double **stepIndPrev, double **stepIndAdaptive,
		int **individual_move_success, int **volitive_move_success,
		int **stagnancyCount, int **flagStagnancy,
		TFssData **data, TFssParameters **parameters) {
	if (debug == TRUE) {
		printf("fss_exit :: Free memory : begin\n");
		fflush(NULL);
	}

	free(*positions);
	free(*deltaX);
	free(*deltaFitness);
	free(*deltaFitnessNormalized);
	free(*fitness);
	free(*fitnessNeighbor);
	free(*oldWeight);
	free(*weight);
	free(*stepInd);
	free(*stepIndPrev);
	free(*stepIndAdaptive);
	free(*individual_move_success);
	free(*volitive_move_success);
	free(*stagnancyCount);
	free(*flagStagnancy);

	*positions = NULL;
	*stepInd = NULL;
	*stepIndAdaptive = NULL;
	*stepIndPrev = NULL;
	*weight= NULL;
	*fitness= NULL;

	*deltaX= NULL;
	*oldWeight= NULL;
	*deltaFitness= NULL;
	*deltaFitnessNormalized= NULL;
	*fitnessNeighbor= NULL;
	*individual_move_success= NULL;
	*volitive_move_success= NULL;
	*stagnancyCount = NULL;
	*flagStagnancy = NULL;


	if (data != NULL) {
		free(*data);
	}
	if (parameters != NULL) {
		free(*parameters);
	}

	if (debug == TRUE) {
		printf("fss_exit :: Free memory : end\n");
		fflush(NULL);
	}

}

/* --------------------------------------------------------- */
static void* new_voidFss(int n, size_t size) {
	static char s[70];
	void* p = calloc((unsigned) n, size);
	if (p == NULL) {
		sprintf(s, "new_void(): calloc(%ld,%ld) failed",(long)n,(long)size);
		//		FATAL(s, 0, 0, 0);
	}
	return p;
}

double *
fss_NewDouble(int n) {
	return new_doubleFss(n);
}

static double * new_doubleFss(int n) {
	static char s[170];
	double *p = (double *) calloc((unsigned) n, sizeof(double));
	if (p == NULL) {
		sprintf(s, "new_double(): calloc(%ld,%ld) failed",
				(long)n,(long)sizeof(double));
		//		FATAL(s, 0, 0, 0);
	}
	return p;
}

double **
fss_NewDouble2D(int n) {
	return new_double2DFss(n);
}

static double ** new_double2DFss(int n) {
	static char s[170];
	double **p = (double **) calloc((unsigned) n, sizeof(double*));
	if (p == NULL) {
		sprintf(s, "new_double2D(): calloc(%ld,%ld) failed",
				(long)n,(long)sizeof(double*));
		//		FATAL(s, 0, 0, 0);
	}
	return p;
}

/* --------------------------------------------------------- */
/* ---------------- Functions: random_t -------------------- */
/* --------------------------------------------------------- */
/* --------------------------------------------------------- */
/* X_1 exakt :          0.79788456)  */
/* chi_eins simuliert : 0.798xx   (seed -3) */
/*                    +-0.001 */
/* --------------------------------------------------------- */
/*
 Gauss() liefert normalverteilte Zufallszahlen
 bei vorgegebenem seed.
 */
/* --------------------------------------------------------- */
/* --------------------------------------------------------- */

long fssrandom_init(random_t *t, long unsigned inseed) {

	clock_t cloc = clock();

	t->flgstored = 0;
	t->rgrand = (long *) new_voidFss(32, sizeof(long));
	if (inseed < 1) {
		while ((long) (cloc - clock()) == 0)
			; /* TODO: remove this for time critical applications? */
		inseed = (long) abs(100 * time(NULL) + clock());
	}
	long unsigned ret = fssrandom_Start(t, &inseed);
	if (debug == TRUE) {
		printf("fssrandom_init 6 : t: %p :: inseed: %lu :: retorno: %lu\n", t,
			inseed, ret);
		fflush(NULL);
	}
	return ret;
}

void fssrandom_exit(random_t *t) {
	free(t->rgrand);
}

/* --------------------------------------------------------- */
long unsigned fssrandom_Start(random_t *t, long unsigned *inseed) {
	long tmp;
	int i;

	t->flgstored = 0;
	t->startseed = inseed[0];
	if (inseed[0] < 1)
		inseed[0] = 1;
	t->aktseed = inseed[0];
	for (i = 0; i < 40; i++) {
		tmp = t->aktseed / 127773;
		t->aktseed = 16807 * (t->aktseed - tmp * 127773) - 2836 * tmp;
		if (t->aktseed < 0)
			t->aktseed += 2147483647;
		if (i < 32) {
			t->rgrand[i] = t->aktseed;
		}
	}
	t->aktrand = t->rgrand[0];
	if (debug == TRUE) {
		printf("starting random_t: %lu \n", t->aktrand);
		fflush(NULL);

		printf("end of starting random_t: %p - inseed: %lu\n", t, inseed[0]);
		fflush(NULL);
	}
	return inseed[0];
}

/* --------------------------------------------------------- */
double fssrandom_Gauss(random_t *t) {
	double x1, x2, rquad, fac;

	if (t->flgstored) {
		t->flgstored = 0;
		return t->hold;
	}
	do {
		x1 = 2.0 * fssrandom_Uniform(t) - 1.0;
		x2 = 2.0 * fssrandom_Uniform(t) - 1.0;
		rquad = x1 * x1 + x2 * x2;
	} while (rquad >= 1 || rquad <= 0);
	fac = sqrt(-2.0 * log(rquad) / rquad);
	t->flgstored = 1;
	t->hold = fac * x1;
	return fac * x2;
}

/* --------------------------------------------------------- */
double fssrandom_Uniform(random_t *t) {
	long int tmp;
	tmp = t->aktseed / 127773;
	t->aktseed = 16807 * (t->aktseed - tmp * 127773) - 2836 * tmp;
	if (t->aktseed < 0) {
		t->aktseed += 2147483647;
	}
	tmp = t->aktrand / 67108865;

	t->aktrand = t->rgrand[tmp];
	t->rgrand[tmp] = t->aktseed;

	double ret = (double) (t->aktrand / 2.147483647e9);
	return ret;
}

static int randomXOR(int id) {
	int randomNumber;
	randomNumber = clock() + clock() * id;
	randomNumber = randomNumber ^ (randomNumber << 21);
	//randomNumber = randomNumber^(randomNumber >> 35);
	randomNumber = randomNumber ^ (randomNumber << 4);
	if (randomNumber < 0)
		randomNumber = -randomNumber;

	//	printf("randomXOR :: number %d \n", randomNumber);
	//	fflush(NULL);
	return randomNumber;
}


