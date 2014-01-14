/*
 * fss.h
 *
 *  Created on: 22/03/2012
 *      Author: anthonylins
 */

#ifndef FSS_H_
#define FSS_H_

#include <time.h>


#define TRUE 1
#define FALSE 0

/* random_t
 * sets up a pseudo random number generator instance
 */
typedef struct
{
  /* Variables for Uniform() */
  long int startseed;
  long int aktseed;
  long int aktrand;
  long int *rgrand;

  /* Variables for Gauss() */
  short flgstored;
  double hold;
} random_t;



//typedef struct SFish {

//} TFish;

/*
typedef struct SSchool {
	TFish *fish;
//	SSchool *next;
} TSchool;
*/

typedef struct SData{
	random_t *rand; /* random number generator */

	double *schoolWeight; //total weight of the fish school
	double *gbestFitness; //global best fitness value
	int *gbest; // fish number with the GBest value
	int *cycleGbest; //cycle number which GBest was reached
	double *stepInd; /* step value for the individual operator. */
	double *stepVol; /* step value for the vollitive operator */

	double *barycentre;

	char* version;
	char sOutString[330]; /* 4x80 */

} TFssData;

typedef struct SParameters{
	int maxNumberOfIterations;

	double weight_scale;
	double initialWeightMax;
	double initialWeightMin;

	int dimensions;
	int schoolSize;

	double *initialRangeLeft;
	double *initialRangeRight;
	double *minimumSearchSpace;
	double *maximumSearchSpace;

	double *stepIndividualInitial;
	double *stepIndividualFinal;
	double *stepVolitiveInitial;
	double *stepVolitiveFinal;

	double deltaLinearWeight; /* delta value for linear weight update - Janecek and Tan  - 1% number of iterations as initial suggestion */
	int janecek;

	int stagnancyMaxLimit; /* maximum limit for iterations with fitness stagnation = 10% number iterations as initial suggestion*/

	long seed;
	int countevals;

} TFssParameters;

typedef struct
/* timings_t
 * time measurement, used to time eigendecomposition
 */
{
  /* for outside use */
  double totaltime;
  double tictoctime;
  double lasttictoctime;

  /* local fields */
  clock_t lastclock;
  time_t lasttime;
  clock_t ticclock;
  time_t tictime;
  short istic;
  short isstarted;

  double lastdiff;
  double tictoczwischensumme;
} timings_t;


/* fish structure
 * Anthony Lins
 */
double *positions; // fish position vector
double *weight; // fish weight vector
double *fitness; // fish fitness value calculated by objective function

double *deltaX; //variation between the actual and the new position (individual operator)
double *oldWeight; // previous weight of the fish
double *deltaFitness; // fitness variation
double *deltaFitnessNormalized; // fitness variation normalized
double *fitnessNeighbor; // fitness value for a new position (neighbor)
int *individual_move_success;
int *instinctive_move_success;
int *volitive_move_success;

double *stepInd; /* actual step value on dimension for the individual operator. */
double *stepIndPrev; /* previous step value on dimension for the individual operator. */
double *stepIndAdaptive; /* adaptive step value for individual operator */

int *stagnancyCount; /* counter for iterations with fitness stagnation */
int *flagStagnancy;  /* flag to indicate that there was stagnation. */

int debug;

#endif /* FSS_H_ */
