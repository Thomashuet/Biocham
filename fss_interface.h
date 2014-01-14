/*
 * fss_interfaces.h
 *
 *  Created on: 23/03/2012
 *      Author: anthonylins
 */

#ifndef FSS_INTERFACES_H_
#define FSS_INTERFACES_H_

#include "fss.h"

/* --- initialization, constructors, destructors --- */
/* initialization */
/* - fss attributes (position, weight, fitness) */
//TSchool * fss_allocationMemorySchool(int, int);

TFssData * fss_allocationMemoryData(int);

TFssParameters * fss_allocationMemoryParameters(int, int,
		double *, double *,int);

void fss_init_data(TFssData **, TFssParameters **, int); /*, double *);*/

void fss_init_positions(double **, double *, double *, random_t *, int, int);

void fss_init_steps(double **, double **, double **, double *, double *,
		random_t *, int, int);

void fss_init_weights(double **, double **, double, double, random_t *, int);

void fss_fitness(double(*pFun)(double const *, int), double **, double **,
		double **, int **, int **, TFssData **, TFssParameters **);

/* FSS operators */
int fss_individual_operator(double(*pFun)(double const *, int), double **,
		double **, double **, double **, double **, int **, int **, int **, double **,
		double **, double **, TFssData **, TFssParameters **);

void fss_feeding_operator(double **, double **, double **, double **,
		TFssParameters **);

int fss_instinctive_operator(double(*pFun)(double const *, int), double **,
		double **, double **, double **, double **, int **, int **, TFssData **data,
		TFssParameters **);

int fss_volitive_operator(double(*pFun)(double const *, int),
		double **, double **, double **,
		double **, double **, double **,
		int **, TFssData **, TFssParameters **);

void fss_update_steps(TFssData **, TFssParameters **);

void weightUpdateStrategiesS1(double **weight,TFssParameters **);

void weightUpdateStrategiesS2(double **, double **,
		TFssParameters **);

void stepSizeUpdateStrategiesS3(TFssData **, TFssParameters **, int );


void fss_exit(double **, double **, double **, double **, double **, double **,
		double **, double **, double **, double **, double **, int **, int **, int **,
		int **, TFssData **, TFssParameters **);

char* fss_SayHello(TFssData **, TFssParameters **);

//double* fss_UpdateDistribution(TSchool **, TFssData **, TFssParameters **, const double *);

const char* fss_TestForTermination(TFssData **, TFssParameters **);

double * fss_NewDouble(int n);
double ** fss_NewDouble2D(int n);



#endif /* FSS_INTERFACES_H_ */
