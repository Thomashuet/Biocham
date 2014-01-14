#include <stdio.h>
#include <string.h> /* strncmp */
#include <math.h>
#include <stdlib.h>
#include <stdarg.h>

// __APPLE__ will also match on iOS, but we do not really care
#if defined(__unix__) || defined(__CYGWIN__) || defined(__APPLE__)
  #include <unistd.h>
  #include <sys/types.h>
  #include <sys/wait.h>
#else
  #include <windows.h>
  #include <winnt.h>
  #include <process.h>
  #include <io.h>
  #include <fcntl.h>
#endif

#include "cmaes_interface.h"
#include "gprolog.h"
#include "biocham_mpi.h"


void
error(char *message)
{
  fprintf(stderr, "Error: %s.\n", message);
  exit(1);
}

int
get_prolog_atom(char *s)
{
  int atom = Pl_Find_Atom(s);
  if (atom == -1) {
    error("Unknown atom");
  }
  return atom;
}

void
call_prolog(int functor, int arity, PlTerm *arg)
{
  Pl_Query_Begin(PL_FALSE);
  if (Pl_Query_Call(functor, arity, arg) != PL_SUCCESS) {
    error("Prolog error");
  }
  Pl_Query_End(PL_CUT);
}

double
call_prolog_distance(unsigned int multi, unsigned int dimension, double const*values)
{
  PlTerm prolog_values[dimension];
  unsigned int i;
  PlTerm cost;
  PlTerm args[2];
  double result;
  int functor;
  for (i = 0; i < dimension; i++){
    prolog_values[i] = Mk_Float(values[i]);
  }
  cost = Mk_Variable();
  args[0] = Mk_Proper_List(dimension, prolog_values);
  args[1] = cost;
  if (multi == 0) {
    functor = get_prolog_atom("distance_cmaes");
  }
  else {
    functor = get_prolog_atom("distance_cmaes_multi");
  }
  call_prolog(functor, 2, args);
  result = Rd_Float_Check(cost);
  return result;
}

/*___________________________________________________________________________
 *
 * Function Declarations
 *___________________________________________________________________________
*/
double **OrthogonalBasis(int DIM);
//double f_biocham( double const *x);
void set_random_params(int dim);
int is_feasible(double const *x);
int simul( double const *x, int dim);
//double f_biocham_multi( double const *x);


static double fCost=0;
//Dragana
//Bool getFCost(double *k);

double * optimize(int multi, int number_of_restarts, 
		  double increment_factor_for_population_size, 
		  char *input_parameter_filename, int dim, int maxevals_b, double stopfitness_b, int cseed, double std);

extern void   random_init( random_t *, long unsigned seed /*=0=clock*/);
extern void   random_exit( random_t *);
extern double random_Gauss( random_t *); /* (0,1)-normally distributed */

/*___________________________________________________________________________
//___________________________________________________________________________
//
// reads from file "initials.par" here and in cmaes_init()
//___________________________________________________________________________
*/ 


Bool 
call_main_cmaes(int dim, int multi, int maxevals_b, PlTerm pl_stopfitness_b, int cseed, double std)
{

  // [Sylvain] no read no write
  // char *filename = "initials.par"; /* input parameter file */
  char *filename = "non"; /* input parameter file */

//  int nb = 0, nbrestarts = 0;
  int nbrestarts = 0;
  double incpopsize = 2;
//  int maxnb;

  double stopfitness_b=Rd_Number_Check(pl_stopfitness_b);
  double *x;

  printf("dim: %d; multi: %d; maxevals_b: %d; stopfitness_b: %f; cseed: %d; std: %f\n", dim, multi, maxevals_b, stopfitness_b, cseed, std);

//   maxnb = 23; 

  // [Sylvain] no read no write
//   /* Read objective function number and number of restarts from file */
//   fp = fopen(filename, "r");
//   if (fp) {
//     fscanf(fp, " function number %d ", &nb); 
//     /* go to next line, a bit sloppy */ 
//     for (c = ' ', ret = 1; c != '\n' && c != '\0' && c != EOF && ret && ret != EOF; 
// 	 ret=fscanf(fp, "%c", &c))
//       ;
//     fscanf(fp, " restarts %d %lf", &nbrestarts, &incpopsize);
//     fclose(fp);
//     if (nb < 0 || nb > maxnb)
//       nb = 0;
//     if (nbrestarts < 0)
//       nbrestarts = 0;
//   } else
//     printf("main(): could not open %s to read function number", filename);
// 
  /* Optimize function */
  // [Sylvain] no read no write
  // nb=fun;

  nbrestarts = 10;
  incpopsize = 1;
  // printf("c1\n");
  x = optimize(multi, nbrestarts, incpopsize, filename, dim, maxevals_b,  stopfitness_b, cseed, std);

  /* here we could utilize the solution x, and finally free memory */

  simul(x,dim);

  free(x); 

  return PL_TRUE;

} /* main() */

double atod(const char *s) {
  double result;
  if (sscanf(s, "%lf", &result) != 1) {
    perror("sscanf@atod");
    exit(EXIT_FAILURE);
  }
  return result;
}

int debug;

void debug_printf(int level, const char *format, ...)
{
  va_list args;
  if (debug >= level) {
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
  }
}


//int main(int argc, const char *argv[]) {
//  debug = atoi(argv[1]);
//  int dim = atoi(argv[2]);
//  int fun = atoi(argv[3]);
//  int maxevals_b = atoi(argv[4]);
//  double stopfitness_b = atod(argv[5]);
//  int cseed = atoi(argv[6]);
//  double std = atod(argv[7]);
//
//  typedef double (*pfun_t)(double const *); 
//  pfun_t rgpFun[99];  /* array (range) of pointer to objective function */
//  // [Sylvain] no read no write
//  // char *filename = "initials.par"; /* input parameter file */
//  char *filename = "non";  /* input parameter file */
//
//  int nb = 0, nbrestarts = 0;
//  double incpopsize = 2;
//  int maxnb;
//
//  double *x;
//
//
//  /* Put together objective functions */
//
//  rgpFun[0] = f_biocham;      
// rgpFun[1] = f_biocham_multi;    
//   maxnb = 23; 
//
//  // [Sylvain] no read no write
////   /* Read objective function number and number of restarts from file */
////   fp = fopen(filename, "r");
////   if (fp) {
////     fscanf(fp, " function number %d ", &nb); 
////     /* go to next line, a bit sloppy */ 
////     for (c = ' ', ret = 1; c != '\n' && c != '\0' && c != EOF && ret && ret != EOF; 
//// 	 ret=fscanf(fp, "%c", &c))
////       ;
////     fscanf(fp, " restarts %d %lf", &nbrestarts, &incpopsize);
////     fclose(fp);
////     if (nb < 0 || nb > maxnb)
////       nb = 0;
////     if (nbrestarts < 0)
////       nbrestarts = 0;
////   } else
////     printf("main(): could not open %s to read function number", filename);
//// 
//  /* Optimize function */
//  // [Sylvain] no read no write
//  // nb=fun;
//
//  nb = fun;
//  nbrestarts = 10;
//  incpopsize = 1;
//  // printf("c1\n");
//  x = optimize(rgpFun[nb], nbrestarts, incpopsize, filename, dim, maxevals_b,  stopfitness_b, cseed, std);
//
//  /* here we could utilize the solution x, and finally free memory */
//
//  simul(x,dim);
//
//  free(x); 
//
//  return EXIT_SUCCESS;
//}


//Dragana
Bool getFCost(double *k){
   *k=fCost;
   return PL_TRUE;
}

/*___________________________________________________________________________
//___________________________________________________________________________
//
// Somewhat extended interface for optimizing pFun with cmaes_t
// implementing a restart procedure with increasing population size
//___________________________________________________________________________
*/ 

double * optimize(int multi, int nrestarts, double incpopsize, char * filename, int dim, int maxevals_b,  double stopfitness_b, int cseed, double std)
{
  cmaes_t evo;       /* the optimizer */
  double *const*pop; /* sampled population */
  double *fitvals;   /* objective function values of sampled population */
  double fbestever=0, *xbestever=NULL; /* store best solution */
  /*, *xbesteverdisp=NULL*/
  double fmean; 
  int i, irun,
    lambda = 0,      /* offspring population size, 0 invokes default */
    countevals = 0;  /* used to set for restarts */
  char const * stop; /* stop message */


  for (irun = 0; irun < nrestarts+1; ++irun) /* restarts */
    {

      /* Parameters can be set in three ways. Here as input parameter 
       * to cmaes_init, as value read from initials.par in readpara_init
       * during initialization, and as value read from signals.par by
       * calling cmaes_ReadSignals explicitely. 
       */
      // [Sylvain] no read no write
      // fitvals = cmaes_init(&evo, dim, NULL, NULL, 0, lambda, filename,
      double * typicalx = (double *) calloc((unsigned) dim, sizeof(double));
      double * stddev = (double *) calloc((unsigned) dim, sizeof(double));
      for (i=0; i<dim; ++i) {
	      typicalx[i] = 1;
         stddev[i] = std;
      }

      fitvals = cmaes_init(&evo, dim, typicalx, stddev, cseed, lambda, filename,
			    maxevals_b,  stopfitness_b); /* allocs fitvals */
      printf("%s\n", cmaes_SayHello(&evo));
      
      evo.countevals = countevals; /* a hack, effects the output and termination */
      // [Sylvain] no read no write
      // cmaes_ReadSignals(&evo, "signals.par"); /* write initial values, headers in case */

      biocham_mpi_begin_compute();
      
      if(irun>0)set_random_params(dim);
      while(!(stop=cmaes_TestForTermination(&evo)))
	{ 
	  // printf("c14\n");
	  /* Generate population of new candidate solutions */
	  pop = cmaes_SamplePopulation(&evo); /* do not change content of pop */
	  //printf("c15\n");
	  /* Here optionally handle constraints etc. on pop. You may
	   * call cmaes_ReSampleSingle(&evo, i) to resample the i-th
	   * vector pop[i], see below.  Do not change pop in any other
	   * way. You may also copy and modify (repair) pop[i] only
	   * for the evaluation of the fitness function and consider
	   * adding a penalty depending on the size of the
	   * modification.
	   */

          biocham_mpi_compute_fitness_value(multi, cmaes_Get(&evo, "popsize"), dim, pop, fitvals);
	  
//	  /* Compute fitness value for each candidate solution */
//	  for (i = 0; i < cmaes_Get(&evo, "popsize"); ++i) {
//	    /* You may resample the solution i until it lies within the
//	       feasible domain here, e.g. until it satisfies given  
//               box constraints (variable boundaries). The function 
//               is_feasible() needs to be user-defined.  
//	       Assumptions: the feasible domain is convex, the optimum
//	       is not on (or very close to) the domain boundary,
//	       initialX is feasible and initialStandardDeviations are
//	       sufficiently small to prevent quasi-infinite looping.
//	    */
//
//	    //while (!is_feasible(pop[i])) 
//	    // now done by correcting value 
//	    //and adding an error when outside bounds
//	    //   {cmaes_ReSampleSingle(&evo, i); 
//		 //printf("resample\n");
//	    //   }
//
//	    fitvals[i] = (*pFun)(pop[i]); 
//	  }


	  /* update search distribution */
	  cmaes_UpdateDistribution(&evo, fitvals); 
	  
	  /*disp best*/
	  /*xbesteverdisp = cmaes_GetInto(&evo, "xbestever", xbestever);
	    simul(xbesteverdisp);*/

	  /* read control signals for output and termination */
     // [Sylvain] no read no write
	  // cmaes_ReadSignals(&evo, "signals.par"); /* from file signals.par */
	  
	  fflush(stdout);
	} /* while !cmaes_TestForTermination(&evo) */


      biocham_mpi_end_compute();

      lambda = incpopsize * cmaes_Get(&evo, "lambda");   /* needed for the restart */
      countevals = cmaes_Get(&evo, "eval");     /* ditto */

      /* print some "final" output */
      printf("%.0f generations, %.0f fevals (%.1f sec): f(x)=%g\n", 
	     cmaes_Get(&evo, "gen"), cmaes_Get(&evo, "eval"), 
	     evo.eigenTimings.totaltime,
	     cmaes_Get(&evo, "funval"));
      printf("  (axis-ratio=%.2e, max/min-stddev=%.2e/%.2e)\n", 
	     cmaes_Get(&evo, "maxaxislen") / cmaes_Get(&evo, "minaxislen"),
	     cmaes_Get(&evo, "maxstddev"), cmaes_Get(&evo, "minstddev") 
	     );
      printf("Stop (run %d):\n%s\n",  irun+1, cmaes_TestForTermination(&evo));

      /* write some data */
      // [Sylvain] no read no write
      // cmaes_WriteToFile(&evo, "all", "allcmaes.dat");
      
      /* keep best ever solution */
      if (irun == 0 || cmaes_Get(&evo, "fbestever") < fbestever) {
	fbestever = cmaes_Get(&evo, "fbestever"); 
	xbestever = cmaes_GetInto(&evo, "xbestever", xbestever); /* alloc mem if needed */
      }
      /* best estimator for the optimum is xmean, therefore check */
      if ((fmean = call_prolog_distance(multi, dim, cmaes_GetPtr(&evo, "xmean"))) < fbestever) {
	fbestever = fmean;
	xbestever = cmaes_GetInto(&evo, "xmean", xbestever);
      }
	
      cmaes_exit(&evo); /* does not effect the content of stop string and xbestever */

      /* abandon restarts if target fitness value was achieved or MaxFunEvals reached */
      if (stop) /* as it can be NULL */ {
	if (strncmp(stop, "Fitness", 7) == 0 || strncmp(stop, "MaxFunEvals", 11) == 0)
	  break;
      }
      if (strncmp(stop, "Manual", 6) == 0) {
	printf("Press RETURN to start next run\n"); fflush(stdout);
	getchar();
      }
    } /* for restarts */

  printf("\nFinal cost : %g\n\n",fbestever);
  fCost=fbestever;

  

  

  return xbestever; /* was dynamically allocated, should be freed in the end */
}


/*___________________________________________________________________________
//___________________________________________________________________________
*/

/* ----------------------------------------------------------------------- */

//void write_to_string(char **cur, size_t *left_sz, const char *format, ...)
//{
//  va_list args;
//  int written_sz;
//  va_start(args, format);
//  written_sz = vsnprintf(*cur, *left_sz, format, args);
//  if (written_sz < 0) {
//    perror("vsnprintf@write_to_string");
//    exit(EXIT_FAILURE);
//  }
//  va_end(args);
//  *left_sz -= written_sz;
//  *cur += written_sz;
//}
//
//void write_x_to_string(char **cur, size_t *left_sz, const double *x, int dim)
//{
//  int i;
//  if (dim > 0) {
//    write_to_string(cur, left_sz, "%f", x[0]);
//    for (i = 1; i < dim; i++) {
//      write_to_string(cur, left_sz, ",%f", x[i]);
//    }
//  }
//}
//
//double read_float(int fd) {
//  FILE *f = fdopen(fd, "r");
//  double d;
//  if (fscanf(f, "%lf", &d) != 1) {
//    perror("fscanf@read_float");
//    exit(EXIT_FAILURE);
//  }
//  fclose(f);
//  return d;
//}
//
//int wait_status(pid_t pid) {
//  int status;  
//  #if defined(__unix__) || defined(__CYGWIN__) || defined(__APPLE__)
//    pid_t waited_pid = waitpid(pid, &status, 0);
//    if (waited_pid < 0) {
//      perror("waitpid@wait_status");
//      exit(EXIT_FAILURE);
//    }
//  #else
//    HANDLE phandle = OpenProcess(SYNCHRONIZE | PROCESS_QUERY_INFORMATION, 1, pid);
//    if (phandle == 0) {
//      perror("OpenProcess@wait_status");
//      exit(EXIT_FAILURE);
//    }
//    if (WaitForSingleObject(phandle, INFINITE) == WAIT_FAILED)  {
//      perror("WaitForSingleObject@wait_status");
//      exit(EXIT_FAILURE);
//    }
//    if (!GetExitCodeProcess(phandle, (LPDWORD) &status)) {
//      perror("GetExitCodeProcess@wait_status");
//      exit(EXIT_FAILURE);
//    }
//    if (!CloseHandle(phandle)) {
//      perror("CloseHandle@wait_status");
//      exit(EXIT_FAILURE);
//    }
//  #endif
//  return status;
//}
//
//void wait_success(pid_t pid) {
//  if (wait_status(pid) != 0) {
//    fprintf(stderr, "wait_success failure.\n");
//    exit(EXIT_FAILURE);
//  }
//}
//
//void call_biocham(const char *goal, pid_t *pid, int *outfd)
//{
//  debug_printf(1, "biocham --client %s\n", goal);
//  #if defined(__unix__ ) || defined(__CYGWIN__) || defined(__APPLE__)
//    int outpipe[2];
//    pid_t fork_result;
//    if (pipe(outpipe) != 0) {
//      perror("pipe@call_biocham");
//      exit(EXIT_FAILURE);
//    }
//    fork_result = fork();
//    if (fork_result < 0) {
//      perror("fork@call_biocham");
//      exit(EXIT_FAILURE);
//    }
//    if (fork_result == 0) {
//      dup2(outpipe[1], 1);
//      close(outpipe[0]);
//      execlp("biocham", "biocham", "--client", goal, NULL);
//      perror("execlp@call_biocham");
//      exit(EXIT_FAILURE);
//    }
//    *pid = fork_result;
//    if (close(outpipe[1]) != 0) {
//      perror("close@call_biocham");
//      exit(EXIT_FAILURE);
//    }
//    *outfd = outpipe[0];
//  #else
//    #define MAX_CMD_LENGTH 4096
//    int status;
//    SECURITY_ATTRIBUTES sa = { 0 };
//    STARTUPINFO si = { 0 };
//    PROCESS_INFORMATION pi = { 0 };
//    HANDLE pipe_out_r = NULL;
//    HANDLE pipe_out_w = NULL;
//    char cmd[MAX_CMD_LENGTH];
//    char *cmd_cur = cmd;
//    size_t left_sz = MAX_CMD_LENGTH;
//    int i, n, open_result;
//  
//    sa.nLength = sizeof(sa);
//    sa.bInheritHandle = TRUE;
//    sa.lpSecurityDescriptor = NULL;
//  
//    if (!CreatePipe(&pipe_out_r, &pipe_out_w, &sa, 0)) {
//      perror("CreatePipe@call_biocham");
//      exit(EXIT_FAILURE);
//    }
//  
//    si.cb = sizeof(si);
//    si.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
//    si.wShowWindow = SW_HIDE;
//    si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
//    si.hStdOutput = pipe_out_w;
//    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
//    write_to_string(&cmd_cur, &left_sz, "biocham --client \"%s\"", goal);
//    if (!CreateProcess(NULL, cmd, NULL, NULL, TRUE, DETACHED_PROCESS, NULL, NULL, &si, &pi)) {
//      perror("CreateProcess@call_biocham");
//      exit(EXIT_FAILURE);
//    }
//    if (!CloseHandle(pipe_out_w)) {
//      perror("CloseHandle@call_biocham");
//      exit(EXIT_FAILURE);
//    }
//    *pid = pi.dwProcessId;
//    open_result = _open_osfhandle(pipe_out_r, _O_TEXT);
//    if (open_result < 0) {
//      perror("_open_osfhandle@call_biocham");
//      exit(EXIT_FAILURE);
//    }
//    *outfd = open_result;
//  #endif
//}

//#define MAX_GOAL_LENGTH 4096
//
//double f_biocham_generic(const char *p, const double *x) {
//  int dim = (int)(x[-1]);
//  char goal[MAX_GOAL_LENGTH];
//  char *goal_cur = goal;
//  size_t left_sz = MAX_GOAL_LENGTH;
//  pid_t pid;
//  int outfd;
//  double dist;
//  write_to_string(&goal_cur, &left_sz, "%s([", p);
//  write_x_to_string(&goal_cur, &left_sz, x, dim);
//  write_to_string(&goal_cur, &left_sz, "],X),print(X).");
//  call_biocham(goal, &pid, &outfd);
//  dist = read_float(outfd);
//  wait_success(pid);
//  close(outfd);
//  return dist;
//}
//
//double f_biocham( double const *x)
//{
////  return f_biocham_generic("distance_cmaes", x);
//  
//  int i;
//  double dist = 0.;
//  int DIM = (int)(x[-1]);
//  PlTerm plist[DIM];
//  PlTerm cost;
//  PlTerm args[2];
//  int result;
//
//
//  Pl_Query_Begin(TRUE);
//  
//  /*make vars */
//  
//   
//  cost=Mk_Variable();
//  for (i = 0; i < DIM; ++i){
//    plist[i] = Mk_Float(x[i]);
//  }
//  args[0]=Mk_Proper_List(DIM,plist);
//  args[1]=cost;
//
//  result=Pl_Query_Call(Find_Atom("distance_cmaes"),2,args);
//
//  dist=Rd_Float_Check(args[1]);
//
//   Pl_Query_End(PL_RECOVER);
//
//   
//  return dist;
//  
//}


int simul( double const *x, int DIM)
{
  /*
  char goal[MAX_GOAL_LENGTH];
  char *goal_cur = goal;
  size_t left_sz = MAX_GOAL_LENGTH;
  pid_t pid;
  int outfd;
  write_to_string(&goal_cur, &left_sz, "simul_cmaes([");
  write_x_to_string(&goal_cur, &left_sz, x, DIM);
  write_to_string(&goal_cur, &left_sz, "]).");
  call_biocham(goal, &pid, &outfd);
  wait_success(pid);
  close(outfd);
  return 1;
  */
  
  int i;
  PlTerm plist[DIM];
  PlTerm args[1];

  //    printf("\nsimul \n");
  
  /*make vars */
  
   
  for (i = 0; i < DIM; ++i){
    plist[i] = Mk_Float(x[i]);
  }
  args[0]=Mk_Proper_List(DIM,plist);
 

  call_prolog(Find_Atom("simul_cmaes"), 1, args);

   
  return 1;
}




/* ----------------------------------------------------------------------- */
/* a hack, memory is never released */
double **OrthogonalBasis(int DIM) {
  static int b_dim;
  static double **b;
  double sp; 
  int i,j,k;
  random_t R; 

  if(b_dim != 0) { /* Initialization was done */

    if (b_dim != DIM) {
      printf("function OrthogonalBasis cannot change dimensionality in file example2.c");
    
      exit(0);
    }

    

    return b;
  }

  /* Otherwise initialize basis b */
  random_init(&R, 2); /* TODO: choose not always the same basis? */
  
  /* allocate b */
  b = (double **) calloc((unsigned) DIM, sizeof(double*));
  if (!b) {
    printf("calloc failed in function OrthogonalBasis in file example2.c");
    
    exit(0);
  }
  for (i = 0; i < DIM; ++i) {
    b[i] = (double *) calloc((unsigned) DIM, sizeof(double));
    if (!b[i]) {
      printf("calloc failed in function Orthogonalbasis in file example2.c");
     
      exit(0);
    }
  }
  b_dim = DIM;
  
  /* generate orthogonal basis */
  for (i = 0; i < DIM; ++i) {
    /* sample components gaussian */
    for (j = 0; j < DIM; ++j) 
      b[i][j] = random_Gauss(&R);
    /* substract projection of previous vectors */
    for (j = i-1; j >= 0; --j) {
      for (sp = 0., k = 0; k < DIM; ++k)
	sp += b[i][k]*b[j][k]; /* scalar product */
      for (k = 0; k < DIM; ++k)
	b[i][k] -= sp * b[j][k]; /* substract */
    }
    /* normalize */
    for (sp = 0., k = 0; k < DIM; ++k)
      sp += b[i][k]*b[i][k]; /* squared norm */
    for (k = 0; k < DIM; ++k)
      b[i][k] /= sqrt(sp); 
  }
  random_exit(&R); 
  
 
  return b;

} /* OrthogonalBasis(int DIM) */



int is_feasible(double const *x)
{
//  int dim = (int)(x[-1]);
//  char goal[MAX_GOAL_LENGTH];
//  int i;
//  for (i = 0; i < dim; i++) {
//    char *goal_cur = goal;
//    size_t left_sz =  MAX_GOAL_LENGTH;
//    pid_t pid;
//    int outfd;
//    write_to_string(&goal_cur, &left_sz, "bounds(%f,Min,Max),Min=<%f,%f=<Max", x[i]);
//    call_biocham(goal, &pid, &outfd);
//    close(outfd);
//    if (wait_status(pid) != 0) {
//      return 0;
//    }
//  }
//  return 1;
  
  int i;
  int res=1;
  double dmin,dmax;
  PlTerm min,max;
  PlTerm args[3];


  int DIM = (int)(x[-1]);
  for( i=0; i<DIM; i++){
    
    min=Mk_Variable();
    max=Mk_Variable();
    args[0]=Mk_Integer(i);
    args[1]=min;
    args[2]=max;
    call_prolog(Find_Atom("bounds"), 3, args);
    dmin=Rd_Float_Check(args[1]);
    dmax=Rd_Float_Check(args[2]);
    //printf("dmin : %f dmax : %f xi : %f\n",dmin,dmax,x[i]);
    if ((x[i]<dmin) ||(x[i]>dmax))
      res=0 ;
    
  }
  return res;
  
}



///* ----------------------------------------------------------------------- */
//double f_biocham_multi( double const *x)
//{
////  return f_biocham_generic("distance_cmaes_multi", x);
//  
//  int i;
//  double dist = 0.;
//  int DIM = (int)(x[-1]);
//  PlTerm plist[DIM];
//  PlTerm cost;
//  PlTerm args[2];
//  int result;
//
//
//
//  Pl_Query_Begin(TRUE);
//  
//  /*make vars */
//
//   
//  cost=Mk_Variable();
//  for (i = 0; i < DIM; ++i){
//    plist[i] = Mk_Float(x[i]);
//  }
//  args[0]=Mk_Proper_List(DIM,plist);
//  args[1]=cost;
//
//  result=Pl_Query_Call(Find_Atom("distance_cmaes_multi"),2,args);
//
//  dist=Rd_Float_Check(args[1]);
//
//   Pl_Query_End(PL_RECOVER);
//
//   // printf("cdone\n");
//  return dist;
//  
//}




void set_random_params(int dim){
//  char goal[MAX_GOAL_LENGTH];
//  char *goal_cur = goal;
//  size_t left_sz = MAX_GOAL_LENGTH;
//  pid_t pid;
//  int outfd;
//  write_to_string(&goal_cur, &left_sz, "cmaes_restart(%d).", dim);
//  call_biocham(goal, &pid, &outfd);
//  wait_success(pid);
//  close(outfd);
  PlTerm args[1];

  args[0]=Mk_Integer(dim);
  
  call_prolog(Find_Atom("cmaes_restart"), 1, args);

}
/*
  05/10/05: revised buggy comment on handling constraints by resampling 
*/
