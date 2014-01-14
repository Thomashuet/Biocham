#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#if defined(USE_MPI)
#include <mpi.h>
#endif
#include <gprolog.h>
#include "cmaes_biocham.h"

enum messages {
  MSG_INITIALIZE,
  MSG_READY,
  MSG_COMPUTE,
  MSG_COMPUTE_INDEX,
  MSG_COMPUTE_MULTI,
  MSG_COMPUTE_DIMENSION,
  MSG_COMPUTE_VALUES,
  MSG_COMPUTED_INDEX,
  MSG_COMPUTED_VALUE,
  MSG_CALL_LENGTH,
  MSG_CALL
};

enum initialize {
  INIT_END,
  INIT_COMPUTE,
  INIT_CALL
};

enum ready {
  READY_WITHOUT_RESULT,
  READY_WITH_RESULT
};

enum compute {
  COMPUTE_END,
  COMPUTE_NOW,
  COMPUTE_WAIT
};

#if defined(USE_MPI)
static int
slave_available(void)
{
  int numprocs;
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  return numprocs >= 2;
}

static unsigned int
receive_unsigned_int(int source, int tag)
{
  MPI_Status status;
  unsigned int result;
  MPI_Recv(&result, 1, MPI_UNSIGNED, source, tag, MPI_COMM_WORLD, &status);
  return result;
}

static double
receive_double(int source, int tag)
{
  MPI_Status status;
  double result;
  MPI_Recv(&result, 1, MPI_DOUBLE, source, tag, MPI_COMM_WORLD, &status);
  return result;
}

static void
send_unsigned_int(unsigned int value, int target, int tag) {
  MPI_Send(&value, 1, MPI_UNSIGNED, target, tag, MPI_COMM_WORLD);
}

static void
send_double(double value, int target, int tag) {
  MPI_Send(&value, 1, MPI_DOUBLE, target, tag, MPI_COMM_WORLD);
}

static void
biocham_mpi_send_initialize(unsigned int rank, unsigned int initialize_code)
{
  MPI_Send(&initialize_code, 1, MPI_UNSIGNED, rank, MSG_INITIALIZE, MPI_COMM_WORLD);
}

static void
biocham_mpi_broadcast_initialize(unsigned int initialize_code)
{
  int numprocs;
  unsigned int i;
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  for (i = 1; i < numprocs; i++) {
    biocham_mpi_send_initialize(i, initialize_code);
  }
}

static void
biocham_mpi_terminate_slaves(void)
{
  biocham_mpi_broadcast_initialize(INIT_END);
  MPI_Finalize();
}
#endif

void
biocham_mpi_begin_compute(void)
{
  #if defined(USE_MPI)
    biocham_mpi_broadcast_initialize(INIT_COMPUTE);
  #endif
}

#if defined(USE_MPI)
  static
  void biocham_mpi_send_goal(unsigned int rank, char *goal)
  {
    size_t length = strlen(goal);
    biocham_mpi_send_initialize(rank, INIT_CALL);
    MPI_Send(&length, 1, MPI_UNSIGNED, rank, MSG_CALL_LENGTH, MPI_COMM_WORLD);
    MPI_Send(goal, length, MPI_CHAR, rank, MSG_CALL, MPI_COMM_WORLD);
  }

  static
  void biocham_mpi_broadcast_goal(char *goal)
  {
    int numprocs;
    unsigned int i;
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
    for (i = 1; i < numprocs; i++) {
      biocham_mpi_send_goal(i, goal);
    }
  }
#endif

Bool 
slave_call_c(char *goal)
{
  #if defined(USE_MPI)
    biocham_mpi_broadcast_goal(goal);
    return PL_TRUE;
  #endif
}

#if defined(USE_MPI)
  static int
  wait_slave(int rank, double *fitvals)
  {
    MPI_Status status;
    unsigned int waiting;
    int source;
    MPI_Recv(&waiting, 1, MPI_UNSIGNED, rank, MSG_READY, MPI_COMM_WORLD, &status);
    source = status.MPI_SOURCE;
    if (waiting == READY_WITH_RESULT) {
      if (fitvals == NULL) {
        error("Pending result");
      }
      unsigned int index = receive_unsigned_int(source, MSG_COMPUTED_INDEX);
      double value = receive_double(source, MSG_COMPUTED_VALUE);
      fitvals[index] = value;
    }
    return source;
  }
  
  static void
  wait_all_slaves(double *fitvals, enum compute compute)
  {
    unsigned int i;
    int numprocs;
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
    for (i = 1; i < numprocs; i++) {
      int source = wait_slave(i, fitvals);
      send_unsigned_int(compute, source, MSG_COMPUTE);
    }
  }
#endif

void
biocham_mpi_compute_fitness_value(unsigned int multi, unsigned int population_size, unsigned int dimension, double *const*values, double *fitvals)
{
  unsigned int i;
  #if defined(USE_MPI)
    if (slave_available()) {
      for (i = 0; i < population_size; i++) {
        int source;
        source = wait_slave(MPI_ANY_SOURCE, fitvals);
        send_unsigned_int(COMPUTE_NOW, source, MSG_COMPUTE);
        send_unsigned_int(i, source, MSG_COMPUTE_INDEX);
        send_unsigned_int(multi, source, MSG_COMPUTE_MULTI);
        send_unsigned_int(dimension, source, MSG_COMPUTE_DIMENSION);
        MPI_Send(values[i], dimension, MPI_DOUBLE, source, MSG_COMPUTE_VALUES, MPI_COMM_WORLD);
      }
      wait_all_slaves(fitvals, COMPUTE_WAIT);
    }
    else {
  #endif
      for (i = 0; i < population_size; i++) {
        fitvals[i] = call_prolog_distance(multi, dimension, values[i]);
      }
  #if defined(USE_MPI)
    }
  #endif
}

void
biocham_mpi_end_compute(void)
{
  #if defined(USE_MPI)
    wait_all_slaves(NULL, COMPUTE_END);
  #endif
}

#if defined(USE_MPI)
  static int
  Main_Wrapper(int argc, char *argv[])
  {
    int nb_user_directive;
    PlBool top_level;
  
    atexit(biocham_mpi_terminate_slaves);
  
    nb_user_directive = Pl_Start_Prolog(argc, argv);
  
    top_level = Pl_Try_Execute_Top_Level();
  
    Pl_Stop_Prolog();
  
    if (top_level || nb_user_directive)
      return 0;
  
    fprintf(stderr,
            "Warning: no initial goal executed\n"
            "   use a directive :- initialization(Goal)\n"
            "   or remove the link option --no-top-level"
            " (or --min-bips or --min-size)\n");
  
    return 1;
  }
  
  static void
  worker_compute()
  {
    unsigned int compute_code;
    send_unsigned_int(READY_WITHOUT_RESULT, 0, MSG_READY);
    while ((compute_code = receive_unsigned_int(0, MSG_COMPUTE)) != COMPUTE_END) {
      if (compute_code == COMPUTE_NOW) {
        unsigned int index = receive_unsigned_int(0, MSG_COMPUTE_INDEX);
        unsigned int multi = receive_unsigned_int(0, MSG_COMPUTE_MULTI);
        unsigned int dimension = receive_unsigned_int(0, MSG_COMPUTE_DIMENSION);
        double values[dimension];
        MPI_Status status;
        double result;
        MPI_Recv(values, dimension, MPI_DOUBLE, 0, MSG_COMPUTE_VALUES, MPI_COMM_WORLD, &status);
        result = call_prolog_distance(multi, dimension, values);
        send_unsigned_int(READY_WITH_RESULT, 0, MSG_READY);
        send_unsigned_int(index, 0, MSG_COMPUTED_INDEX);
        send_double(result, 0, MSG_COMPUTED_VALUE);
      }
      else {
        send_unsigned_int(READY_WITHOUT_RESULT, 0, MSG_READY);
      }
    }
  }

  static void
  prolog_call(const char *goal) {
    PlTerm goal_term = Pl_Mk_Atom(Pl_Create_Allocate_Atom(goal));
    int client_call = Pl_Find_Atom("client_call");
    if (client_call == -1) {
      error("Cannot find client_call");
    }
    call_prolog(client_call, 1, &goal_term);
  }
  
  static void
  worker_call()
  {
    size_t length = receive_unsigned_int(0, MSG_CALL_LENGTH);
    char string[length + 1];
    MPI_Status status;
    MPI_Recv(string, length, MPI_CHAR, 0, MSG_CALL, MPI_COMM_WORLD, &status);
    string[length] = '\0';
    prolog_call(string);
  }
  
  static void
  worker_loop()
  {
    char *argv[2];
    char *arg_biocham = "biocham_job";
    char *arg_slave = "--slave";
    unsigned int init_code;
    argv[0] = arg_biocham;
    argv[1] = arg_slave;
    Pl_Start_Prolog(2, argv);
    while ((init_code = receive_unsigned_int(0, MSG_INITIALIZE)) != INIT_END) {
      switch (init_code) {
      case INIT_COMPUTE:
        worker_compute();
        break;
      case INIT_CALL:
        worker_call();
        break;
      default:
        error("Unknown initialization code");
      }
    }
    Pl_Stop_Prolog();
  }
  
  int
  main(int argc, char *argv[])
  {
    int rank;
    int numprocs;
    int namelen;
    char processor_name[BUFSIZ];
  
    if (MPI_Init(&argc, &argv) != MPI_SUCCESS) {
      error("Cannot initialize MPI");
    }
  
    if (MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_ARE_FATAL) != MPI_SUCCESS) {
      error("Cannot set error handler");
    }
  
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  
    MPI_Get_processor_name(processor_name, &namelen);
  
    printf("Process %d on %s out of %d\n", rank, processor_name, numprocs);
  
    if (rank == 0) {
      Main_Wrapper(argc, argv);
    }
    else {
      worker_loop();
      MPI_Finalize();
    }
    return 0;
  }
#endif
