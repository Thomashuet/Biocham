void
biocham_mpi_begin_compute(void);

void
biocham_mpi_compute_fitness_value(unsigned int multi, unsigned int population_size, unsigned int dimension, double *const*pop, double *fitvals);

void
biocham_mpi_end_compute(void);

double
call_prolog_distance(unsigned int multi, unsigned int dimension, double const*values);
