void
error(char *message);

int
get_prolog_atom(char *s);

void
call_prolog(int functor, int arity, PlTerm *arg);

double
call_prolog_distance(unsigned int multi, unsigned int dimension, double const*values);



