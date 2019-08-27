extern "C" {

void clib_set_omp_num_threads(const int n_threads);

}

#include <omp.h>

void clib_set_omp_num_threads(const int n_threads)
{
  omp_set_num_threads(n_threads);
}
