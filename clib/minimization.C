#include <gsl/gsl_multimin.h>

#include <vector>
#include <cassert>

extern "C" {

  void clib_minimization_test();

  typedef double (*ClibGslMinimizationFunction)(const int, const double*);

  size_t clib_gsl_mult_minimization_nmsimplex2(
      const int n_params,
      const ClibGslMinimizationFunction f,
      double* double_inputs,
      const size_t max_iter);

}

double clib_gsl_minimization_function(const gsl_vector* v, void* params)
{
  ClibGslMinimizationFunction f = (ClibGslMinimizationFunction)params;
  const int size = v->size;
  std::vector<double> vec(size, 0.0);
  for (int i = 0; i < size; ++i) {
    vec[i] = gsl_vector_get(v, i);
  }
  return f(size, vec.data());
}

size_t clib_gsl_mult_minimization_nmsimplex2(
    const int n_params,
    const ClibGslMinimizationFunction f,
    double* double_inputs,
    const size_t max_iter)
{
  double* params = double_inputs;
  const double* step_sizes = double_inputs + n_params;
  double* p_epsabs = double_inputs + 2 * n_params;
  double* fvalue = double_inputs + 2 * n_params + 1;
  gsl_vector *x, *ss;
  x = gsl_vector_alloc(n_params);
  ss = gsl_vector_alloc(n_params);
  for (int i = 0; i < n_params; ++i) {
    /* Starting point */
    gsl_vector_set(x, i, params[i]);
    /* Set initial step sizes to 1 */
    gsl_vector_set(ss, i, step_sizes[i]);
  }
  /* Initialize method and iterate */
  const gsl_multimin_fminimizer_type* T = gsl_multimin_fminimizer_nmsimplex2;
  gsl_multimin_fminimizer *s = gsl_multimin_fminimizer_alloc(T, n_params);
  gsl_multimin_function minex_func;
  minex_func.n = n_params;
  minex_func.f = clib_gsl_minimization_function;
  minex_func.params = (void*)f;
  gsl_multimin_fminimizer_set(s, &minex_func, x, ss);
  size_t iter;
  int status;
  double size;
  for (iter = 0; iter < max_iter; ++iter) {
    status = gsl_multimin_fminimizer_iterate(s);
    if (status) {
      break;
    }
    size = gsl_multimin_fminimizer_size(s);
    status = gsl_multimin_test_size(size, *p_epsabs);
    if (status == GSL_SUCCESS) {
      break;
    }
  }
  for (int i = 0; i < n_params; ++i) {
    params[i] = gsl_vector_get(s->x, i);
  }
  *fvalue = s->fval;
  *p_epsabs = size;
  gsl_vector_free(x);
  gsl_vector_free(ss);
  gsl_multimin_fminimizer_free(s);
  return iter;
}

double my_f(const gsl_vector *v, void *params)
{
  double x, y;
  double *p = (double *)params;
  
  x = gsl_vector_get(v, 0);
  y = gsl_vector_get(v, 1);
 
  return p[2] * (x - p[0]) * (x - p[0]) +
           p[3] * (y - p[1]) * (y - p[1]) + p[4]; 
}

void clib_minimization_test()
{
  double par[5] = {1.0, 2.0, 10.0, 20.0, 30.0};

  const gsl_multimin_fminimizer_type *T = 
    gsl_multimin_fminimizer_nmsimplex2;
  gsl_multimin_fminimizer *s = NULL;
  gsl_vector *ss, *x;
  gsl_multimin_function minex_func;

  size_t iter = 0;
  int status;
  double size;

  /* Starting point */
  x = gsl_vector_alloc (2);
  gsl_vector_set (x, 0, 5.0);
  gsl_vector_set (x, 1, 7.0);

  /* Set initial step sizes to 1 */
  ss = gsl_vector_alloc (2);
  gsl_vector_set_all (ss, 1.0);

  /* Initialize method and iterate */
  minex_func.n = 2;
  minex_func.f = my_f;
  minex_func.params = par;

  s = gsl_multimin_fminimizer_alloc (T, 2);
  gsl_multimin_fminimizer_set (s, &minex_func, x, ss);

  do
    {
      iter++;
      status = gsl_multimin_fminimizer_iterate(s);
      
      if (status) 
        break;

      size = gsl_multimin_fminimizer_size (s);
      status = gsl_multimin_test_size (size, 1e-2);

      if (status == GSL_SUCCESS)
        {
          printf ("converged to minimum at\n");
        }

      printf ("%5d %10.3e %10.3e f() = %7.3f size = %.3f\n", 
              (int)iter,
              gsl_vector_get (s->x, 0), 
              gsl_vector_get (s->x, 1), 
              s->fval, size);
    }
  while (status == GSL_CONTINUE && iter < 100);
  
  gsl_vector_free(x);
  gsl_vector_free(ss);
  gsl_multimin_fminimizer_free (s);
}
