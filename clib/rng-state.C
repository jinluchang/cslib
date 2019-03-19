#include "rng-state.h"

extern "C" {

size_t clib_rng_state_num_of_int32();

typedef uint32_t* ClibRngState;

void clib_set_rng_state_root(ClibRngState rng);

void clib_set_rng_state_seed_long(ClibRngState rng, const long sindex);

void clib_set_rng_state_seed_string(ClibRngState rng, const char* sindex);

void clib_set_rng_state_split_long(ClibRngState rng, const ClibRngState rng0,
                                   const long sindex);

void clib_set_rng_state_split_string(ClibRngState rng, const ClibRngState rng0,
                                     const char* sindex);

void clib_set_rng_state_type(ClibRngState rng, const unsigned long type);

uint64_t clib_rand_gen(ClibRngState rng);

double clib_u_rand_gen(ClibRngState rng, const double upper,
                       const double lower);

double clib_g_rand_gen(ClibRngState rng, const double center,
                       const double sigma);

void clib_set_global_rng_state(const ClibRngState rng);

void clib_get_global_rng_state(ClibRngState rng);

uint64_t clib_rand_gen_g();

double clib_u_rand_gen_g(const double upper, const double lower);

double clib_g_rand_gen_g(const double center, const double sigma);
}

size_t clib_rng_state_num_of_int32() { return RNG_STATE_NUM_OF_INT32; }

void clib_set_rng_state_root(ClibRngState rng)
{
  RngState rs;
  exportRngState(rng, rs);
}

void clib_set_rng_state_seed_long(ClibRngState rng, const long sindex)
{
  RngState rs(sindex);
  exportRngState(rng, rs);
}

void clib_set_rng_state_seed_string(ClibRngState rng, const char* sindex)
{
  RngState rs(sindex);
  exportRngState(rng, rs);
}

void clib_set_rng_state_split_long(ClibRngState rng, const ClibRngState rng0,
                                   const long sindex)
{
  RngState rs;
  importRngState(rs, rng0);
  rs = rs.split(sindex);
  exportRngState(rng, rs);
}

void clib_set_rng_state_split_string(ClibRngState rng, const ClibRngState rng0,
                                     const char* sindex)
{
  RngState rs;
  importRngState(rs, rng0);
  rs = rs.split(sindex);
  exportRngState(rng, rs);
}

void clib_set_rng_state_type(ClibRngState rng, const unsigned long type)
{
  RngState rs;
  importRngState(rs, rng);
  set_type(rs, type);
  exportRngState(rng, rs);
}

uint64_t clib_rand_gen(ClibRngState rng)
{
  RngState rs;
  importRngState(rs, rng);
  uint64_t ret = rand_gen(rs);
  exportRngState(rng, rs);
  return ret;
}

double clib_u_rand_gen(ClibRngState rng, const double upper, const double lower)
{
  RngState rs;
  importRngState(rs, rng);
  double ret = u_rand_gen(rs, upper, lower);
  exportRngState(rng, rs);
  return ret;
}

double clib_g_rand_gen(ClibRngState rng, const double center,
                       const double sigma)
{
  RngState rs;
  importRngState(rs, rng);
  double ret = g_rand_gen(rs, center, sigma);
  exportRngState(rng, rs);
  return ret;
}

void clib_set_global_rng_state(const ClibRngState rng)
{
  RngState& rs = get_global_rng_state();
  importRngState(rs, rng);
}

void clib_get_global_rng_state(ClibRngState rng)
{
  const RngState& rs = get_global_rng_state();
  exportRngState(rng, rs);
}

uint64_t clib_rand_gen_g() { return rand_gen(get_global_rng_state()); }

double clib_u_rand_gen_g(const double upper, const double lower)
{
  return u_rand_gen(get_global_rng_state(), upper, lower);
}

double clib_g_rand_gen_g(const double center, const double sigma)
{
  return g_rand_gen(get_global_rng_state(), center, sigma);
}
