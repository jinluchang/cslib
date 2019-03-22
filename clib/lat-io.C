extern "C" {

typedef void* ClibLatData;

ClibLatData clib_lat_data_new();

void clib_lat_data_delete(ClibLatData cld);

void clib_lat_data_set_zero(ClibLatData cld);

void clib_lat_data_copy(ClibLatData cld, ClibLatData cld1);

void clib_lat_data_load(ClibLatData cld, const char* path);

void clib_lat_data_save(ClibLatData cld, const char* path);

long clib_lat_data_size(ClibLatData cld, const int level);

void clib_lat_data_print(ClibLatData cld);

int clib_lat_data_is_complex(ClibLatData cld);

int clib_lat_data_ndim(ClibLatData cld);

const char* clib_lat_data_dim_name(ClibLatData cld, const int dim);

long clib_lat_data_dim_size(ClibLatData cld, const int dim);

void clib_lat_data_add_dim(ClibLatData cld, const char* name, const long size);

double clib_lat_data_get(ClibLatData cld, const long* idxs, const int ndim);

double clib_lat_data_get_im(ClibLatData cld, const long* idxs, const int ndim);

void clib_lat_data_set(ClibLatData cld, const long* idxs, const int ndim,
                       const double val);

void clib_lat_data_set_im(ClibLatData cld, const long* idxs, const int ndim,
                          const double val);

void clib_lat_data_plus(ClibLatData cld, ClibLatData cld1, ClibLatData cld2);

void clib_lat_data_minus(ClibLatData cld, ClibLatData cld1, ClibLatData cld2);

void clib_lat_data_scale(ClibLatData cld, ClibLatData cld1,
                         const double factor);
}

#include "lat-io.h"

ClibLatData clib_lat_data_new()
{
  LatData* pld = new LatData;
  return (ClibLatData)pld;
}

void clib_lat_data_delete(ClibLatData cld)
{
  LatData& ld = *((LatData*)cld);
  delete &ld;
}

void clib_lat_data_set_zero(ClibLatData cld)
{
  LatData& ld = *((LatData*)cld);
  set_zero(ld);
}

void clib_lat_data_copy(ClibLatData cld, ClibLatData cld1)
{
  LatData& ld = *((LatData*)cld);
  const LatData& ld1 = *(LatData*)cld1;
  ld = ld1;
}

void clib_lat_data_load(ClibLatData cld, const char* path)
{
  LatData& ld = *((LatData*)cld);
  ld.load(path);
}

void clib_lat_data_save(ClibLatData cld, const char* path)
{
  LatData& ld = *((LatData*)cld);
  ld.save(path);
}

long clib_lat_data_size(ClibLatData cld, const int level)
  // return number of double
{
  const LatData& ld = *((LatData*)cld);
  return lat_data_size(ld.info, level);
}

void clib_lat_data_print(ClibLatData cld)
{
  const LatData& ld = *((LatData*)cld);
  print(ld);
}

int clib_lat_data_is_complex(ClibLatData cld)
{
  const LatData& ld = *((LatData*)cld);
  return is_lat_info_complex(ld.info);
}

int clib_lat_data_ndim(ClibLatData cld)
{
  const LatData& ld = *((LatData*)cld);
  if (is_lat_info_complex(ld.info)) {
    return ld.info.size() - 1;
  } else {
    return ld.info.size();
  }
}

const char* clib_lat_data_dim_name(ClibLatData cld, const int dim)
{
  const LatData& ld = *((LatData*)cld);
  assert(0 <= dim and dim < (int)ld.info.size());
  return ld.info[dim].name.c_str();
}

long clib_lat_data_dim_size(ClibLatData cld, const int dim)
{
  const LatData& ld = *((LatData*)cld);
  assert(0 <= dim and dim < (int)ld.info.size());
  return ld.info[dim].size;
}

void clib_lat_data_add_dim(ClibLatData cld, const char* name, const long size)
{
  LatData& ld = *((LatData*)cld);
  LatDim dim;
  dim.name = std::string(name);
  dim.size = size;
  ld.info.push_back(dim);
  lat_data_alloc(ld);
  set_zero(ld);
}

double clib_lat_data_get(ClibLatData cld, const long* idxs, const int ndim)
{
  const LatData& ld = *((LatData*)cld);
  const Vector<long> idx_v(idxs, ndim);
  return lat_data_get_const(ld, idx_v)[0];
}

double clib_lat_data_get_im(ClibLatData cld, const long* idxs, const int ndim)
{
  const LatData& ld = *((LatData*)cld);
  const Vector<long> idx_v(idxs, ndim);
  return lat_data_get_const(ld, idx_v)[1];
}

void clib_lat_data_set(ClibLatData cld, const long* idxs, const int ndim,
                       const double val)
{
  LatData& ld = *((LatData*)cld);
  const Vector<long> idx_v(idxs, ndim);
  lat_data_get(ld, idx_v)[0] = val;
}

void clib_lat_data_set_im(ClibLatData cld, const long* idxs, const int ndim,
                          const double val)
{
  LatData& ld = *((LatData*)cld);
  const Vector<long> idx_v(idxs, ndim);
  lat_data_get(ld, idx_v)[1] = val;
}

void clib_lat_data_plus(ClibLatData cld, ClibLatData cld1, ClibLatData cld2)
{
  LatData& ld = *((LatData*)cld);
  const LatData& ld1 = *(LatData*)cld1;
  const LatData& ld2 = *(LatData*)cld2;
  ld = ld1 + ld2;
}

void clib_lat_data_minus(ClibLatData cld, ClibLatData cld1, ClibLatData cld2)
{
  LatData& ld = *((LatData*)cld);
  const LatData& ld1 = *(LatData*)cld1;
  const LatData& ld2 = *(LatData*)cld2;
  ld = ld1 - ld2;
}

void clib_lat_data_scale(ClibLatData cld, ClibLatData cld1, const double factor)
{
  LatData& ld = *((LatData*)cld);
  const LatData& ld1 = *(LatData*)cld1;
  ld = factor * ld1;
}

