extern "C" {

typedef void* ClibLatData;

ClibLatData clib_lat_data_new();

void clib_lat_data_delete(ClibLatData cld);

void clib_lat_data_set_zero(ClibLatData cld);

void clib_lat_data_copy(ClibLatData cld, ClibLatData cld1);

void clib_lat_data_load(ClibLatData cld, const char* path);

void clib_lat_data_save(ClibLatData cld, const char* path);

long clib_lat_data_double_size(ClibLatData cld, const int level);

void clib_lat_data_print(ClibLatData cld);

int clib_lat_data_is_complex(ClibLatData cld);

int clib_lat_data_ndim(ClibLatData cld);

void clib_lat_data_dim_sizes(ClibLatData cld, long* dim_sizes, const int ndim);

const char* clib_lat_data_dim_name(ClibLatData cld, const int dim);

long clib_lat_data_dim_size(ClibLatData cld, const int dim);

long clib_lat_data_dim_idx_size(ClibLatData cld, const int dim);

const char* clib_lat_data_dim_idx_name(ClibLatData cld, const int dim, const long idx);

void clib_lat_data_add_dim(ClibLatData cld, const char* name, const long size);

void clib_lat_data_add_dim_idx_name(ClibLatData cld, const int dim, const char* name);

void clib_lat_data_set(ClibLatData cld, const long* idxs, const long level,
                       const char* data, const long dsize);

void clib_lat_data_ref(ClibLatData cld, const long* idxs, const long level,
                       char* data, const long dsize);

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

long clib_lat_data_double_size(ClibLatData cld, const int level)
  // return number of double numbers
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

void clib_lat_data_dim_sizes(ClibLatData cld, long* dim_sizes, const int ndim)
{
  const LatData& ld = *((LatData*)cld);
  assert((int)ld.info.size() == ndim or (int)ld.info.size() == ndim + 1);
  for (int i = 0; i < ndim; ++i) {
    dim_sizes[i] = ld.info[i].size;
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

long clib_lat_data_dim_idx_size(ClibLatData cld, const int dim)
{
  const LatData& ld = *((LatData*)cld);
  assert(0 <= dim and dim < (int)ld.info.size());
  return ld.info[dim].indices.size();
}

const char* clib_lat_data_dim_idx_name(ClibLatData cld, const int dim, const long idx)
{
  const LatData& ld = *((LatData*)cld);
  assert(0 <= dim and dim < (int)ld.info.size());
  assert(0 <= idx and idx < (long)ld.info[dim].indices.size());
  return ld.info[dim].indices[idx].c_str();
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

void clib_lat_data_add_dim_idx_name(ClibLatData cld, const int dim, const char* name)
{
  LatData& ld = *((LatData*)cld);
  ld.info[dim].indices.push_back(std::string(name));
}

void clib_lat_data_set(ClibLatData cld, const long* idxs, const long level,
                       const char* data, const long dsize)
{
  LatData& ld = *((LatData*)cld);
  lat_data_alloc(ld);
  const Vector<long> idx_v(idxs, level);
  Vector<double> dv = lat_data_get(ld, idx_v);
  assert(dsize == dv.data_size());
  memcpy((void*)dv.data(), (const void*)data, dsize);
}

void clib_lat_data_ref(ClibLatData cld, const long* idxs, const long level,
                       char* data, const long dsize)
{
  const LatData& ld = *((LatData*)cld);
  const Vector<long> idx_v(idxs, level);
  const Vector<double> dv = lat_data_get_const(ld, idx_v);
  assert(dsize == dv.data_size());
  memcpy((void*)data, (const void*)dv.data(), dsize);
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

