#include "lat-io.h"

extern "C" {

typedef void* ClibLatData;

ClibLatData clib_lat_data_new();

void clib_lat_data_delete(ClibLatData cld);

void clib_lat_data_load(ClibLatData cld, const char* path);

void clib_lat_data_save(ClibLatData cld, const char* path);
}

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
