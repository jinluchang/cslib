#pragma once

#include <cassert>
#include <string>
#include <vector>

#include <endian.h>
#include <omp.h>
#include <stdint.h>

#include <zlib.h>

#include "crc32.h"
#include "qutils-io.h"
#include "qutils.h"
#include "show.h"

namespace qlat
{  //

const std::string lat_data_header = "#!/usr/bin/env lat-io-glimpse\n";

struct LatDim {
  std::string name;
  long size;                         // size of this dimension
  std::vector<std::string> indices;  // indices names
                                     // (default: "-1", "-2", "-3", ...)
                                     // If indices.size() < size then example
                                     // will be indices[0], indices[1], "-3",
                                     // "-4", ... Won't check duplication (when
                                     // assess, search from left to right)
  //
  LatDim() { size = 0; }
};

typedef std::vector<LatDim> LatInfo;

struct LatData {
  LatInfo info;
  std::vector<double> res;
  //
  LatData(){};
  //
  void load(const std::string& fn);
  void save(const std::string& fn) const;
};

inline bool is_initialized(const LatData& ld)
{
  return ld.res.size() > 0;
}

inline long lat_data_size(const LatInfo& info, const int level = 0)
{
  if (info.size() == 0) {
    return 0;
  }
  long total = 1;
  for (int i = level; i < (int)info.size(); ++i) {
    total *= info[i].size;
  }
  return total;
}

inline void lat_data_alloc(LatData& ld)
{
  ld.res.resize(lat_data_size(ld.info));
}

inline std::string show(const LatDim& dim)
{
  std::ostringstream out;
  out << ssprintf("\"%s\"[%ld]:", dim.name.c_str(), dim.size);
  for (long i = 0; i < (long)dim.indices.size(); ++i) {
    out << ssprintf(" \"%s\"", dim.indices[i].c_str());
  }
  return out.str();
}

inline std::string show(const LatInfo& info)
{
  std::ostringstream out;
  out << ssprintf("ndim: %ld\n", info.size());
  for (int i = 0; i < (int)info.size(); ++i) {
    out << show(info[i]) << "\n";
  }
  return out.str();
}

inline LatDim read_lat_dim(const std::string& str)
{
  LatDim dim;
  long cur = 0;
  char c;
  if (!parse_string(dim.name, cur, str)) {
    assert(false);
  } else if (!parse_char(c, cur, str) or c != '[') {
    assert(false);
  } else if (!parse_long(dim.size, cur, str)) {
    assert(false);
  } else if (!parse_char(c, cur, str) or c != ']') {
    assert(false);
  } else if (!parse_char(c, cur, str) or c != ':') {
    assert(false);
  } else {
    while (parse_char(c, cur, str)) {
      assert(c == ' ');
      std::string index;
      if (!parse_string(index, cur, str)) {
        assert(false);
      }
      dim.indices.push_back(index);
    }
  }
  return dim;
}

inline LatInfo read_lat_info(const std::string& str)
{
  LatInfo info;
  const std::vector<std::string> infos = split_into_lines(str);
  assert(infos.size() >= 1);
  const std::string ndim_prop = "ndim: ";
  assert(infos[0].compare(0, ndim_prop.size(), ndim_prop) == 0);
  const long ndim = read_long(std::string(infos[0], ndim_prop.size()));
  assert(ndim == (long)infos.size() - 1);
  for (int i = 1; i < (int)infos.size(); ++i) {
    info.push_back(read_lat_dim(infos[i]));
  }
  return info;
}

inline void LatData::load(const std::string& fn)
{
  FILE* fp = fopen(fn.c_str(), "r");
  assert(fp != NULL);
  std::vector<char> check_line(lat_data_header.size(), 0);
  fread(check_line.data(), lat_data_header.size(), 1, fp);
  assert(std::string(check_line.data(), check_line.size()) == lat_data_header);
  std::vector<std::string> infos;
  infos.push_back(lat_data_header);
  while (infos.back() != "END_HEADER\n" && infos.back() != "") {
    infos.push_back(qgetline(fp));
  }
  std::ostringstream out;
  for (int i = 3; i < (int)infos.size() - 2; ++i) {
    out << infos[i];
  }
  const std::string info_str = out.str();
  info = read_lat_info(info_str);
  const std::string& crc_str = infos[infos.size() - 2];
  const std::string crc_prop = "crc32: ";
  assert(crc_str.compare(0, crc_prop.size(), crc_prop) == 0);
  const crc32_t crc = read_crc32(std::string(crc_str, crc_prop.size()));
  lat_data_alloc(*this);
  assert((long)res.size() == lat_data_size(info));
  assert((long)res.size() * (long)sizeof(double) == read_long(infos[2]));
  fread(res.data(), sizeof(double), res.size(), fp);
  const crc32_t crc_computed =
      crc32_par(res.data(), res.size() * sizeof(double));
  if (crc != crc_computed) {
    displayln(
        ssprintf("ERROR: crc do not match: file=%08X computed=%08X fn='%s'.",
                 crc, crc_computed, fn.c_str()));
    assert(false);
  }
  to_from_little_endian_64(res.data(), res.size() * sizeof(double));
  fclose(fp);
}

inline void LatData::save(const std::string& fn) const
{
  FILE* fp = fopen((fn + ".partial").c_str(), "w");
  assert(fp != NULL);
  std::vector<double> res_copy;
  if (!is_little_endian()) {
    res_copy = res;
    assert(res_copy.size() == res.size());
    to_from_little_endian_64(res_copy.data(), res_copy.size() * sizeof(double));
  }
  const std::string data_size =
      ssprintf("data_size\n%ld\n", res.size() * sizeof(double));
  const std::string info_str = show(info);
  const std::string checksum_str =
      ssprintf("crc32: %08X\n",
               crc32_par(is_little_endian() ? res.data() : res_copy.data(),
                         res.size() * sizeof(double)));
  const std::string end_header = "END_HEADER\n";
  fwrite(lat_data_header.data(), lat_data_header.size(), 1, fp);
  fwrite(data_size.data(), data_size.size(), 1, fp);
  fwrite(info_str.data(), info_str.size(), 1, fp);
  fwrite(checksum_str.data(), checksum_str.size(), 1, fp);
  fwrite(end_header.data(), end_header.size(), 1, fp);
  fwrite(is_little_endian() ? res.data() : res_copy.data(), sizeof(double),
         res.size(), fp);
  fclose(fp);
  rename((fn + ".partial").c_str(), fn.c_str());
}

inline void clear(LatData& ld)
{
  clear(ld.info);
  clear(ld.res);
}

inline LatDim lat_dim_number(const std::string& name, const long start,
                             const long end, const long inc = 1)
{
  LatDim dim;
  dim.name = name;
  for (long i = start; i <= end; i += inc) {
    dim.size += 1;
    dim.indices.push_back(show(i));
  }
  return dim;
}

template <unsigned long N>
inline LatDim lat_dim_string(const std::string& name,
                             const std::array<std::string, N>& is)
{
  LatDim dim;
  dim.name = name;
  dim.size = N;
  for (int i = 0; i < (int)N; ++i) {
    dim.indices.push_back(is[i]);
  }
  return dim;
}

inline LatDim lat_dim_re_im()
{
  return lat_dim_string("re-im", make_array<std::string>("re", "im"));
}

inline long lat_dim_idx(const LatDim& dim, const std::string& idx)
{
  assert((long)dim.indices.size() <= dim.size);
  for (long i = 0; i < (long)dim.indices.size(); ++i) {
    if (idx == dim.indices[i]) {
      return i;
    }
  }
  const long i = -read_long(idx) - 1;
  assert((long)dim.indices.size() <= i and i < dim.size);
  return i;
}

inline long lat_dim_idx(const LatDim& dim, const long& idx)
{
  assert((long)dim.indices.size() <= dim.size);
  assert(0 <= idx and idx < dim.size);
  return idx;
}

template <class VecS>
inline long lat_data_offset(const LatInfo& info, const VecS& idx)
// will return offset at the level the idx specify
// VecS can be std::vector<std::string> or std::vector<long>
// or can be std::array of certain length
{
  assert((long)idx.size() <= (long)info.size());
  long ret = 0;
  for (int i = 0; i < (int)idx.size(); ++i) {
    const long k = lat_dim_idx(info[i], idx[i]);
    ret = ret * info[i].size + k;
  }
  return ret;
}

inline bool is_lat_info_complex(const LatInfo& info)
{
  assert((long)info.size() >= 1);
  const LatDim& dim = info.back();
  if (dim.name != "re-im" or dim.size != 2) {
    return false;
  } else if (dim.indices.size() != 2) {
    return false;
  } else if (dim.indices[0] != "re" or dim.indices[1] != "im") {
    return false;
  } else {
    return true;
  }
}

inline std::string idx_name(const LatDim& dim, const long idx)
{
  if (idx < (long)dim.indices.size()) {
    return dim.indices[idx];
  } else {
    return show(-idx - 1);
  }
}

inline void print(const LatData& ld)
{
  const LatInfo& info = ld.info;
  display(ssprintf("%s", show(info).c_str()));
  std::vector<long> idx(info.size(), 0);
  for (long k = 0; k < lat_data_size(info); ++k) {
    for (int a = 0; a < (int)info.size(); ++a) {
      display(ssprintf("%s[%8s] ", info[a].name.c_str(),
                       idx_name(info[a], idx[a]).c_str()));
    }
    display(ssprintf("%24.17E\n", ld.res[lat_data_offset(info, idx)]));
    idx[info.size() - 1] += 1;
    for (int a = info.size() - 1; a > 0; --a) {
      if (idx[a] == info[a].size) {
        idx[a] = 0;
        idx[a - 1] += 1;
      }
    }
  }
}

inline bool operator==(const LatDim& d1, const LatDim& d2)
{
  return d1.name == d2.name and d1.size == d2.size and d1.indices == d2.indices;
}

inline bool operator!=(const LatDim& d1, const LatDim& d2)
{
  return not(d1 == d2);
}

inline void set_zero(LatData& ld)
{
  std::memset(ld.res.data(), 0, ld.res.size() * sizeof(double));
}

inline bool is_matching(const LatData& ld1, const LatData& ld2)
{
  return ld1.res.size() == ld2.res.size() and ld1.info == ld2.info;
}

inline const LatData& operator*=(LatData& ld, const double factor)
{
  for (long i = 0; i < (long)ld.res.size(); ++i) {
    ld.res[i] *= factor;
  }
  return ld;
}

inline const LatData& operator+=(LatData& ld, const LatData& ld1)
{
  if (not is_initialized(ld)) {
    ld = ld1;
  } else {
    assert(is_matching(ld, ld1));
    for (long i = 0; i < (long)ld.res.size(); ++i) {
      ld.res[i] += ld1.res[i];
    }
  }
  return ld;
}

inline const LatData& operator-=(LatData& ld, const LatData& ld1)
{
  if (not is_initialized(ld)) {
    ld = ld1;
    ld *= -1;
  } else {
    assert(is_matching(ld, ld1));
    for (long i = 0; i < (long)ld.res.size(); ++i) {
      ld.res[i] -= ld1.res[i];
    }
  }
  return ld;
}

inline LatData operator+(const LatData& ld, const LatData& ld1)
{
  LatData ret = ld;
  ret += ld1;
  return ret;
}

inline LatData operator-(const LatData& ld, const LatData& ld1)
{
  LatData ret = ld;
  ret -= ld1;
  return ret;
}

inline LatData operator*(const LatData& ld, const double factor)
{
  LatData ret = ld;
  for (long i = 0; i < (long)ret.res.size(); ++i) {
    ret.res[i] *= factor;
  }
  return ret;
}

inline LatData operator*(const double factor, const LatData& ld)
{
  return ld * factor;
}

template <class VecS>
inline Vector<double> lat_data_get(LatData& ld, const VecS& idx)
{
  const long offset = lat_data_offset(ld.info, idx);
  const long size = lat_data_size(ld.info, idx.size());
  qassert(offset * size + size <= (long)ld.res.size());
  Vector<double> ret(&ld.res[offset * size], size);
  return ret;
}

template <class VecS>
inline Vector<double> lat_data_get_const(const LatData& ld, const VecS& idx)
// Be cautious about the const property
// 改不改靠自觉
{
  const long offset = lat_data_offset(ld.info, idx);
  const long size = lat_data_size(ld.info, idx.size());
  qassert(offset * size + size <= (long)ld.res.size());
  Vector<double> ret(&ld.res[offset * size], size);
  return ret;
}

template <class VecS>
inline Vector<Complex> lat_data_complex_get(LatData& ld, const VecS& idx)
{
  qassert(is_lat_info_complex(ld.info));
  qassert((long)idx.size() < (long)ld.info.size());
  const long offset = lat_data_offset(ld.info, idx);
  const long size = lat_data_size(ld.info, idx.size());
  qassert(size % 2 == 0);
  qassert(offset * size + size <= (long)ld.res.size());
  Vector<Complex> ret((Complex*)&ld.res[offset * size], size / 2);
  return ret;
}

template <class VecS>
inline Vector<Complex> lat_data_complex_get_const(const LatData& ld,
                                                  const VecS& idx)
// Be cautious about the const property
// 改不改靠自觉
{
  qassert(is_lat_info_complex(ld.info));
  qassert((long)idx.size() < (long)ld.info.size());
  const long offset = lat_data_offset(ld.info, idx);
  const long size = lat_data_size(ld.info, idx.size());
  qassert(size % 2 == 0);
  qassert(offset * size + size <= (long)ld.res.size());
  Vector<Complex> ret((Complex*)&ld.res[offset * size], size / 2);
  return ret;
}

}  // namespace qlat

#ifndef USE_NAMESPACE
using namespace qlat;
#endif
