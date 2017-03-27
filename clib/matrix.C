#include <eigen3/Eigen/Dense>
#include <complex>

extern "C" {

  void clib_matrix_scale(char* ret, const char* x, const double real, const double imag);

  void clib_matrix_plus(char* ret, const char* x, const char* y);

  void clib_matrix_minus(char* ret, const char* x, const char* y);

  void clib_matrix_multiply(char* ret, const char* x, const char* y);

  void clib_matrix_negate(char* ret, const char* x);

  void clib_matrix_inverse(char* ret, const char* x);

}

using namespace Eigen;

typedef std::complex<double> Complex;

typedef Matrix<Complex, Dynamic, Dynamic, RowMajor> Mat;

Mat clib_make_matrix(const char* x)
{
  const int64_t* dims = (const int64_t*)x;
  const Complex* data = (const Complex*)x + 1;
  const int n = dims[0];
  const int m = dims[1];
  Mat ret(n,m);
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      ret(i,j) = data[m * i + j];
    }
  }
  return ret;
}

void clib_set_matrix(char* x, const Mat& mat)
{
  const int n = mat.rows();
  const int m = mat.cols();
  int64_t* dims = (int64_t*)x;
  Complex* data = (Complex*)x + 1;
  dims[0] = n;
  dims[1] = m;
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      data[m * i + j] = mat(i,j);
    }
  }
}

void clib_matrix_plus(char* ret, const char* x, const char* y)
{
  clib_set_matrix(ret, clib_make_matrix(x) + clib_make_matrix(y));
}

void clib_matrix_minus(char* ret, const char* x, const char* y)
{
  clib_set_matrix(ret, clib_make_matrix(x) - clib_make_matrix(y));
}

void clib_matrix_scale(char* ret, const char* x, const double real, const double imag)
{
  clib_set_matrix(ret, clib_make_matrix(x) * Complex(real, imag));
}

void clib_matrix_multiply(char* ret, const char* x, const char* y)
{
  clib_set_matrix(ret, clib_make_matrix(x) * clib_make_matrix(y));
}

void clib_matrix_negate(char* ret, const char* x)
{
  clib_set_matrix(ret, -clib_make_matrix(x));
}

void clib_matrix_inverse(char* ret, const char* x)
{
  clib_set_matrix(ret, clib_make_matrix(x).inverse());
}
