//
// Set up a clasp.h include file with all the good stuff
//
#include <stdio.h>
#include <clasp/clasp.h>
#include <clasp/clbind/clbind.h>
#include <clasp/core/numbers.h>
#include <clasp/core/debugger.h>
#include <clasp/core/array.h>
#include <clasp/core/package.h>
#include <clasp/core/translators.h> // assorted translators for string etc
#include <clasp/core/cons.h>

#define DIAG

namespace dv {
class MismatchedDimension : public std::exception {};

class DoubleVector {
private:
  vector<double> values;

public:
  DoubleVector(int sz) { this->values.resize(sz); };

  DoubleVector(const vector<double> &arg) { this->fill(arg); }

  void fill(const vector<double> &arg) {
    this->values.resize(arg.size());
    for (int i = 0; i < arg.size(); ++i) {
      this->values[i] = arg[i];
    }
  }

  double &operator[](size_t idx) { return this->values[idx]; };

  const double &operator[](size_t idx) const { return this->values[idx]; };

  size_t dimension() const { return this->values.size(); };

  void set_dimension(int sz) { this->values.resize(sz, 0.0); };

  DoubleVector add(const DoubleVector &y) {
    if (this->dimension() != y.dimension()) {
      throw MismatchedDimension();
    }
    DoubleVector result(this->dimension());
    for (int i = 0; i < this->dimension(); ++i) {
      result[i] = (*this)[i] + y[i];
    }
    return result;
  }

  double vref(int i) {
    if (i < 0 || i >= this->dimension()) {
      SIMPLE_ERROR("Index out of bounds");
    }
    return this->values[i];
  };

  double setf_vref(double value, int i) {
    if (i < 0 || i >= this->dimension()) {
      SIMPLE_ERROR("Index out of bounds");
    }
    return this->values[i] = value;
  };

  double dot(const DoubleVector &y) {
    if (this->dimension() != y.dimension()) {
      throw MismatchedDimension();
    }
    double dot = 0.0;
    for (int i = 0; i < this->dimension(); ++i) {
      dot += (*this)[i] * y[i];
    }
    return dot;
  }
};
}; // namespace dv

namespace translate {
template <typename T> struct to_object<const vector<T> &> {
  static core::T_sp convert(const vector<T> &v) {
    core::SimpleVector_sp vec = core::SimpleVector_O::make(v.size());
    for (int i(0); i < v.size(); ++i) {
      (*vec)[i] = to_object<T>::convert(v[i]);
    }
    return vec;
  }
};

template <typename T> struct from_object<const vector<T> &> {
  typedef vector<T> DeclareType;
  DeclareType _v;
  from_object(core::T_sp obj) {
    if (obj.nilp()) {
      this->_v.clear();
    } else if (core::List_sp list = obj.asOrNull<core::List_V>()) {
      // Translate a CONS list of doubles into a vector<T>
      this->_v.resize(core::cl__length(list));
      size_t idx = 0;
      for (auto c : list) {
        if (oCar(c).notnilp()) {
          this->_v[idx++] = from_object<T>(oCar(c))._v;
        }
      }
    } else if (core::SimpleVector_sp vec = gc::As<core::SimpleVector_sp>(obj)) {
      // Translate a VECTOR of doubles into a vector<T>
      this->_v.resize(core::cl__length(vec));
      for (size_t idx(0); idx < vec->length(); ++idx) {
        this->_v[idx] = from_object<T>(vec->rowMajorAref(idx))._v;
      }
    } else {
      SIMPLE_ERROR("Could not convert %s to vector<%s>", core::_rep_(obj), typeid(T).name());
    }
  }
};
}; // namespace translate

PACKAGE_USE("COMMON-LISP");
PACKAGE_NICKNAME("DV");
NAMESPACE_PACKAGE_ASSOCIATION(dv, DVPkg, "DOUBLE-VECTOR");

namespace dv {
CL_EXPOSE
void double_vector_startup() {
  using namespace clbind;
  package_ pkg(DVPkg);
  scope_ &s = pkg.scope();

  class_<DoubleVector>(s, "double-vector")
      .def_constructor("make-double-vector", constructor<int>(), "", "", "Create a DOUBLE-VECTOR of a specific size.")
      .def("add", &DoubleVector::add, "Add two vectors together."_docstring)
      .def("dot", &DoubleVector::dot, "Computer the dot product of two vectors."_docstring)
      .def("setf_vref", &DoubleVector::setf_vref, noAutoExport(), "Set a specific component of a vector."_docstring)
      .def("vref", &DoubleVector::vref, "Return a specific component of a vector."_docstring)
      .def("dimension", &DoubleVector::dimension, "Return the dimension of a vector."_docstring);

  pkg.def(
      "double-vector",
      +[](core::Vaslist_sp args) {
        DoubleVector res = DoubleVector(args->nargs());
        for (size_t i = 0; args->nargs() > 0; i++) {
          res[i] = core::clasp_to_float(args->next_arg());
        }
        return res;
      },
      "(core:&va-rest args)"_ll, "Create a double vector with specific components."_docstring);
}
}; // namespace dv
