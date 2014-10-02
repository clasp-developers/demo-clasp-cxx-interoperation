//
// Set up a clasp.h include file with all the good stuff
//
#include <stdio.h>
#include "clasp.h"
#include "core/lispVector.h"
#include "core/cons.h"


#define EXPORT __attribute__((visibility("default")))

void helloWorld()
{
    printf("Hello World\n");
}


double addThreeNumbers(double x, double y, double z)
{
    return x+y+z;
}

#ifdef DEMO_CLASS

class MismatchedSize : public std::exception {};

class DoubleVector {
private:
    vector<double>      values;
public:
    void fill(const vector<double>& arg) {
        this->values.resize(arg.size());
        for (int i=0; i<arg.size(); ++i ) {
            this->values[i] = arg[i];
        }
    }

    double& operator[](size_t idx) { return this->values[idx];};
    const double& operator[](size_t idx) const { return this->values[idx];};
    size_t size() const { return this->values.size(); };
    void setSize(int sz) { this->values.resize(sz,0.0); };
    void add(DoubleVector& result, const DoubleVector& x, const DoubleVector& y) {
        if ( result.size() != x.size() || result.size() != y.size() ) {
            throw MismatchedSize();
        }
        for ( int i=0; i<result.size(); ++i ) {
            result[i] = x[i]+y[i];
        }
    }
    double dot(const DoubleVector& x, const DoubleVector& y) {
        if ( x.size() != y.size() ) {
            throw MismatchedSize();
        }
        double dot = 0.0;
        for ( int i=0; i<x.size(); ++i ) {
            dot += x[i]*y[i];
        }
        return dot;
    }
};


namespace translate {
    template <> struct from_object<vector<double> >
    {
        typedef vector<double> DeclareType;
        DeclareType _v;
        from_object(core::T_sp obj)
        {
            if ( obj.nilp() ) {
                this->_v.clear();
            } else if ( core::Cons_sp list = obj.asOrNull<core::Cons_O>() ) {
                this->_v.resize(list->length());
                size_t idx=0;
                for ( ; list.notnilp(); list=cCdr(list) ) {
                    this->_v[idx++] = oCar(list).as<core::DoubleFloat_O>()->get();
                }
            } else if ( core::Vector_sp vec = obj.asOrNull<core::Vector_O>() ) {
                this->_v.resize(vec->length());
                for ( size_t idx; idx<vec->length(); ++idx ) {
                    this->_v[idx] = (*vec)[idx].as<core::Number_O>()->as_double();
                }
            } else {
                SIMPLE_ERROR(BF("Could not convert %s to vector<double>") % core::_rep_(obj));
            }
        }
    };
};
#endif



extern "C" {
    EXPORT
    void CLASP_MAIN()
    {
        using namespace clbind;

        printf("!\n!\n!\n!   Starting CLASP_MAIN\n!\n!\n");
        printf("Expose C++ code to Clasp here\n");
        package("HELLO") [
            def("hello-world",&helloWorld)
            , def("addThreeNumbers",&addThreeNumbers)
#ifdef DEMO_CLASS
            , class_<DoubleVector>("double-vector") //,no_default_constructor )
            .   def("fill",&DoubleVector::fill)
            .   def("add",&DoubleVector::add)
            .   def("dot",&DoubleVector::dot)
#endif
            ];
    }
}
