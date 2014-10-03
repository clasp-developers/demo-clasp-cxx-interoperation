//
// Set up a clasp.h include file with all the good stuff
//
#include <stdio.h>
#include "clasp.h"
#include "core/lispVector.h"
#include "core/vectorObjects.h"
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

class MismatchedSize : public std::exception {};

class DoubleVector {
private:
    vector<double>      values;
public:
    DoubleVector(int sz) {this->values.resize(sz);};
    DoubleVector(const vector<double>& arg) {
        this->fill(arg);
    }
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
    void add(DoubleVector& result, const DoubleVector& y) {
        if ( result.size() != this->size() || this->size() != y.size() ) {
            throw MismatchedSize();
        }
        for ( int i=0; i<this->size(); ++i ) {
            result[i] = (*this)[i]+y[i];
        }
    }
    double dot(const DoubleVector& y) {
        if ( this->size() != y.size() ) {
            throw MismatchedSize();
        }
        double dot = 0.0;
        for ( int i=0; i<this->size(); ++i ) {
            dot += (*this)[i]*y[i];
        }
        return dot;
    }
};


namespace translate {
    template <> struct to_object<const vector<double>&>
    {
        static core::T_sp convert(const vector<double>& v)
        {
            core::VectorObjects_sp vec;
            vec->adjust(_Nil<core::T_O>(),_Nil<core::Cons_O>(),v.size());
            printf("%s:%d fill the vec here\n", __FILE__, __LINE__ );
            return vec;
        }
    };

    template <> struct from_object<vector<double> >
    {
        typedef vector<double> DeclareType;
        DeclareType _v;
        from_object(core::T_sp obj)
        {
            if ( obj.nilp() ) {
                this->_v.clear();
            } else if ( core::Cons_sp list = obj.asOrNull<core::Cons_O>() ) {
                // Translate a CONS list of doubles into a vector<double>
                this->_v.resize(list->length());
                size_t idx=0;
                for ( ; list.notnilp(); list=cCdr(list) ) {
                    this->_v[idx++] = oCar(list).as<core::Number_O>()->as_double();
                }
            } else if ( core::Vector_sp vec = obj.asOrNull<core::Vector_O>() ) {
                // Translate a VECTOR of doubles into a vector<double>
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



extern "C" {
    EXPORT
    void CLASP_MAIN()
    {
        using namespace clbind;
        package("HW") [
            def("hello-world",&helloWorld)
            , def("addThreeNumbers",&addThreeNumbers)
            , class_<DoubleVector>("double-vector" ,no_default_constructor )
            .   def_constructor("make-double-vector-with-size",constructor<int>())
            .   def_constructor("make-double-vector-with-values",constructor<vector<double>>())
            .   def("fill",&DoubleVector::fill)
            .   def("add",&DoubleVector::add)
            .   def("dot",&DoubleVector::dot)
            ];
    }
}
