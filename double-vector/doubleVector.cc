//
// Set up a clasp.h include file with all the good stuff
//
#include <stdio.h>
#include <clasp/clasp.h>

#define DIAG

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
	    printf("Filling %d --> %lf\n", i, arg[i]);
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
    double at(int i) 
    {
	if ( i<0 || i >= this->values.size() ) {
	    SIMPLE_ERROR(BF("Index out of bounds"));
	}
	return this->values[i]; 
    };
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

    // Note: Here I'm setting up arguments as a Common Lisp lambda-list for the method.
    // Just don't go nuts with initializers. :-)
#define ARGS_dump "(self &optional (prefix \"entry\"))"
#define DECL_dump ""
#define DOCS_dump "Dump the vector using printf. Optionally provide a prefix string to print"
    void dump(const string& prefix ) {
	printf("Dumping double-vector\n");
	for ( int i(0); i<this->values.size(); ++i ) {
	    printf("%s[%3d] %lf\n", prefix.c_str(), i, this->values[i]);
	}
    }
    
};


namespace translate {
    template <typename T> struct to_object<const vector<T>&>
    {
        static core::T_sp convert(const vector<T>& v)
        {
            core::SimpleVector_sp vec = core::SimpleVector_O::make(v.size());
            for ( int i(0); i<v.size(); ++i ) {
		(*vec)[i] = to_object<T>::convert(v[i]);
            }
            return vec;
        }
    };

    template <typename T> struct from_object<const vector<T>& >
    {
        typedef vector<T> DeclareType;
        DeclareType _v;
        from_object(core::T_sp obj)
        {
            if ( obj.nilp() ) {
                this->_v.clear();
            } else if ( core::List_sp list = obj.asOrNull<core::List_V>() ) {
                // Translate a CONS list of doubles into a vector<T>
                this->_v.resize(core::cl__length(list));
                size_t idx=0;
                for ( auto c : list ) {
		    if (oCar(c).notnilp()) {
			this->_v[idx++] = from_object<T>(oCar(c))._v;
		    }
                }
            } else if (core::Vector_sp vec = obj.asOrNull<core::Vector_O>() ) {
                // Translate a VECTOR of doubles into a vector<T>
                this->_v.resize(core::cl__length(vec));
                for ( size_t idx(0); idx<vec->length(); ++idx ) {
		    this->_v[idx] = from_object<T>(vec->rowMajorAref(idx))._v;
                }
            } else {
		SIMPLE_ERROR(BF("Could not convert %s to vector<%s>") % core::_rep_(obj) % typeid(T).name());
            }
        }
    };
};



void startup()
{
    printf("Exporting functions\n");
    using namespace clbind;
    package("DV") [
		   class_<DoubleVector>("double-vector" ,no_default_constructor )
		   .   def_constructor("make-double-vector-with-size",constructor<int>())
		   .   def_constructor("make-double-vector-with-values",constructor<const vector<double>&>())
		   .   def("fill",&DoubleVector::fill)
		   .   def("add",&DoubleVector::add)
		   .   def("dot",&DoubleVector::dot)
		   .   def("at",&DoubleVector::at)
		   .   def("dump",&DoubleVector::dump,policies<>(),ARGS_dump,DECL_dump,DOCS_dump)
		   ];
};


CLASP_REGISTER_STARTUP(startup);
