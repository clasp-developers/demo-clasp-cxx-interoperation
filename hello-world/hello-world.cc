//
// Set up a clasp.h include file with all the good stuff
//
#include <stdio.h>
#include <clasp/clasp.h>

void helloWorld()
{
    printf("Hello World\n");
    printf("This is C++ code being invoked from Clasp Common Lisp\n");
}

double addThreeNumbers(double x, double y, double z)
{
    return x+y+z;
}

   
float addThreeSingleFloats(float x, float y, float z)
{
    return x+y+z;
}

double addThreeNumbers_n_times(size_t n, double x, double y, double z)
{
    double result = 0.0;
    for (size_t i(0); i<n; ++i ) {
	result += x+y+z;
    }
    return result;
}


// ------------------------------------------------------------
//
// Set this code up so that it can be loaded into Clasp
//

namespace hw {
CL_EXPOSE
void hello_world_startup() {
  printf("Entered %s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__ );
  using namespace clbind;
  package_ pkg("HW", {}, {} );
  scope_& s = pkg.scope();
  s.def("hello-world",&helloWorld);
  s.def("addThreeNumbers",&addThreeNumbers);
  s.def("addThreeSingleFloats",&addThreeSingleFloats);
  s.def("addThreeNumbers_n_times", &addThreeNumbers_n_times);
}
};
