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


enum ColorEnum { red, green, blue  };

void printColor( ColorEnum color ) {
  switch (color) {
  case red:
      printf("red\n");
      break;
  case green:
      printf("green\n");
      break;
  case blue:
      printf("blue\n");
      break;
  }
}

// ------------------------------------------------------------
//
// Set this code up so that it can be loaded into Clasp
//

PACKAGE_NICKNAME("HW");
NAMESPACE_PACKAGE_ASSOCIATION(hw,HWPkg,"HELLO-WORLD");

SYMBOL_EXPORT_SC_(HWPkg,STARcolorTranslatorSTAR);
CLBIND_TRANSLATE_SYMBOL_TO_ENUM(ColorEnum, hw::_sym_STARcolorTranslatorSTAR );

namespace hw {
CL_EXPOSE
void hello_world_startup() {
  printf("Entered %s:%d:%s\n", __FILE__, __LINE__, __FUNCTION__ );
  using namespace clbind;
  package_ pkg(HWPkg);
  scope_& s = pkg.scope();
  s.def("hello-world",&helloWorld);
  s.def("addThreeNumbers",&addThreeNumbers);
  s.def("addThreeSingleFloats",&addThreeSingleFloats);
  s.def("addThreeNumbers_n_times", &addThreeNumbers_n_times);
  enum_<ColorEnum>(s,hw:_sym_STARcolorTranslatorSTAR)
      .value("red",red)
      .value("green",green)
      .value("blue",blue);
  s.def("printColor",&printColor);
}


};

