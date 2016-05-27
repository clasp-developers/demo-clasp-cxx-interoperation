//
// Set up a clasp.h include file with all the good stuff
//
#include <stdio.h>
#include <clbind.h>
#include <lambdaListHandler.h>

#define EXPORT __attribute__((visibility("default")))

void helloWorld()
{
    printf("Hello World\n");
}

double addThreeNumbers(double x, double y, double z)
{
    return x+y+z;
}

extern "C" {
    EXPORT
    void CLASP_MAIN()
    {
        using namespace clbind;
        package("HW") [
            def("hello-world",&helloWorld)
            , def("addThreeNumbers",&addThreeNumbers)
            ];
    }
}
