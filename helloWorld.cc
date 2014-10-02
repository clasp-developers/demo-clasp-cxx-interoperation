//
// Set up a clasp.h include file with all the good stuff
//
#include <stdio.h>
#include "clasp.h"

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
        printf("!\n!\n!\n!   Starting CLASP_MAIN\n!\n!\n");
        printf("Expose C++ code to Clasp here\n");
        clbind::package("HELLO") [
            clbind::def("hello-world",&helloWorld)
            , clbind::def("addThreeNumbers",&addThreeNumbers)
            ];
    }
}
