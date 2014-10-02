//
// Set up a clasp.h include file with all the good stuff
//

//#include "clasp.h"

#include <stdio.h>



#define EXPORT __attribute__((visibility("default")))


void helloWorld()
{
    printf("Hello World\n");
}



extern "C" {
    EXPORT
    void CLASP_MAIN()
    {
        printf("!\n!\n!\n!   Starting CLASP_MAIN\n!\n!\n");
        printf("Expose C++ code to Clasp here\n");
    }

}
