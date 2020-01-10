#include <stdio.h>

int fac(int n) {
    int r = 1;
    int c = 1;
    while(c <= n){
        r = r * c;
        c = c + 1;
    }
    return r;
}

int main() {
    int r = fac(10);
    printf("result: %d\n", r);
}