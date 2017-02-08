#include <stdio.h>
#include <stdlib.h>

int g;
int f(int x){
    ++g;
    return 3;
}

int main(){
    g = 6;
    if(g == f(2) + f(2))
        printf("Eject!");
    else
        printf("Maintain current course");
}
