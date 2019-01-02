#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void printInt(int x) {
    printf("%d\n", x);
}

void printString(const char* str) {
    printf("%s\n", str);
}

void error() {
    printf("runtime error\n");
    abort();
}

int readInt() {
    int r;
    scanf("%d", &r);
    return r;
}

char* readString() {
    char* s = 0;
    scanf("%m[^\n]", &s);
    return s;
}

char* _concat(const char* s1, const char* s2) {
    char* t = malloc(strlen(s1)+strlen(s2)+1);
    return strcat(strcpy(t, s1), s2);
}
