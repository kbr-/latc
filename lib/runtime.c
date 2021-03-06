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
    exit(EXIT_FAILURE);
}

int readInt() {
    int r = 0;
    scanf("%d", &r);
    int c = getchar();
    if (c != '\n') {
        ungetc(c, stdin);
    }
    return r;
}

char* readString() {
    size_t size = 32;
    char* s = malloc(size);
    int c;
    size_t len = 0;
    while (EOF != (c = getchar()) && c != '\n') {
        s[len++] = c;
        if (len == size) {
            s = realloc(s, size *= 2);
        }
    }
    s[len++]='\0';
    return realloc(s, len);
}

char* _concat(const char* s1, const char* s2) {
    char* t = malloc(strlen(s1)+strlen(s2)+1);
    return strcat(strcpy(t, s1), s2);
}

char* _new(int n) {
    char* r =  malloc(4*(n+1));
    ((int*)r)[0] = n;
    return r;
}

char* _newStruct(int size) {
    return malloc(4*size);
}
