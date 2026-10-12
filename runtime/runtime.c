/*
 * The runtime library linked into every compiled Latte program.
 *
 * Compiled code relies on the following representation of values:
 *
 *   - a string is a pointer to NUL-terminated bytes, and the null pointer
 *     stands for the empty string;
 *   - an array is a pointer to its 32-bit length, followed, ARRAY_HEADER
 *     bytes in, by its elements;
 *   - an object is a block of fields.
 *
 * Arrays and objects start out zeroed, which gives every element and field
 * its default value: 0, false, the empty string or null.
 *
 * Memory is never freed: Latte has no way to release it, and the compiler
 * does not emit the information a garbage collector would need.
 *
 * The functions called by compiled code are named lrt_*. A function f of the
 * compiled program is named latte_f, so the two sets of names cannot clash.
 */

#define _GNU_SOURCE

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

/* Must agree with arrayHeader in src/Latte/Layout.hs. */
#define ARRAY_HEADER 8

/* The compiled program's `main`. */
extern int latte_main(void);

void lrt_error(void) {
    fflush(stdout);
    fputs("runtime error\n", stderr);
    exit(1);
}

static void *allocate(size_t size) {
    /* A size of zero is rounded up, so that distinct objects stay distinct. */
    void *memory = calloc(1, size > 0 ? size : 1);
    if (memory == NULL) {
        lrt_error();
    }
    return memory;
}

void *lrt_new_object(int size) {
    return allocate((size_t)size);
}

void *lrt_new_array(int length, int element_size) {
    if (length < 0) {
        lrt_error();
    }
    int *array = allocate(ARRAY_HEADER + (size_t)length * (size_t)element_size);
    *array = length;
    return array;
}

void lrt_print_int(int value) {
    printf("%d\n", value);
}

void lrt_print_string(const char *string) {
    puts(string != NULL ? string : "");
}

static int is_blank(int c) {
    return c == ' ' || c == '\t' || c == '\r';
}

/* Reads an integer and, if nothing else is left on its line, the line break.
   Input that is not an integer, or does not fit in an int, is an error. */
int lrt_read_int(void) {
    int next = getchar();
    while (is_blank(next) || next == '\n') {
        next = getchar();
    }
    int negative = next == '-';
    if (next == '-' || next == '+') {
        next = getchar();
    }
    /* The magnitude, which stops growing once it is too large for an int. */
    long long magnitude = 0;
    int digits = 0;
    for (; next >= '0' && next <= '9'; next = getchar()) {
        if (magnitude <= INT_MAX) {
            magnitude = magnitude * 10 + (next - '0');
        }
        digits++;
    }
    long long value = negative ? -magnitude : magnitude;
    if (digits == 0 || value < INT_MIN || value > INT_MAX) {
        lrt_error();
    }
    while (is_blank(next)) {
        next = getchar();
    }
    if (next != '\n' && next != EOF) {
        ungetc(next, stdin);
    }
    return (int)value;
}

/* Reads a line, without its line break. At the end of input, the result is
   the empty string. */
char *lrt_read_string(void) {
    char *line = NULL;
    size_t capacity = 0;
    ssize_t length = getline(&line, &capacity, stdin);
    if (length < 0) {
        free(line);
        return NULL;
    }
    if (length > 0 && line[length - 1] == '\n') {
        line[--length] = '\0';
    }
    if (length > 0 && line[length - 1] == '\r') {
        line[--length] = '\0';
    }
    return line;
}

/* Strings are immutable, so concatenating with an empty string can return the
   other operand instead of a copy. */
char *lrt_concat(char *left, char *right) {
    if (left == NULL || *left == '\0') {
        return right;
    }
    if (right == NULL || *right == '\0') {
        return left;
    }
    size_t left_length = strlen(left);
    size_t right_length = strlen(right);
    char *result = allocate(left_length + right_length + 1);
    memcpy(result, left, left_length);
    memcpy(result + left_length, right, right_length + 1);
    return result;
}

int lrt_string_equals(const char *left, const char *right) {
    return strcmp(left != NULL ? left : "", right != NULL ? right : "") == 0;
}

int main(void) {
    /* Each line is written out as soon as it is complete, so that nothing the
       program has printed is lost if it is later killed by a signal. */
    setvbuf(stdout, NULL, _IOLBF, 0);
    return latte_main();
}
