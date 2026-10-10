#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_INVALID_ARGUMENT,
    STATUS_OVERFLOW,
    STATUS_NO_CONVERGENCE,
    STATUS_MEMORY_ERROR,
    STATUS_FILE_ERROR,
    STATUS_NO_ROOTS
} StatusCode;

static int parse_int(const char *text, int *value)
{
    char *end = NULL;
    long parsed;

    if (text == NULL || text[0] == '\0') {
        return 0;
    }
    errno = 0;
    parsed = strtol(text, &end, 10);
    if (errno == ERANGE || end == text || *end != '\0' ||
        parsed < INT_MIN || parsed > INT_MAX) {
        return 0;
    }
    *value = (int)parsed;
    return 1;
}

static int random_in_range(int minimum, int maximum)
{
    double fraction = (double)rand() / ((double)RAND_MAX + 1.0);
    long long range = (long long)maximum - minimum + 1;
    return (int)((long long)minimum + (long long)(fraction * range));
}

static void print_array(const char *name, const int *array, size_t size)
{
    size_t shown = size < 20 ? size : 20;
    size_t i;

    printf("%s (размер %zu):", name, size);
    for (i = 0; i < shown; ++i) {
        printf(" %d", array[i]);
    }
    if (shown < size) {
        printf(" ...");
    }
    putchar('\n');
}

static StatusCode find_and_swap_min_max(int *array, size_t size,
                                        int *minimum, int *maximum)
{
    size_t minimum_index = 0;
    size_t maximum_index = 0;
    size_t i;

    if (array == NULL || minimum == NULL || maximum == NULL || size == 0) {
        return STATUS_INVALID_ARGUMENT;
    }
    for (i = 1; i < size; ++i) {
        if (array[i] < array[minimum_index]) {
            minimum_index = i;
        }
        if (array[i] > array[maximum_index]) {
            maximum_index = i;
        }
    }
    *minimum = array[minimum_index];
    *maximum = array[maximum_index];
    {
        int temporary = array[minimum_index];
        array[minimum_index] = array[maximum_index];
        array[maximum_index] = temporary;
    }
    return STATUS_OK;
}

static int *create_random_array(size_t size, int minimum, int maximum,
                                StatusCode *status)
{
    int *array;
    size_t i;

    if (status == NULL) {
        return NULL;
    }
    if (size == 0 || minimum > maximum) {
        *status = STATUS_INVALID_ARGUMENT;
        return NULL;
    }
    array = (int *)malloc(size * sizeof(*array));
    if (array == NULL) {
        *status = STATUS_MEMORY_ERROR;
        return NULL;
    }
    for (i = 0; i < size; ++i) {
        array[i] = random_in_range(minimum, maximum);
    }
    *status = STATUS_OK;
    return array;
}

static StatusCode make_array_c(const int *array_a, size_t size_a,
                               const int *array_b, size_t size_b,
                               int **array_c)
{
    int *result;
    size_t i;

    if (array_a == NULL || array_b == NULL || array_c == NULL ||
        size_a == 0 || size_b == 0) {
        return STATUS_INVALID_ARGUMENT;
    }
    result = (int *)malloc(size_a * sizeof(*result));
    if (result == NULL) {
        return STATUS_MEMORY_ERROR;
    }
    for (i = 0; i < size_a; ++i) {
        size_t j;
        int nearest = array_b[0];
        int best_distance = abs(array_a[i] - nearest);

        for (j = 1; j < size_b; ++j) {
            int distance = abs(array_a[i] - array_b[j]);
            if (distance < best_distance) {
                best_distance = distance;
                nearest = array_b[j];
            }
        }
        result[i] = array_a[i] + nearest;
    }
    *array_c = result;
    return STATUS_OK;
}

static void print_error(StatusCode status, const char *name)
{
    fprintf(stderr, "%s: ", name);
    switch (status) {
        case STATUS_INVALID_ARGUMENT:
            fprintf(stderr, "некорректные данные\n");
            break;
        case STATUS_OVERFLOW:
            fprintf(stderr, "переполнение\n");
            break;
        case STATUS_NO_CONVERGENCE:
            fprintf(stderr, "вычисление не завершилось\n");
            break;
        case STATUS_MEMORY_ERROR:
            fprintf(stderr, "недостаточно памяти\n");
            break;
        case STATUS_FILE_ERROR:
            fprintf(stderr, "ошибка файла\n");
            break;
        case STATUS_NO_ROOTS:
            fprintf(stderr, "решение не найдено\n");
            break;
        case STATUS_OK:
            break;
    }
}

int main(int argc, char **argv)
{
    const size_t fixed_size = 10;
    int lower;
    int upper;
    int fixed_array[10];
    int minimum;
    int maximum;
    size_t size_a;
    size_t size_b;
    int *array_a = NULL;
    int *array_b = NULL;
    int *array_c = NULL;
    size_t i;
    StatusCode status;

    if (argc != 3 || !parse_int(argv[1], &lower) ||
        !parse_int(argv[2], &upper) || lower > upper) {
        fprintf(stderr, "Использование: %s <a> <b>, где a <= b\n", argv[0]);
        return 1;
    }
    srand((unsigned int)time(NULL));

    for (i = 0; i < fixed_size; ++i) {
        fixed_array[i] = random_in_range(lower, upper);
    }
    print_array("Фиксированный массив до", fixed_array, fixed_size);
    status = find_and_swap_min_max(fixed_array, fixed_size, &minimum, &maximum);
    switch (status) {
        case STATUS_OK:
            break;
        default:
            print_error(status, "Фиксированный массив");
            return 1;
    }
    printf("Минимум = %d, максимум = %d\n", minimum, maximum);
    print_array("Фиксированный массив после", fixed_array, fixed_size);

    size_a = (size_t)random_in_range(10, 10000);
    size_b = (size_t)random_in_range(10, 10000);
    array_a = create_random_array(size_a, -1000, 1000, &status);
    switch (status) {
        case STATUS_OK:
            break;
        default:
            print_error(status, "Массив A");
            free(array_a);
            return 1;
    }
    array_b = create_random_array(size_b, -1000, 1000, &status);
    switch (status) {
        case STATUS_OK:
            break;
        default:
            print_error(status, "Массив B");
            free(array_a);
            free(array_b);
            return 1;
    }
    status = make_array_c(array_a, size_a, array_b, size_b, &array_c);
    switch (status) {
        case STATUS_OK:
            break;
        default:
            print_error(status, "Массив C");
            free(array_a);
            free(array_b);
            free(array_c);
            return 1;
    }

    print_array("A", array_a, size_a);
    print_array("B", array_b, size_b);
    print_array("C", array_c, size_a);
    free(array_c);
    free(array_b);
    free(array_a);
    return 0;
}
