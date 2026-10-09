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

/* парсинг целого числа типа инт */
static int parse_int(const char *str_in, int *znach_out)
{
    char *ukaz_end = NULL;
    long long chislo;

    if (str_in == NULL || str_in[0] == '\0') {
        return 0;
    }
    chislo = strtoll(str_in, &ukaz_end, 10);
    if (ukaz_end == str_in || *ukaz_end != '\0' ||
        chislo < INT_MIN || chislo > INT_MAX) {
        return 0;
    }
    *znach_out = (int)chislo;
    return 1;
}

/* генерация случайного числа на отрезке */
static int random_in_range(int min_gran, int max_gran)
{
    double dolya = (double)rand() / ((double)RAND_MAX + 1.0);
    long long diapazon = (long long)max_gran - min_gran + 1;
    return (int)((long long)min_gran + (long long)(dolya * diapazon));
}

/* вывод первых элементов массива в консоль */
static void print_array(const char *imya_massiva, const int *massiv, size_t razmer)
{
    size_t pokaz_kolvo = razmer < 20 ? razmer : 20;
    size_t i;

    printf("%s (размер %zu):", imya_massiva, razmer);
    for (i = 0; i < pokaz_kolvo; ++i) {
        printf(" %d", massiv[i]);
    }
    if (pokaz_kolvo < razmer) {
        printf(" ...");
    }
    putchar('\n');
}

/* поиск и обмен минимума и максимума за один проход */
static StatusCode find_and_swap_min_max(int *massiv, size_t razmer,
                                        int *min_out, int *max_out)
{
    size_t idx_min = 0;
    size_t idx_max = 0;
    size_t i;

    if (massiv == NULL || min_out == NULL || max_out == NULL || razmer == 0) {
        return STATUS_INVALID_ARGUMENT;
    }
    for (i = 1; i < razmer; ++i) {
        if (massiv[i] < massiv[idx_min]) {
            idx_min = i;
        }
        if (massiv[i] > massiv[idx_max]) {
            idx_max = i;
        }
    }
    *min_out = massiv[idx_min];
    *max_out = massiv[idx_max];
    {
        int temp_obmen = massiv[idx_min];
        massiv[idx_min] = massiv[idx_max];
        massiv[idx_max] = temp_obmen;
    }
    return STATUS_OK;
}

/* аллокация и заполнение динамического массива случайными числами */
static int *create_random_array(size_t razmer, int min_gran, int max_gran,
                                StatusCode *st_out)
{
    int *massiv;
    size_t i;

    if (st_out == NULL) {
        return NULL;
    }
    if (razmer == 0 || min_gran > max_gran) {
        *st_out = STATUS_INVALID_ARGUMENT;
        return NULL;
    }
    massiv = (int *)malloc(razmer * sizeof(*massiv));
    if (massiv == NULL) {
        *st_out = STATUS_MEMORY_ERROR;
        return NULL;
    }
    for (i = 0; i < razmer; ++i) {
        massiv[i] = random_in_range(min_gran, max_gran);
    }
    *st_out = STATUS_OK;
    return massiv;
}

/* сборка массива c из элементов a и ближайших к ним из b */
static StatusCode make_array_c(const int *massiv_a, size_t razmer_a,
                               const int *massiv_b, size_t razmer_b,
                               int **massiv_c_out)
{
    int *nov_c;
    size_t i;

    if (massiv_a == NULL || massiv_b == NULL || massiv_c_out == NULL ||
        razmer_a == 0 || razmer_b == 0) {
        return STATUS_INVALID_ARGUMENT;
    }
    nov_c = (int *)malloc(razmer_a * sizeof(*nov_c));
    if (nov_c == NULL) {
        return STATUS_MEMORY_ERROR;
    }
    for (i = 0; i < razmer_a; ++i) {
        size_t j;
        int blizhaysh = massiv_b[0];
        int min_rasst = abs(massiv_a[i] - blizhaysh);

        for (j = 1; j < razmer_b; ++j) {
            int tek_rasst = abs(massiv_a[i] - massiv_b[j]);
            if (tek_rasst < min_rasst) {
                min_rasst = tek_rasst;
                blizhaysh = massiv_b[j];
            }
        }
        nov_c[i] = massiv_a[i] + blizhaysh;
    }
    *massiv_c_out = nov_c;
    return STATUS_OK;
}

/* печать ошибки */
static void print_error(StatusCode st_kod, const char *kontekst)
{
    fprintf(stderr, "%s: ", kontekst);
    switch (st_kod) {
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
    const size_t fiks_razmer = 10;
    int nizh_gran;
    int verh_gran;
    int fiks_massiv[10];
    int min_znach;
    int max_znach;
    size_t razmer_a;
    size_t razmer_b;
    int *massiv_a = NULL;
    int *massiv_b = NULL;
    int *massiv_c = NULL;
    size_t i;
    StatusCode st_kod;

    /* валидация границ диапазона из аргументов */
    if (argc != 3 || !parse_int(argv[1], &nizh_gran) ||
        !parse_int(argv[2], &verh_gran) || nizh_gran > verh_gran) {
        fprintf(stderr, "Использование: %s <a> <b>, где a <= b\n", argv[0]);
        return 1;
    }
    srand((unsigned int)time(NULL));

    for (i = 0; i < fiks_razmer; ++i) {
        fiks_massiv[i] = random_in_range(nizh_gran, verh_gran);
    }
    print_array("Фиксированный массив до", fiks_massiv, fiks_razmer);
    st_kod = find_and_swap_min_max(fiks_massiv, fiks_razmer, &min_znach, &max_znach);
    switch (st_kod) {
        case STATUS_OK:
            break;
        default:
            print_error(st_kod, "Фиксированный массив");
            return 1;
    }
    printf("Минимум = %d, максимум = %d\n", min_znach, max_znach);
    print_array("Фиксированный массив после", fiks_massiv, fiks_razmer);

    razmer_a = (size_t)random_in_range(10, 10000);
    razmer_b = (size_t)random_in_range(10, 10000);
    massiv_a = create_random_array(razmer_a, -1000, 1000, &st_kod);
    switch (st_kod) {
        case STATUS_OK:
            break;
        default:
            print_error(st_kod, "Массив A");
            free(massiv_a);
            return 1;
    }
    massiv_b = create_random_array(razmer_b, -1000, 1000, &st_kod);
    switch (st_kod) {
        case STATUS_OK:
            break;
        default:
            print_error(st_kod, "Массив B");
            free(massiv_a);
            free(massiv_b);
            return 1;
    }
    st_kod = make_array_c(massiv_a, razmer_a, massiv_b, razmer_b, &massiv_c);
    switch (st_kod) {
        case STATUS_OK:
            break;
        default:
            print_error(st_kod, "Массив C");
            free(massiv_a);
            free(massiv_b);
            free(massiv_c);
            return 1;
    }

    print_array("A", massiv_a, razmer_a);
    print_array("B", massiv_b, razmer_b);
    print_array("C", massiv_c, razmer_a);

    /* освобождение динамической памяти */
    free(massiv_c);
    free(massiv_b);
    free(massiv_a);
    return 0;
}
