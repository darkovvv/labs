#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_INVALID_ARGUMENT,
    STATUS_OVERFLOW,
    STATUS_NO_CONVERGENCE,
    STATUS_MEMORY_ERROR,
    STATUS_FILE_ERROR,
    STATUS_NO_ROOTS
} StatusCode;

/* парсинг беззнакового числа из строки */
static StatusCode read_number(const char *str_in, unsigned long long *chislo_out)
{
    unsigned long long tek_val = 0;
    size_t i;

    /* валидация входных поинтеров */
    if (str_in == NULL || chislo_out == NULL || str_in[0] == '\0') {
        return STATUS_INVALID_ARGUMENT;
    }
    for (i = 0; str_in[i] != '\0'; ++i) {
        unsigned int cifra;
        if (str_in[i] < '0' || str_in[i] > '9') {
            return STATUS_INVALID_ARGUMENT;
        }
        cifra = (unsigned int)(str_in[i] - '0');
        /* проверка переполнения в сишке */
        if (tek_val > (ULLONG_MAX - cifra) / 10) {
            return STATUS_OVERFLOW;
        }
        tek_val = tek_val * 10 + cifra;
    }
    *chislo_out = tek_val;
    return STATUS_OK;
}

/* проверка флага командной строки */
static int is_flag(const char *arg_str, char flag_ch)
{
    return arg_str != NULL && (arg_str[0] == '-' || arg_str[0] == '/') &&
           arg_str[1] == flag_ch && arg_str[2] == '\0';
}

/* проверка числа на простоту */
static int is_prime(unsigned long long chislo)
{
    unsigned long long delitel;

    if (chislo < 2) {
        return 0;
    }
    for (delitel = 2; delitel <= chislo / delitel; ++delitel) {
        if (chislo % delitel == 0) {
            return 0;
        }
    }
    return 1;
}

/* поиск кратных чисел до ста */
static StatusCode calculate_multiples(unsigned long long chislo,
                                      unsigned long long kratn_massiv[100],
                                      size_t *kolvo_out)
{
    unsigned long long tek_kratn;

    if (chislo == 0 || kratn_massiv == NULL || kolvo_out == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    *kolvo_out = 0;
    for (tek_kratn = chislo; tek_kratn <= 100; tek_kratn += chislo) {
        kratn_massiv[(*kolvo_out)++] = tek_kratn;
        if (tek_kratn > 100 - chislo) {
            break;
        }
    }
    return STATUS_OK;
}

/* перевод числа в хекс буфер */
static void number_to_hex(unsigned long long chislo,
                          char hex_cifry[sizeof(unsigned long long) * 2],
                          size_t *dlina_out)
{
    const char alfavit[] = "0123456789ABCDEF";

    *dlina_out = 0;
    do {
        hex_cifry[(*dlina_out)++] = alfavit[chislo % 16];
        chislo /= 16;
    } while (chislo != 0);
}

/* заполнение таблицы степеней */
static StatusCode calculate_powers(unsigned long long max_stepen,
                                   unsigned long long tabl_step[10][10])
{
    unsigned long long osn;
    unsigned long long stepen;

    if (max_stepen == 0 || max_stepen > 10 || tabl_step == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    for (osn = 1; osn <= 10; ++osn) {
        unsigned long long tek_val = 1;
        for (stepen = 1; stepen <= max_stepen; ++stepen) {
            if (tek_val > ULLONG_MAX / osn) {
                return STATUS_OVERFLOW;
            }
            tek_val *= osn;
            tabl_step[osn - 1][stepen - 1] = tek_val;
        }
    }
    return STATUS_OK;
}

/* вычисление арифметической суммы от 1 до числа */
static StatusCode calculate_sum(unsigned long long chislo,
                                unsigned long long *obsh_sum_out)
{
    unsigned long long mnozh1 = chislo;
    unsigned long long mnozh2;

    if (obsh_sum_out == NULL || chislo == 0) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (chislo == ULLONG_MAX) {
        return STATUS_OVERFLOW;
    }
    mnozh2 = chislo + 1;
    if (mnozh1 % 2 == 0) {
        mnozh1 /= 2;
    } else {
        mnozh2 /= 2;
    }
    if (mnozh2 != 0 && mnozh1 > ULLONG_MAX / mnozh2) {
        return STATUS_OVERFLOW;
    }
    *obsh_sum_out = mnozh1 * mnozh2;
    return STATUS_OK;
}

/* вычисление факториала с проверкой оверфлоу */
static StatusCode calculate_factorial(unsigned long long chislo,
                                      unsigned long long *fakt_out)
{
    unsigned long long i;
    unsigned long long tek_fakt = 1;

    if (fakt_out == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    for (i = 2; i <= chislo; ++i) {
        if (tek_fakt > ULLONG_MAX / i) {
            return STATUS_OVERFLOW;
        }
        tek_fakt *= i;
    }
    *fakt_out = tek_fakt;
    return STATUS_OK;
}

/* вывод текста ошибки по статус коду */
static void print_error(StatusCode st_kod)
{
    switch (st_kod) {
        case STATUS_INVALID_ARGUMENT:
            fprintf(stderr, "Некорректный аргумент.\n");
            break;
        case STATUS_OVERFLOW:
            fprintf(stderr, "Результат не помещается в тип данных.\n");
            break;
        case STATUS_NO_CONVERGENCE:
            fprintf(stderr, "Не удалось выполнить вычисление.\n");
            break;
        case STATUS_MEMORY_ERROR:
            fprintf(stderr, "Недостаточно памяти.\n");
            break;
        case STATUS_FILE_ERROR:
            fprintf(stderr, "Ошибка работы с файлом.\n");
            break;
        case STATUS_NO_ROOTS:
            fprintf(stderr, "Корни не найдены.\n");
            break;
        case STATUS_OK:
            break;
    }
}

int main(int argc, char **argv)
{
    unsigned long long vved_chislo;
    StatusCode st_kod;

    /* валидация количества аргументов */
    if (argc != 3) {
        fprintf(stderr, "Использование: %s <x> <-h|-p|-s|-e|-a|-f>\n", argv[0]);
        return 1;
    }

    st_kod = read_number(argv[1], &vved_chislo);
    if (st_kod != STATUS_OK) {
        print_error(st_kod);
        return 1;
    }

    /* обработка флага кратных чисел */
    if (is_flag(argv[2], 'h')) {
        unsigned long long kratn_massiv[100];
        size_t kolvo;
        size_t i;

        st_kod = calculate_multiples(vved_chislo, kratn_massiv, &kolvo);
        if (st_kod != STATUS_OK) {
            print_error(st_kod);
            return 1;
        }
        if (kolvo == 0) {
            puts("Кратных чисел до 100 нет.");
        } else {
            for (i = 0; i < kolvo; ++i) {
                printf("%s%llu", i == 0 ? "" : " ", kratn_massiv[i]);
            }
            putchar('\n');
        }
        return 0;
    }

    /* обработка флага проверки на простоту */
    if (is_flag(argv[2], 'p')) {
        if (vved_chislo < 2) {
            puts("Число не является ни простым, ни составным.");
        } else if (is_prime(vved_chislo)) {
            puts("Число простое.");
        } else {
            puts("Число составное.");
        }
        return 0;
    }

    /* обработка флага вывода хекс цифр */
    if (is_flag(argv[2], 's')) {
        char hex_cifry[sizeof(unsigned long long) * 2];
        size_t dlina;
        size_t i;

        number_to_hex(vved_chislo, hex_cifry, &dlina);
        for (i = dlina; i > 0; --i) {
            printf("%c%s", hex_cifry[i - 1], i > 1 ? " " : "\n");
        }
        return 0;
    }

    /* обработка флага таблицы степеней */
    if (is_flag(argv[2], 'e')) {
        unsigned long long tabl_step[10][10];
        unsigned long long osn;
        unsigned long long stepen;

        st_kod = calculate_powers(vved_chislo, tabl_step);
        if (st_kod != STATUS_OK) {
            print_error(st_kod);
            return 1;
        }
        for (osn = 1; osn <= 10; ++osn) {
            printf("Основание %llu:", osn);
            for (stepen = 1; stepen <= vved_chislo; ++stepen) {
                printf(" %llu^%llu=%llu", osn, stepen,
                       tabl_step[osn - 1][stepen - 1]);
            }
            putchar('\n');
        }
        return 0;
    }

    /* обработка флага суммы прогрессии */
    if (is_flag(argv[2], 'a')) {
        unsigned long long obsh_sum;

        st_kod = calculate_sum(vved_chislo, &obsh_sum);
        if (st_kod != STATUS_OK) {
            print_error(st_kod);
            return 1;
        }
        printf("Сумма от 1 до %llu: %llu\n", vved_chislo, obsh_sum);
        return 0;
    }

    /* обработка флага факториала */
    if (is_flag(argv[2], 'f')) {
        unsigned long long fakt_res;

        st_kod = calculate_factorial(vved_chislo, &fakt_res);
        if (st_kod != STATUS_OK) {
            print_error(st_kod);
            return 1;
        }
        printf("%llu! = %llu\n", vved_chislo, fakt_res);
        return 0;
    }

    print_error(STATUS_INVALID_ARGUMENT);
    return 1;
}
