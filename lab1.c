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

static StatusCode read_number(const char *text, unsigned long long *number)
{
    unsigned long long value = 0;
    size_t i;

    if (text == NULL || number == NULL || text[0] == '\0') {
        return STATUS_INVALID_ARGUMENT;
    }
    for (i = 0; text[i] != '\0'; ++i) {
        unsigned int digit;
        if (text[i] < '0' || text[i] > '9') {
            return STATUS_INVALID_ARGUMENT;
        }
        digit = (unsigned int)(text[i] - '0');
        if (value > (ULLONG_MAX - digit) / 10) {
            return STATUS_OVERFLOW;
        }
        value = value * 10 + digit;
    }
    *number = value;
    return STATUS_OK;
}

static int is_flag(const char *text, char flag)
{
    return text != NULL && (text[0] == '-' || text[0] == '/') &&
           text[1] == flag && text[2] == '\0';
}

static int is_prime(unsigned long long number)
{
    unsigned long long divisor;

    if (number < 2) {
        return 0;
    }
    for (divisor = 2; divisor <= number / divisor; ++divisor) {
        if (number % divisor == 0) {
            return 0;
        }
    }
    return 1;
}

static StatusCode calculate_multiples(unsigned long long number,
                                      unsigned long long values[100],
                                      size_t *count)
{
    unsigned long long value;

    if (number == 0 || values == NULL || count == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    *count = 0;
    for (value = number; value <= 100; value += number) {
        values[(*count)++] = value;
        if (value > 100 - number) {
            break;
        }
    }
    return STATUS_OK;
}

static void number_to_hex(unsigned long long number,
                          char digits[sizeof(unsigned long long) * 2],
                          size_t *length)
{
    const char symbols[] = "0123456789ABCDEF";

    *length = 0;
    do {
        digits[(*length)++] = symbols[number % 16];
        number /= 16;
    } while (number != 0);
}

static StatusCode calculate_powers(unsigned long long maximum,
                                   unsigned long long values[10][10])
{
    unsigned long long base;
    unsigned long long exponent;

    if (maximum == 0 || maximum > 10 || values == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    for (base = 1; base <= 10; ++base) {
        unsigned long long value = 1;
        for (exponent = 1; exponent <= maximum; ++exponent) {
            if (value > ULLONG_MAX / base) {
                return STATUS_OVERFLOW;
            }
            value *= base;
            values[base - 1][exponent - 1] = value;
        }
    }
    return STATUS_OK;
}

static StatusCode calculate_sum(unsigned long long number,
                                unsigned long long *result)
{
    unsigned long long first = number;
    unsigned long long second;

    if (result == NULL || number == 0) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (number == ULLONG_MAX) {
        return STATUS_OVERFLOW;
    }
    second = number + 1;
    if (first % 2 == 0) {
        first /= 2;
    } else {
        second /= 2;
    }
    if (second != 0 && first > ULLONG_MAX / second) {
        return STATUS_OVERFLOW;
    }
    *result = first * second;
    return STATUS_OK;
}

static StatusCode calculate_factorial(unsigned long long number,
                                      unsigned long long *result)
{
    unsigned long long value;
    unsigned long long factorial = 1;

    if (result == NULL) {
        return STATUS_INVALID_ARGUMENT;
    }
    for (value = 2; value <= number; ++value) {
        if (factorial > ULLONG_MAX / value) {
            return STATUS_OVERFLOW;
        }
        factorial *= value;
    }
    *result = factorial;
    return STATUS_OK;
}

static void print_error(StatusCode status)
{
    switch (status) {
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
    unsigned long long number;
    StatusCode status;

    if (argc != 3) {
        fprintf(stderr, "Использование: %s <x> <-h|-p|-s|-e|-a|-f>\n", argv[0]);
        return 1;
    }

    status = read_number(argv[1], &number);
    if (status != STATUS_OK) {
        print_error(status);
        return 1;
    }

    if (is_flag(argv[2], 'h')) {
        unsigned long long values[100];
        size_t count;
        size_t i;

        status = calculate_multiples(number, values, &count);
        if (status != STATUS_OK) {
            print_error(status);
            return 1;
        }
        if (count == 0) {
            puts("Кратных чисел до 100 нет.");
        } else {
            for (i = 0; i < count; ++i) {
                printf("%s%llu", i == 0 ? "" : " ", values[i]);
            }
            putchar('\n');
        }
        return 0;
    }

    if (is_flag(argv[2], 'p')) {
        if (number < 2) {
            puts("Число не является ни простым, ни составным.");
        } else if (is_prime(number)) {
            puts("Число простое.");
        } else {
            puts("Число составное.");
        }
        return 0;
    }

    if (is_flag(argv[2], 's')) {
        char digits[sizeof(unsigned long long) * 2];
        size_t length;
        size_t i;

        number_to_hex(number, digits, &length);
        for (i = length; i > 0; --i) {
            printf("%c%s", digits[i - 1], i > 1 ? " " : "\n");
        }
        return 0;
    }

    if (is_flag(argv[2], 'e')) {
        unsigned long long powers[10][10];
        unsigned long long base;
        unsigned long long exponent;

        status = calculate_powers(number, powers);
        if (status != STATUS_OK) {
            print_error(status);
            return 1;
        }
        for (base = 1; base <= 10; ++base) {
            printf("Основание %llu:", base);
            for (exponent = 1; exponent <= number; ++exponent) {
                printf(" %llu^%llu=%llu", base, exponent,
                       powers[base - 1][exponent - 1]);
            }
            putchar('\n');
        }
        return 0;
    }

    if (is_flag(argv[2], 'a')) {
        unsigned long long result;

        status = calculate_sum(number, &result);
        if (status != STATUS_OK) {
            print_error(status);
            return 1;
        }
        printf("Сумма от 1 до %llu: %llu\n", number, result);
        return 0;
    }

    if (is_flag(argv[2], 'f')) {
        unsigned long long result;

        status = calculate_factorial(number, &result);
        if (status != STATUS_OK) {
            print_error(status);
            return 1;
        }
        printf("%llu! = %llu\n", number, result);
        return 0;
    }

    print_error(STATUS_INVALID_ARGUMENT);
    return 1;
}
