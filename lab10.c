#include <ctype.h>
#include <errno.h>
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

static int digit_value(char symbol)
{
    if (symbol >= '0' && symbol <= '9') {
        return symbol - '0';
    }
    if (symbol >= 'A' && symbol <= 'Z') {
        return symbol - 'A' + 10;
    }
    if (symbol >= 'a' && symbol <= 'z') {
        return symbol - 'a' + 10;
    }
    return -1;
}

static StatusCode read_number(const char *text, int base, long long *number)
{
    int negative = 0;
    unsigned long long value = 0;
    unsigned long long limit;

    if (text == NULL || number == NULL || base < 2 || base > 36) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (*text == '+' || *text == '-') {
        negative = *text == '-';
        ++text;
    }
    if (*text == '\0') {
        return STATUS_INVALID_ARGUMENT;
    }

    limit = negative ? (unsigned long long)LLONG_MAX + 1
                     : (unsigned long long)LLONG_MAX;
    while (*text != '\0') {
        int digit = digit_value(*text);
        if (digit < 0 || digit >= base) {
            return STATUS_INVALID_ARGUMENT;
        }
        if (value > (limit - (unsigned long long)digit) /
                    (unsigned long long)base) {
            return STATUS_OVERFLOW;
        }
        value = value * (unsigned long long)base + (unsigned long long)digit;
        ++text;
    }

    if (negative) {
        if (value == (unsigned long long)LLONG_MAX + 1) {
            *number = LLONG_MIN;
        } else {
            *number = -(long long)value;
        }
    } else {
        *number = (long long)value;
    }
    return STATUS_OK;
}

static unsigned long long absolute_value(long long number)
{
    if (number >= 0) {
        return (unsigned long long)number;
    }
    if (number == LLONG_MIN) {
        return (unsigned long long)LLONG_MAX + 1;
    }
    return (unsigned long long)(-number);
}

static StatusCode convert_to_base(long long number, int base,
                                  char digits[70], size_t *length)
{
    const char symbols[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    unsigned long long value;
    size_t count = 0;

    if (digits == NULL || length == NULL || base < 2 || base > 36) {
        return STATUS_INVALID_ARGUMENT;
    }
    value = absolute_value(number);
    do {
        digits[count++] = symbols[value % (unsigned long long)base];
        value /= (unsigned long long)base;
    } while (value > 0);
    if (number < 0) {
        digits[count++] = '-';
    }
    *length = count;
    return STATUS_OK;
}

static void print_in_base(const char digits[70], size_t length)
{
    while (length > 0) {
        putchar(digits[--length]);
    }
    putchar('\n');
}

static void print_error(StatusCode status)
{
    switch (status) {
        case STATUS_INVALID_ARGUMENT:
            puts("Недопустимые данные.");
            break;
        case STATUS_OVERFLOW:
            puts("Переполнение.");
            break;
        case STATUS_NO_CONVERGENCE:
            puts("Вычисление не завершилось.");
            break;
        case STATUS_MEMORY_ERROR:
            puts("Недостаточно памяти.");
            break;
        case STATUS_FILE_ERROR:
            puts("Ошибка файла.");
            break;
        case STATUS_NO_ROOTS:
            puts("Решение не найдено.");
            break;
        case STATUS_OK:
            break;
    }
}

int main(void)
{
    char line[512];
    char *end;
    long base_value;
    int base;
    int has_number = 0;
    long long number;
    long long maximum = 0;
    long long sum = 0;
    int sum_overflowed = 0;
    unsigned long long max_absolute = 0;
    const int output_bases[] = {9, 18, 27, 36};
    int i;

    printf("Введите основание системы счисления (2..36): ");
    if (fgets(line, sizeof(line), stdin) == NULL) {
        puts("Ошибка ввода.");
        return 1;
    }
    errno = 0;
    base_value = strtol(line, &end, 10);
    while (isspace((unsigned char)*end)) {
        ++end;
    }
    if (errno == ERANGE || end == line || *end != '\0' ||
        base_value < 2 || base_value > 36) {
        puts("Недопустимое основание системы счисления.");
        return 1;
    }
    base = (int)base_value;

    puts("Введите числа по одному в строке. Для завершения введите Stop.");
    while (fgets(line, sizeof(line), stdin) != NULL) {
        size_t length = strlen(line);
        char *text;
        StatusCode status;

        while (length > 0 && isspace((unsigned char)line[length - 1])) {
            line[--length] = '\0';
        }
        text = line;
        while (isspace((unsigned char)*text)) {
            ++text;
        }
        if (strcmp(text, "Stop") == 0) {
            break;
        }

        status = read_number(text, base, &number);
        switch (status) {
            case STATUS_OK:
                break;
            case STATUS_INVALID_ARGUMENT:
                puts("Недопустимое целое число; попробуйте ещё раз.");
                continue;
            case STATUS_OVERFLOW:
                puts("Число выходит за диапазон long long.");
                continue;
            default:
                print_error(status);
                continue;
        }

        if (!sum_overflowed &&
            ((number > 0 && sum > LLONG_MAX - number) ||
             (number < 0 && sum < LLONG_MIN - number))) {
            puts("Переполнение суммы: сумму всех введённых чисел невозможно представить.");
            sum_overflowed = 1;
        } else if (!sum_overflowed) {
            sum += number;
        }
        if (!has_number || absolute_value(number) > max_absolute) {
            maximum = number;
            max_absolute = absolute_value(number);
            has_number = 1;
        }
    }

    if (!has_number) {
        puts("Числа не введены.");
        return 0;
    }
    printf("Максимальное по модулю число: %lld\n", maximum);
    if (sum_overflowed) {
        puts("Сумма недоступна из-за переполнения.");
        return 1;
    }
    printf("Сумма: %lld\n", sum);

    for (i = 0; i < 4; ++i) {
        char converted[70];
        size_t converted_length;
        StatusCode status;

        status = convert_to_base(maximum, output_bases[i],
                                 converted, &converted_length);
        switch (status) {
            case STATUS_OK:
                printf("Максимальное число в системе счисления с основанием %d: ",
                       output_bases[i]);
                print_in_base(converted, converted_length);
                break;
            default:
                print_error(status);
                return 1;
        }
        status = convert_to_base(sum, output_bases[i],
                                 converted, &converted_length);
        switch (status) {
            case STATUS_OK:
                printf("Сумма в системе счисления с основанием %d: ",
                       output_bases[i]);
                print_in_base(converted, converted_length);
                break;
            default:
                print_error(status);
                return 1;
        }
    }
    return 0;
}
