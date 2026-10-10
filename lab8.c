#include <ctype.h>
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

typedef enum {
    READ_MEMORY_ERROR = -2,
    READ_ERROR = -1,
    READ_EOF = 0,
    READ_OK = 1
} ReadStatus;

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

static ReadStatus read_word(FILE *file, char **word)
{
    size_t capacity = 16;
    size_t length = 0;
    int current;
    char *text;

    if (file == NULL || word == NULL) {
        return READ_ERROR;
    }
    *word = NULL;
    text = (char *)malloc(capacity);
    if (text == NULL) {
        return READ_MEMORY_ERROR;
    }

    do {
        current = fgetc(file);
    } while (current != EOF && isspace((unsigned char)current));
    if (current == EOF) {
        free(text);
        return ferror(file) ? READ_ERROR : READ_EOF;
    }

    while (current != EOF && !isspace((unsigned char)current)) {
        if (length + 1 >= capacity) {
            char *new_text;
            capacity *= 2;
            new_text = (char *)realloc(text, capacity);
            if (new_text == NULL) {
                free(text);
                return READ_MEMORY_ERROR;
            }
            text = new_text;
        }
        text[length++] = (char)current;
        current = fgetc(file);
    }
    if (current == EOF && ferror(file)) {
        free(text);
        return READ_ERROR;
    }
    text[length] = '\0';
    *word = text;
    return READ_OK;
}

static StatusCode convert_number(char *word, int *base,
                                 unsigned long long *value)
{
    size_t start = 0;
    size_t i;
    int max_digit = 0;
    unsigned long long result = 0;

    if (word == NULL || base == NULL || value == NULL || word[0] == '\0') {
        return STATUS_INVALID_ARGUMENT;
    }
    while (word[start] == '0' && word[start + 1] != '\0') {
        ++start;
    }
    for (i = start; word[i] != '\0'; ++i) {
        int digit = digit_value(word[i]);
        if (digit < 0) {
            return STATUS_INVALID_ARGUMENT;
        }
        if (digit > max_digit) {
            max_digit = digit;
        }
    }

    *base = max_digit + 1;
    if (*base < 2) {
        *base = 2;
    }
    for (i = start; word[i] != '\0'; ++i) {
        int digit = digit_value(word[i]);
        if (result > (ULLONG_MAX - (unsigned long long)digit) /
                    (unsigned long long)*base) {
            return STATUS_OVERFLOW;
        }
        result = result * (unsigned long long)*base + (unsigned long long)digit;
        if (word[i] >= 'a' && word[i] <= 'z') {
            word[i] = (char)toupper((unsigned char)word[i]);
        }
    }
    memmove(word, word + start, strlen(word + start) + 1);
    *value = result;
    return STATUS_OK;
}

static void print_error(StatusCode status)
{
    switch (status) {
        case STATUS_INVALID_ARGUMENT:
            fprintf(stderr, "Недопустимые данные.\n");
            break;
        case STATUS_OVERFLOW:
            fprintf(stderr, "Число не помещается в unsigned long long.\n");
            break;
        case STATUS_NO_CONVERGENCE:
            fprintf(stderr, "Обработка не завершилась.\n");
            break;
        case STATUS_MEMORY_ERROR:
            fprintf(stderr, "Недостаточно памяти.\n");
            break;
        case STATUS_FILE_ERROR:
            fprintf(stderr, "Ошибка чтения или записи файла.\n");
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
    FILE *input;
    FILE *output;
    char *word = NULL;
    ReadStatus read_status;
    StatusCode status = STATUS_OK;

    if (argc != 3) {
        fprintf(stderr, "Использование: %s <входной_файл> <выходной_файл>\n", argv[0]);
        return 1;
    }
    input = fopen(argv[1], "rb");
    if (input == NULL) {
        fprintf(stderr, "Не удалось открыть входной файл.\n");
        return 1;
    }
    output = fopen(argv[2], "wb");
    if (output == NULL) {
        fprintf(stderr, "Не удалось открыть выходной файл.\n");
        fclose(input);
        return 1;
    }

    while ((read_status = read_word(input, &word)) == READ_OK) {
        int base;
        unsigned long long value;
        StatusCode conversion = convert_number(word, &base, &value);

        switch (conversion) {
            case STATUS_OK:
                if (fprintf(output, "%s %d %llu\n", word, base, value) < 0) {
                    status = STATUS_FILE_ERROR;
                }
                break;
            case STATUS_INVALID_ARGUMENT:
            case STATUS_OVERFLOW:
                if (fprintf(output, "%s недопустимо\n", word) < 0) {
                    status = STATUS_FILE_ERROR;
                }
                break;
            case STATUS_NO_CONVERGENCE:
            case STATUS_MEMORY_ERROR:
            case STATUS_FILE_ERROR:
            case STATUS_NO_ROOTS:
                status = conversion;
                break;
        }
        free(word);
        word = NULL;
        if (status != STATUS_OK) {
            break;
        }
    }

    if (read_status == READ_MEMORY_ERROR) {
        status = STATUS_MEMORY_ERROR;
    } else if (read_status == READ_ERROR) {
        status = STATUS_FILE_ERROR;
    }
    if (fclose(input) != 0 && status == STATUS_OK) {
        status = STATUS_FILE_ERROR;
    }
    if (fclose(output) != 0 && status == STATUS_OK) {
        status = STATUS_FILE_ERROR;
    }

    if (status != STATUS_OK) {
        free(word);
        print_error(status);
        return 1;
    }
    printf("Результат сохранён в файл %s\n", argv[2]);
    return 0;
}
