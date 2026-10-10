#include <ctype.h>
#include <errno.h>
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
    TOKEN_MEMORY_ERROR = -2,
    TOKEN_ERROR = -1,
    TOKEN_EOF = 0,
    TOKEN_OK = 1
} TokenStatus;

static int is_flag(const char *text, char letter)
{
    return text != NULL && (text[0] == '-' || text[0] == '/') &&
           text[1] == letter && text[2] == '\0';
}

static TokenStatus read_token(FILE *file, char **result)
{
    size_t capacity = 32;
    size_t length = 0;
    int current;
    char *token;

    if (file == NULL || result == NULL) {
        return TOKEN_ERROR;
    }
    *result = NULL;
    token = (char *)malloc(capacity);
    if (token == NULL) {
        return TOKEN_MEMORY_ERROR;
    }

    do {
        current = fgetc(file);
        if (current == EOF) {
            free(token);
            return ferror(file) ? TOKEN_ERROR : TOKEN_EOF;
        }
    } while (isspace((unsigned char)current));

    while (current != EOF && !isspace((unsigned char)current)) {
        if (length + 1 >= capacity) {
            char *resized;
            capacity *= 2;
            resized = (char *)realloc(token, capacity);
            if (resized == NULL) {
                free(token);
                return TOKEN_MEMORY_ERROR;
            }
            token = resized;
        }
        token[length++] = (char)current;
        current = fgetc(file);
    }
    if (current == EOF && ferror(file)) {
        free(token);
        return TOKEN_ERROR;
    }
    token[length] = '\0';
    *result = token;
    return TOKEN_OK;
}

static int write_base_number(FILE *output, unsigned int value,
                             unsigned int base)
{
    const char digits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    char buffer[32];
    size_t length = 0;

    if (base < 2 || base > 36) {
        return 0;
    }
    if (value == 0) {
        return fputc('0', output) != EOF;
    }
    while (value > 0) {
        buffer[length++] = digits[value % base];
        value /= base;
    }
    while (length > 0) {
        if (fputc((unsigned char)buffer[--length], output) == EOF) {
            return 0;
        }
    }
    return 1;
}

static int write_transformed_token(FILE *output, const char *token,
                                   unsigned long position)
{
    size_t i;

    if (position % 10 == 0) {
        for (i = 0; token[i] != '\0'; ++i) {
            unsigned char symbol = (unsigned char)tolower((unsigned char)token[i]);
            if (!write_base_number(output, symbol, 4)) {
                return 0;
            }
        }
        return 1;
    }
    if (position % 5 == 0) {
        for (i = 0; token[i] != '\0'; ++i) {
            if (!write_base_number(output, (unsigned char)token[i], 8)) {
                return 0;
            }
        }
        return 1;
    }
    if (position % 2 == 0) {
        for (i = 0; token[i] != '\0'; ++i) {
            if (fputc((unsigned char)tolower((unsigned char)token[i]), output) == EOF) {
                return 0;
            }
        }
        return 1;
    }
    return fputs(token, output) >= 0;
}

static StatusCode run_single(FILE *input, FILE *output)
{
    unsigned long position = 0;
    int first_output = 1;

    for (;;) {
        char *token = NULL;
        TokenStatus status = read_token(input, &token);

        if (status == TOKEN_MEMORY_ERROR) {
            return STATUS_MEMORY_ERROR;
        }
        if (status == TOKEN_ERROR) {
            return STATUS_FILE_ERROR;
        }
        if (status == TOKEN_EOF) {
            break;
        }
        ++position;
        if (!first_output && fputc(' ', output) == EOF) {
            free(token);
            return STATUS_FILE_ERROR;
        }
        if (!write_transformed_token(output, token, position)) {
            free(token);
            return STATUS_FILE_ERROR;
        }
        first_output = 0;
        free(token);
    }
    return fputc('\n', output) == EOF ? STATUS_FILE_ERROR : STATUS_OK;
}

static StatusCode run_alternating(FILE *file1, FILE *file2, FILE *output)
{
    int first_finished = 0;
    int second_finished = 0;
    int first_output = 1;
    unsigned long position = 0;

    while (!first_finished || !second_finished) {
        FILE *selected;
        char *token = NULL;
        TokenStatus status;

        if (position % 2 == 0) {
            selected = first_finished ? file2 : file1;
        } else {
            selected = second_finished ? file1 : file2;
        }
        status = read_token(selected, &token);
        if (status == TOKEN_MEMORY_ERROR) {
            return STATUS_MEMORY_ERROR;
        }
        if (status == TOKEN_ERROR) {
            return STATUS_FILE_ERROR;
        }
        if (status == TOKEN_EOF) {
            if (selected == file1) {
                first_finished = 1;
            } else {
                second_finished = 1;
            }
            continue;
        }
        if (!first_output && fputc(' ', output) == EOF) {
            free(token);
            return STATUS_FILE_ERROR;
        }
        ++position;
        if (fputs(token, output) < 0) {
            free(token);
            return STATUS_FILE_ERROR;
        }
        first_output = 0;
        free(token);
    }
    return fputc('\n', output) == EOF ? STATUS_FILE_ERROR : STATUS_OK;
}

static void print_error(StatusCode status)
{
    switch (status) {
        case STATUS_INVALID_ARGUMENT:
            fprintf(stderr, "Некорректные аргументы.\n");
            break;
        case STATUS_OVERFLOW:
            fprintf(stderr, "Слишком большой файл или токен.\n");
            break;
        case STATUS_NO_CONVERGENCE:
            fprintf(stderr, "Обработка не завершилась.\n");
            break;
        case STATUS_MEMORY_ERROR:
            fprintf(stderr, "Недостаточно памяти.\n");
            break;
        case STATUS_FILE_ERROR:
            fprintf(stderr, "Ошибка чтения или записи файлов.\n");
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
    const char *output_path;
    FILE *file1 = NULL;
    FILE *file2 = NULL;
    FILE *output = NULL;
    StatusCode status;

    if (argc == 5 && is_flag(argv[1], 'r')) {
        file1 = fopen(argv[2], "rb");
        file2 = fopen(argv[3], "rb");
        output_path = argv[4];
    } else if (argc == 4 && is_flag(argv[1], 'a')) {
        file1 = fopen(argv[2], "rb");
        output_path = argv[3];
    } else {
        puts("Использование: lab7 -r <файл1> <файл2> <выходной_файл>\n"
             "   или: lab7 -a <файл> <выходной_файл>");
        return 1;
    }

    if (file1 == NULL || (argc == 5 && file2 == NULL)) {
        fprintf(stderr, "Не удалось открыть входной файл: %s\n", strerror(errno));
        if (file1 != NULL) {
            fclose(file1);
        }
        if (file2 != NULL) {
            fclose(file2);
        }
        return 1;
    }
    output = fopen(output_path, "wb");
    if (output == NULL) {
        fprintf(stderr, "Не удалось открыть выходной файл «%s»: %s\n",
                output_path, strerror(errno));
        fclose(file1);
        if (file2 != NULL) {
            fclose(file2);
        }
        return 1;
    }

    status = argc == 5 ? run_alternating(file1, file2, output)
                       : run_single(file1, output);
    if (fclose(output) != 0 && status == STATUS_OK) {
        status = STATUS_FILE_ERROR;
    }
    if (fclose(file1) != 0 && status == STATUS_OK) {
        status = STATUS_FILE_ERROR;
    }
    if (file2 != NULL && fclose(file2) != 0 && status == STATUS_OK) {
        status = STATUS_FILE_ERROR;
    }

    if (status != STATUS_OK) {
        print_error(status);
        return 1;
    }
    printf("Результат сохранён в файл %s\n", output_path);
    return 0;
}
