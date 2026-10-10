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

static int parse_flag(const char *flag, char *mode, int *has_output_name)
{
    int position = 1;

    if (flag == NULL || mode == NULL || has_output_name == NULL ||
        (flag[0] != '-' && flag[0] != '/') || flag[1] == '\0') {
        return 0;
    }
    *has_output_name = 0;
    if (flag[position] == 'n') {
        *has_output_name = 1;
        ++position;
    }
    if ((flag[position] != 'd' && flag[position] != 'i' &&
         flag[position] != 's' && flag[position] != 'a') ||
        flag[position + 1] != '\0') {
        return 0;
    }
    *mode = flag[position];
    return 1;
}

static char *make_output_name(const char *input_path)
{
    const char *separator = strrchr(input_path, '/');
    const char *backslash = strrchr(input_path, '\\');
    const char *name;
    size_t directory_length;
    size_t name_length;
    char *result;

    if (backslash != NULL && (separator == NULL || backslash > separator)) {
        separator = backslash;
    }
    name = separator == NULL ? input_path : separator + 1;
    directory_length = separator == NULL ? 0 : (size_t)(name - input_path);
    name_length = strlen(name);
    result = (char *)malloc(directory_length + 4 + name_length + 1);
    if (result == NULL) {
        return NULL;
    }
    memcpy(result, input_path, directory_length);
    memcpy(result + directory_length, "out_", 4);
    memcpy(result + directory_length + 4, name, name_length + 1);
    return result;
}

static int is_digit(int symbol)
{
    return symbol >= '0' && symbol <= '9';
}

static int is_latin_letter(int symbol)
{
    return (symbol >= 'A' && symbol <= 'Z') ||
           (symbol >= 'a' && symbol <= 'z');
}

static int write_line_result(FILE *output, char mode,
                             unsigned long letters, unsigned long special)
{
    if (mode == 'i') {
        return fprintf(output, "%lu\n", letters) >= 0;
    }
    if (mode == 's') {
        return fprintf(output, "%lu\n", special) >= 0;
    }
    return fputc('\n', output) != EOF;
}

static StatusCode process_file(FILE *input, FILE *output, char mode)
{
    unsigned long letters = 0;
    unsigned long special = 0;
    int current;
    int line_started = 0;

    while ((current = fgetc(input)) != EOF) {
        if (current == '\r') {
            continue;
        }
        if (current == '\n') {
            if (!write_line_result(output, mode, letters, special)) {
                return STATUS_FILE_ERROR;
            }
            letters = 0;
            special = 0;
            line_started = 0;
            continue;
        }

        line_started = 1;
        if (mode == 'd') {
            if (!is_digit(current) && fputc(current, output) == EOF) {
                return STATUS_FILE_ERROR;
            }
        } else if (mode == 'i') {
            if (is_latin_letter(current)) {
                ++letters;
            }
        } else if (mode == 's') {
            if (!is_latin_letter(current) && !is_digit(current) &&
                current != ' ') {
                ++special;
            }
        } else {
            if (is_digit(current)) {
                if (fputc(current, output) == EOF) {
                    return STATUS_FILE_ERROR;
                }
            } else if (fprintf(output, "%02X", (unsigned int)(unsigned char)current) < 0) {
                return STATUS_FILE_ERROR;
            }
        }
    }

    if (ferror(input)) {
        return STATUS_FILE_ERROR;
    }
    if (line_started && !write_line_result(output, mode, letters, special)) {
        return STATUS_FILE_ERROR;
    }
    return ferror(output) ? STATUS_FILE_ERROR : STATUS_OK;
}

static void print_error(StatusCode status)
{
    switch (status) {
        case STATUS_INVALID_ARGUMENT:
            fprintf(stderr, "Недопустимые аргументы.\n");
            break;
        case STATUS_OVERFLOW:
            fprintf(stderr, "Переполнение при обработке файла.\n");
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
    char mode;
    int has_output_name;
    const char *input_path;
    const char *output_path;
    char *generated_output = NULL;
    FILE *input = NULL;
    FILE *output = NULL;
    StatusCode status;

    if (argc < 2 || !parse_flag(argv[1], &mode, &has_output_name)) {
        puts("Использование: lab4 <-d|-i|-s|-a> <входной_файл>\n"
             "   или: lab4 <-nd|-ni|-ns|-na> <входной_файл> <выходной_файл>");
        return 1;
    }
    if ((has_output_name && argc != 4) || (!has_output_name && argc != 3)) {
        print_error(STATUS_INVALID_ARGUMENT);
        return 1;
    }

    input_path = argv[2];
    if (has_output_name) {
        output_path = argv[3];
    } else {
        generated_output = make_output_name(input_path);
        if (generated_output == NULL) {
            print_error(STATUS_MEMORY_ERROR);
            return 1;
        }
        output_path = generated_output;
    }

    input = fopen(input_path, "rb");
    if (input == NULL) {
        fprintf(stderr, "Не удалось открыть входной файл «%s»: %s\n",
                input_path, strerror(errno));
        free(generated_output);
        return 1;
    }
    output = fopen(output_path, "wb");
    if (output == NULL) {
        fprintf(stderr, "Не удалось открыть выходной файл «%s»: %s\n",
                output_path, strerror(errno));
        fclose(input);
        free(generated_output);
        return 1;
    }

    status = process_file(input, output, mode);
    if (fclose(output) != 0 && status == STATUS_OK) {
        status = STATUS_FILE_ERROR;
    }
    if (fclose(input) != 0 && status == STATUS_OK) {
        status = STATUS_FILE_ERROR;
    }
    if (status != STATUS_OK) {
        print_error(status);
        free(generated_output);
        return 1;
    }

    printf("Результат сохранён в файл %s\n", output_path);
    free(generated_output);
    return 0;
}
