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
    READ_MEM_ERR = -2,
    READ_ERR = -1,
    READ_EOF = 0,
    READ_OK = 1
} ReadStatus;

/* проверка совпадения входного и выходного файлов */
static int are_same_files(const char *put1, const char *put2)
{
    char poln1[4096];
    char poln2[4096];

    if (put1 == NULL || put2 == NULL) {
        return 0;
    }
    if (strcmp(put1, put2) == 0) {
        return 1;
    }
#ifdef _WIN32
    if (_fullpath(poln1, put1, sizeof(poln1)) != NULL &&
        _fullpath(poln2, put2, sizeof(poln2)) != NULL) {
        return _stricmp(poln1, poln2) == 0;
    }
#else
    if (realpath(put1, poln1) != NULL &&
        realpath(put2, poln2) != NULL) {
        return strcmp(poln1, poln2) == 0;
    }
#endif
    return 0;
}

/* получение числового значения цифры */
static int digit_value(char simv)
{
    if (simv >= '0' && simv <= '9') {
        return simv - '0';
    }
    if (simv >= 'A' && simv <= 'Z') {
        return simv - 'A' + 10;
    }
    if (simv >= 'a' && simv <= 'z') {
        return simv - 'a' + 10;
    }
    return -1;
}

/* потоковое чтение одного слова из файла */
static ReadStatus read_word(FILE *f_in, char **slovo_out)
{
    size_t emkost = 16;
    size_t dlina = 0;
    int tek_ch;
    char *buf_slova;

    if (f_in == NULL || slovo_out == NULL) {
        return READ_ERR;
    }
    *slovo_out = NULL;
    buf_slova = (char *)malloc(emkost);
    if (buf_slova == NULL) {
        return READ_MEM_ERR;
    }

    do {
        tek_ch = fgetc(f_in);
    } while (tek_ch != EOF && isspace((unsigned char)tek_ch));
    if (tek_ch == EOF) {
        free(buf_slova);
        return ferror(f_in) ? READ_ERR : READ_EOF;
    }

    while (tek_ch != EOF && !isspace((unsigned char)tek_ch)) {
        if (dlina + 1 >= emkost) {
            char *nov_buf;
            emkost *= 2;
            nov_buf = (char *)realloc(buf_slova, emkost);
            if (nov_buf == NULL) {
                free(buf_slova);
                return READ_MEM_ERR;
            }
            buf_slova = nov_buf;
        }
        buf_slova[dlina++] = (char)tek_ch;
        tek_ch = fgetc(f_in);
    }
    if (tek_ch == EOF && ferror(f_in)) {
        free(buf_slova);
        return READ_ERR;
    }
    buf_slova[dlina] = '\0';
    *slovo_out = buf_slova;
    return READ_OK;
}

/* определение минимального основания и перевод в десятичную систему */
static StatusCode convert_number(char *slovo, int *min_osn_out,
                                 unsigned long long *znach_out)
{
    size_t nach_idx = 0;
    size_t i;
    int max_cifra = 0;
    unsigned long long tek_res = 0;

    if (slovo == NULL || min_osn_out == NULL || znach_out == NULL || slovo[0] == '\0') {
        return STATUS_INVALID_ARGUMENT;
    }
    /* пропуск ведущих нулей */
    while (slovo[nach_idx] == '0' && slovo[nach_idx + 1] != '\0') {
        ++nach_idx;
    }
    for (i = nach_idx; slovo[i] != '\0'; ++i) {
        int cifra = digit_value(slovo[i]);
        if (cifra < 0) {
            return STATUS_INVALID_ARGUMENT;
        }
        if (cifra > max_cifra) {
            max_cifra = cifra;
        }
    }

    *min_osn_out = max_cifra + 1;
    if (*min_osn_out < 2) {
        *min_osn_out = 2;
    }
    for (i = nach_idx; slovo[i] != '\0'; ++i) {
        int cifra = digit_value(slovo[i]);
        if (tek_res > (ULLONG_MAX - (unsigned long long)cifra) /
                      (unsigned long long)*min_osn_out) {
            return STATUS_OVERFLOW;
        }
        tek_res = tek_res * (unsigned long long)*min_osn_out + (unsigned long long)cifra;
        if (slovo[i] >= 'a' && slovo[i] <= 'z') {
            slovo[i] = (char)toupper((unsigned char)slovo[i]);
        }
    }
    memmove(slovo, slovo + nach_idx, strlen(slovo + nach_idx) + 1);
    *znach_out = tek_res;
    return STATUS_OK;
}

/* вывод ошибки по коду возврата */
static void print_error(StatusCode st_kod)
{
    switch (st_kod) {
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
    FILE *f_in;
    FILE *f_out;
    char *tek_slovo = NULL;
    ReadStatus st_chten;
    StatusCode st_kod = STATUS_OK;

    /* проверка параметров командной строки */
    if (argc != 3) {
        fprintf(stderr, "Использование: %s <входной_файл> <выходной_файл>\n", argv[0]);
        return 1;
    }
    if (are_same_files(argv[1], argv[2])) {
        fprintf(stderr, "Входной и выходной файлы не должны совпадать.\n");
        return 1;
    }
    f_in = fopen(argv[1], "rb");
    if (f_in == NULL) {
        fprintf(stderr, "Не удалось открыть входной файл.\n");
        return 1;
    }
    f_out = fopen(argv[2], "wb");
    if (f_out == NULL) {
        fprintf(stderr, "Не удалось открыть выходной файл.\n");
        fclose(f_in);
        return 1;
    }

    while ((st_chten = read_word(f_in, &tek_slovo)) == READ_OK) {
        int min_osn;
        unsigned long long desyat_znach;
        StatusCode st_konv = convert_number(tek_slovo, &min_osn, &desyat_znach);

        switch (st_konv) {
            case STATUS_OK:
                if (fprintf(f_out, "%s %d %llu\n", tek_slovo, min_osn, desyat_znach) < 0) {
                    st_kod = STATUS_FILE_ERROR;
                }
                break;
            case STATUS_INVALID_ARGUMENT:
            case STATUS_OVERFLOW:
                if (fprintf(f_out, "%s недопустимо\n", tek_slovo) < 0) {
                    st_kod = STATUS_FILE_ERROR;
                }
                break;
            case STATUS_NO_CONVERGENCE:
            case STATUS_MEMORY_ERROR:
            case STATUS_FILE_ERROR:
            case STATUS_NO_ROOTS:
                st_kod = st_konv;
                break;
        }
        free(tek_slovo);
        tek_slovo = NULL;
        if (st_kod != STATUS_OK) {
            break;
        }
    }

    if (st_chten == READ_MEM_ERR) {
        st_kod = STATUS_MEMORY_ERROR;
    } else if (st_chten == READ_ERR) {
        st_kod = STATUS_FILE_ERROR;
    }
    if (fclose(f_in) != 0 && st_kod == STATUS_OK) {
        st_kod = STATUS_FILE_ERROR;
    }
    if (fclose(f_out) != 0 && st_kod == STATUS_OK) {
        st_kod = STATUS_FILE_ERROR;
    }

    if (st_kod != STATUS_OK) {
        free(tek_slovo);
        print_error(st_kod);
        return 1;
    }
    printf("Результат сохранён в файл %s\n", argv[2]);
    return 0;
}
