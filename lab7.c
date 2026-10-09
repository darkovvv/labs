#include <ctype.h>
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
    TOK_MEM_ERR = -2,
    TOK_ERR = -1,
    TOK_EOF = 0,
    TOK_OK = 1
} TokStatus;

/* проверка совпадения путей к файлам */
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

/* проверка флага командной строки */
static int is_flag(const char *arg_str, char bukva)
{
    return arg_str != NULL && (arg_str[0] == '-' || arg_str[0] == '/') &&
           arg_str[1] == bukva && arg_str[2] == '\0';
}

/* чтение одной лексемы произвольной длины из файла */
static TokStatus read_token(FILE *f_in, char **tok_out)
{
    size_t emkost = 32;
    size_t dlina = 0;
    int tek_ch;
    char *tok_buf;

    if (f_in == NULL || tok_out == NULL) {
        return TOK_ERR;
    }
    *tok_out = NULL;
    /* начальная аллокация под токен в сишке */
    tok_buf = (char *)malloc(emkost);
    if (tok_buf == NULL) {
        return TOK_MEM_ERR;
    }

    do {
        tek_ch = fgetc(f_in);
        if (tek_ch == EOF) {
            free(tok_buf);
            return ferror(f_in) ? TOK_ERR : TOK_EOF;
        }
    } while (isspace((unsigned char)tek_ch));

    while (tek_ch != EOF && !isspace((unsigned char)tek_ch)) {
        if (dlina + 1 >= emkost) {
            char *nov_buf;
            emkost *= 2;
            nov_buf = (char *)realloc(tok_buf, emkost);
            if (nov_buf == NULL) {
                free(tok_buf);
                return TOK_MEM_ERR;
            }
            tok_buf = nov_buf;
        }
        tok_buf[dlina++] = (char)tek_ch;
        tek_ch = fgetc(f_in);
    }
    if (tek_ch == EOF && ferror(f_in)) {
        free(tok_buf);
        return TOK_ERR;
    }
    tok_buf[dlina] = '\0';
    *tok_out = tok_buf;
    return TOK_OK;
}

/* запись числа в заданной системе счисления в файл */
static int write_base_number(FILE *f_out, unsigned int znach, unsigned int osn_ss)
{
    const char alfavit[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    char buf_cifr[32];
    size_t dlina = 0;

    if (osn_ss < 2 || osn_ss > 36) {
        return 0;
    }
    if (znach == 0) {
        return fputc('0', f_out) != EOF;
    }
    while (znach > 0) {
        buf_cifr[dlina++] = alfavit[znach % osn_ss];
        znach /= osn_ss;
    }
    while (dlina > 0) {
        if (fputc((unsigned char)buf_cifr[--dlina], f_out) == EOF) {
            return 0;
        }
    }
    return 1;
}

/* преобразование и запись токена по его порядковому номеру */
static int write_transformed_token(FILE *f_out, const char *tok_str,
                                   unsigned long nomer_slova)
{
    size_t i;

    if (nomer_slova % 10 == 0) {
        for (i = 0; tok_str[i] != '\0'; ++i) {
            unsigned char kod_simv = (unsigned char)tolower((unsigned char)tok_str[i]);
            if (!write_base_number(f_out, kod_simv, 4)) {
                return 0;
            }
        }
        return 1;
    }
    if (nomer_slova % 5 == 0) {
        for (i = 0; tok_str[i] != '\0'; ++i) {
            if (!write_base_number(f_out, (unsigned char)tok_str[i], 8)) {
                return 0;
            }
        }
        return 1;
    }
    if (nomer_slova % 2 == 0) {
        for (i = 0; tok_str[i] != '\0'; ++i) {
            if (fputc((unsigned char)tolower((unsigned char)tok_str[i]), f_out) == EOF) {
                return 0;
            }
        }
        return 1;
    }
    return fputs(tok_str, f_out) >= 0;
}

/* обработка одного файла для флага -a */
static StatusCode run_single(FILE *f_in, FILE *f_out)
{
    unsigned long nomer_slova = 0;
    int pervyi_vyvod = 1;

    for (;;) {
        char *tek_tok = NULL;
        TokStatus st_tok = read_token(f_in, &tek_tok);

        if (st_tok == TOK_MEM_ERR) {
            return STATUS_MEMORY_ERROR;
        }
        if (st_tok == TOK_ERR) {
            return STATUS_FILE_ERROR;
        }
        if (st_tok == TOK_EOF) {
            break;
        }
        ++nomer_slova;
        if (!pervyi_vyvod && fputc(' ', f_out) == EOF) {
            free(tek_tok);
            return STATUS_FILE_ERROR;
        }
        if (!write_transformed_token(f_out, tek_tok, nomer_slova)) {
            free(tek_tok);
            return STATUS_FILE_ERROR;
        }
        pervyi_vyvod = 0;
        free(tek_tok);
    }
    return fputc('\n', f_out) == EOF ? STATUS_FILE_ERROR : STATUS_OK;
}

/* поочередное слияние лексем из двух файлов для флага -r */
static StatusCode run_alternating(FILE *f_in1, FILE *f_in2, FILE *f_out)
{
    int kon1 = 0;
    int kon2 = 0;
    int pervyi_vyvod = 1;
    unsigned long nomer_slova = 0;

    while (!kon1 || !kon2) {
        FILE *vybr_potok;
        char *tek_tok = NULL;
        TokStatus st_tok;

        if (nomer_slova % 2 == 0) {
            vybr_potok = kon1 ? f_in2 : f_in1;
        } else {
            vybr_potok = kon2 ? f_in1 : f_in2;
        }
        st_tok = read_token(vybr_potok, &tek_tok);
        if (st_tok == TOK_MEM_ERR) {
            return STATUS_MEMORY_ERROR;
        }
        if (st_tok == TOK_ERR) {
            return STATUS_FILE_ERROR;
        }
        if (st_tok == TOK_EOF) {
            if (vybr_potok == f_in1) {
                kon1 = 1;
            } else {
                kon2 = 1;
            }
            continue;
        }
        if (!pervyi_vyvod && fputc(' ', f_out) == EOF) {
            free(tek_tok);
            return STATUS_FILE_ERROR;
        }
        ++nomer_slova;
        if (fputs(tek_tok, f_out) < 0) {
            free(tek_tok);
            return STATUS_FILE_ERROR;
        }
        pervyi_vyvod = 0;
        free(tek_tok);
    }
    return fputc('\n', f_out) == EOF ? STATUS_FILE_ERROR : STATUS_OK;
}

/* вывод сообщения об ошибке */
static void print_error(StatusCode st_kod)
{
    switch (st_kod) {
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
    const char *vyhod_put;
    FILE *f_in1 = NULL;
    FILE *f_in2 = NULL;
    FILE *f_out = NULL;
    StatusCode st_kod;

    /* валидация аргументов и проверка совпадения файлов */
    if (argc == 5 && is_flag(argv[1], 'r')) {
        vyhod_put = argv[4];
        if (are_same_files(argv[2], vyhod_put) ||
            are_same_files(argv[3], vyhod_put)) {
            fprintf(stderr, "Входной и выходной файлы не должны совпадать.\n");
            return 1;
        }
        f_in1 = fopen(argv[2], "rb");
        f_in2 = fopen(argv[3], "rb");
    } else if (argc == 4 && is_flag(argv[1], 'a')) {
        vyhod_put = argv[3];
        if (are_same_files(argv[2], vyhod_put)) {
            fprintf(stderr, "Входной и выходной файлы не должны совпадать.\n");
            return 1;
        }
        f_in1 = fopen(argv[2], "rb");
    } else {
        puts("Использование: lab7 -r <файл1> <файл2> <выходной_файл>\n"
             "   или: lab7 -a <файл> <выходной_файл>");
        return 1;
    }

    if (f_in1 == NULL || (argc == 5 && f_in2 == NULL)) {
        fprintf(stderr, "Не удалось открыть входной файл.\n");
        if (f_in1 != NULL) {
            fclose(f_in1);
        }
        if (f_in2 != NULL) {
            fclose(f_in2);
        }
        return 1;
    }
    f_out = fopen(vyhod_put, "wb");
    if (f_out == NULL) {
        fprintf(stderr, "Не удалось открыть выходной файл «%s».\n", vyhod_put);
        fclose(f_in1);
        if (f_in2 != NULL) {
            fclose(f_in2);
        }
        return 1;
    }

    st_kod = argc == 5 ? run_alternating(f_in1, f_in2, f_out)
                       : run_single(f_in1, f_out);
    if (fclose(f_out) != 0 && st_kod == STATUS_OK) {
        st_kod = STATUS_FILE_ERROR;
    }
    if (fclose(f_in1) != 0 && st_kod == STATUS_OK) {
        st_kod = STATUS_FILE_ERROR;
    }
    if (f_in2 != NULL && fclose(f_in2) != 0 && st_kod == STATUS_OK) {
        st_kod = STATUS_FILE_ERROR;
    }

    if (st_kod != STATUS_OK) {
        print_error(st_kod);
        return 1;
    }
    printf("Результат сохранён в файл %s\n", vyhod_put);
    return 0;
}
