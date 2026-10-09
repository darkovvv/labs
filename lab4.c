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

/* проверка совпадения путей к файлам в сишке */
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

/* разбор флага режима и наличия выходного файла */
static int parse_flag(const char *flag_str, char *rezhim_out, int *est_out_imya)
{
    int poz = 1;

    if (flag_str == NULL || rezhim_out == NULL || est_out_imya == NULL ||
        (flag_str[0] != '-' && flag_str[0] != '/') || flag_str[1] == '\0') {
        return 0;
    }
    *est_out_imya = 0;
    if (flag_str[poz] == 'n') {
        *est_out_imya = 1;
        ++poz;
    }
    if ((flag_str[poz] != 'd' && flag_str[poz] != 'i' &&
         flag_str[poz] != 's' && flag_str[poz] != 'a') ||
        flag_str[poz + 1] != '\0') {
        return 0;
    }
    *rezhim_out = flag_str[poz];
    return 1;
}

/* генерация имени выходного файла с префиксом out_ */
static char *make_output_name(const char *vhod_put)
{
    const char *slesh = strrchr(vhod_put, '/');
    const char *obr_slesh = strrchr(vhod_put, '\\');
    const char *imya_f;
    size_t dlina_papki;
    size_t dlina_imeni;
    char *nov_put;

    if (obr_slesh != NULL && (slesh == NULL || obr_slesh > slesh)) {
        slesh = obr_slesh;
    }
    imya_f = slesh == NULL ? vhod_put : slesh + 1;
    dlina_papki = slesh == NULL ? 0 : (size_t)(imya_f - vhod_put);
    dlina_imeni = strlen(imya_f);

    /* аллокация буфера под новый путь */
    nov_put = (char *)malloc(dlina_papki + 4 + dlina_imeni + 1);
    if (nov_put == NULL) {
        return NULL;
    }
    memcpy(nov_put, vhod_put, dlina_papki);
    memcpy(nov_put + dlina_papki, "out_", 4);
    memcpy(nov_put + dlina_papki + 4, imya_f, dlina_imeni + 1);
    return nov_put;
}

/* проверка символа на цифру */
static int is_digit(int simv)
{
    return simv >= '0' && simv <= '9';
}

/* проверка символа на латинскую букву */
static int is_latin_letter(int simv)
{
    return (simv >= 'A' && simv <= 'Z') ||
           (simv >= 'a' && simv <= 'z');
}

/* запись итога по одной строке в выходной поток */
static int write_line_result(FILE *f_out, char rezhim,
                             unsigned long kolvo_bukv, unsigned long kolvo_spec)
{
    if (rezhim == 'i') {
        return fprintf(f_out, "%lu\n", kolvo_bukv) >= 0;
    }
    if (rezhim == 's') {
        return fprintf(f_out, "%lu\n", kolvo_spec) >= 0;
    }
    return fputc('\n', f_out) != EOF;
}

/* потоковая обработка входного файла посимвольно */
static StatusCode process_file(FILE *f_in, FILE *f_out, char rezhim)
{
    unsigned long kolvo_bukv = 0;
    unsigned long kolvo_spec = 0;
    int tek_ch;
    int stroka_nachata = 0;

    while ((tek_ch = fgetc(f_in)) != EOF) {
        if (tek_ch == '\r') {
            continue;
        }
        if (tek_ch == '\n') {
            if (!write_line_result(f_out, rezhim, kolvo_bukv, kolvo_spec)) {
                return STATUS_FILE_ERROR;
            }
            kolvo_bukv = 0;
            kolvo_spec = 0;
            stroka_nachata = 0;
            continue;
        }

        stroka_nachata = 1;
        if (rezhim == 'd') {
            if (!is_digit(tek_ch) && fputc(tek_ch, f_out) == EOF) {
                return STATUS_FILE_ERROR;
            }
        } else if (rezhim == 'i') {
            if (is_latin_letter(tek_ch)) {
                ++kolvo_bukv;
            }
        } else if (rezhim == 's') {
            if (!is_latin_letter(tek_ch) && !is_digit(tek_ch) && tek_ch != ' ') {
                ++kolvo_spec;
            }
        } else {
            if (is_digit(tek_ch)) {
                if (fputc(tek_ch, f_out) == EOF) {
                    return STATUS_FILE_ERROR;
                }
            } else if (fprintf(f_out, "%02X", (unsigned int)(unsigned char)tek_ch) < 0) {
                return STATUS_FILE_ERROR;
            }
        }
    }

    if (ferror(f_in)) {
        return STATUS_FILE_ERROR;
    }
    if (stroka_nachata && !write_line_result(f_out, rezhim, kolvo_bukv, kolvo_spec)) {
        return STATUS_FILE_ERROR;
    }
    return ferror(f_out) ? STATUS_FILE_ERROR : STATUS_OK;
}

/* печать сообщения об ошибке */
static void print_error(StatusCode st_kod)
{
    switch (st_kod) {
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
    char rezhim;
    int est_out_imya;
    const char *vhod_put;
    const char *vyhod_put;
    char *sgen_vyhod = NULL;
    FILE *f_in = NULL;
    FILE *f_out = NULL;
    StatusCode st_kod;

    /* валидация аргументов запуска */
    if (argc < 2 || !parse_flag(argv[1], &rezhim, &est_out_imya)) {
        puts("Использование: lab4 <-d|-i|-s|-a> <входной_файл>\n"
             "   или: lab4 <-nd|-ni|-ns|-na> <входной_файл> <выходной_файл>");
        return 1;
    }
    if ((est_out_imya && argc != 4) || (!est_out_imya && argc != 3)) {
        print_error(STATUS_INVALID_ARGUMENT);
        return 1;
    }

    vhod_put = argv[2];
    if (est_out_imya) {
        vyhod_put = argv[3];
    } else {
        sgen_vyhod = make_output_name(vhod_put);
        if (sgen_vyhod == NULL) {
            print_error(STATUS_MEMORY_ERROR);
            return 1;
        }
        vyhod_put = sgen_vyhod;
    }

    /* защита от перезаписи входного файла */
    if (are_same_files(vhod_put, vyhod_put)) {
        fprintf(stderr, "Входной и выходной файлы не должны совпадать.\n");
        free(sgen_vyhod);
        return 1;
    }

    f_in = fopen(vhod_put, "rb");
    if (f_in == NULL) {
        fprintf(stderr, "Не удалось открыть входной файл «%s».\n", vhod_put);
        free(sgen_vyhod);
        return 1;
    }
    f_out = fopen(vyhod_put, "wb");
    if (f_out == NULL) {
        fprintf(stderr, "Не удалось открыть выходной файл «%s».\n", vyhod_put);
        fclose(f_in);
        free(sgen_vyhod);
        return 1;
    }

    st_kod = process_file(f_in, f_out, rezhim);
    if (fclose(f_out) != 0 && st_kod == STATUS_OK) {
        st_kod = STATUS_FILE_ERROR;
    }
    if (fclose(f_in) != 0 && st_kod == STATUS_OK) {
        st_kod = STATUS_FILE_ERROR;
    }
    if (st_kod != STATUS_OK) {
        print_error(st_kod);
        free(sgen_vyhod);
        return 1;
    }

    printf("Результат сохранён в файл %s\n", vyhod_put);
    free(sgen_vyhod);
    return 0;
}
