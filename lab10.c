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

/* перевод символа в значение цифры */
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

/* парсинг знакового числа в заданной системе счисления */
static StatusCode read_number(const char *str_in, int osn_ss, long long *chislo_out)
{
    int otric_flag = 0;
    unsigned long long tek_val = 0;
    unsigned long long predel;

    if (str_in == NULL || chislo_out == NULL || osn_ss < 2 || osn_ss > 36) {
        return STATUS_INVALID_ARGUMENT;
    }
    if (*str_in == '+' || *str_in == '-') {
        otric_flag = *str_in == '-';
        ++str_in;
    }
    if (*str_in == '\0') {
        return STATUS_INVALID_ARGUMENT;
    }

    predel = otric_flag ? (unsigned long long)LLONG_MAX + 1
                        : (unsigned long long)LLONG_MAX;
    while (*str_in != '\0') {
        int cifra = digit_value(*str_in);
        if (cifra < 0 || cifra >= osn_ss) {
            return STATUS_INVALID_ARGUMENT;
        }
        if (tek_val > (predel - (unsigned long long)cifra) /
                      (unsigned long long)osn_ss) {
            return STATUS_OVERFLOW;
        }
        tek_val = tek_val * (unsigned long long)osn_ss + (unsigned long long)cifra;
        ++str_in;
    }

    if (otric_flag) {
        if (tek_val == (unsigned long long)LLONG_MAX + 1) {
            *chislo_out = LLONG_MIN;
        } else {
            *chislo_out = -(long long)tek_val;
        }
    } else {
        *chislo_out = (long long)tek_val;
    }
    return STATUS_OK;
}

/* вычисление модуля знакового числа без переполнения */
static unsigned long long absolute_value(long long chislo)
{
    if (chislo >= 0) {
        return (unsigned long long)chislo;
    }
    if (chislo == LLONG_MIN) {
        return (unsigned long long)LLONG_MAX + 1;
    }
    return (unsigned long long)(-chislo);
}

/* перевод числа в буфер цифр заданного основания */
static StatusCode convert_to_base(long long chislo, int osn_ss,
                                  char cifry_out[70], size_t *dlina_out)
{
    const char alfavit[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    unsigned long long mod_znach;
    size_t kolvo = 0;

    if (cifry_out == NULL || dlina_out == NULL || osn_ss < 2 || osn_ss > 36) {
        return STATUS_INVALID_ARGUMENT;
    }
    mod_znach = absolute_value(chislo);
    do {
        cifry_out[kolvo++] = alfavit[mod_znach % (unsigned long long)osn_ss];
        mod_znach /= (unsigned long long)osn_ss;
    } while (mod_znach > 0);
    if (chislo < 0) {
        cifry_out[kolvo++] = '-';
    }
    *dlina_out = kolvo;
    return STATUS_OK;
}

/* печать перевернутого буфера цифр */
static void print_in_base(const char cifry_buf[70], size_t dlina)
{
    while (dlina > 0) {
        putchar(cifry_buf[--dlina]);
    }
    putchar('\n');
}

/* вывод сообщения об ошибке */
static void print_error(StatusCode st_kod)
{
    switch (st_kod) {
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
    char stroka_vvoda[512];
    char *ukaz_end;
    long vved_osn;
    int osn_ss;
    int byli_chisla = 0;
    long long tek_chislo;
    long long max_po_mod = 0;
    long long obsh_sum = 0;
    int sum_overflow = 0;
    unsigned long long max_abs_znach = 0;
    const int vyhod_bazy[] = {9, 18, 27, 36};
    int i;

    /* чтение и валидация основания системы счисления */
    printf("Введите основание системы счисления (2..36): ");
    if (fgets(stroka_vvoda, sizeof(stroka_vvoda), stdin) == NULL) {
        puts("Ошибка ввода.");
        return 1;
    }
    vved_osn = strtol(stroka_vvoda, &ukaz_end, 10);
    while (isspace((unsigned char)*ukaz_end)) {
        ++ukaz_end;
    }
    if (ukaz_end == stroka_vvoda || *ukaz_end != '\0' ||
        vved_osn < 2 || vved_osn > 36) {
        puts("Недопустимое основание системы счисления.");
        return 1;
    }
    osn_ss = (int)vved_osn;

    /* цикл ввода чисел до слова стоп */
    puts("Введите числа по одному в строке. Для завершения введите Stop.");
    while (fgets(stroka_vvoda, sizeof(stroka_vvoda), stdin) != NULL) {
        size_t dlina = strlen(stroka_vvoda);
        char *ochish_str;
        StatusCode st_kod;

        while (dlina > 0 && isspace((unsigned char)stroka_vvoda[dlina - 1])) {
            stroka_vvoda[--dlina] = '\0';
        }
        ochish_str = stroka_vvoda;
        while (isspace((unsigned char)*ochish_str)) {
            ++ochish_str;
        }
        if (strcmp(ochish_str, "Stop") == 0) {
            break;
        }

        st_kod = read_number(ochish_str, osn_ss, &tek_chislo);
        switch (st_kod) {
            case STATUS_OK:
                break;
            case STATUS_INVALID_ARGUMENT:
                puts("Недопустимое целое число; попробуйте ещё раз.");
                continue;
            case STATUS_OVERFLOW:
                puts("Число выходит за диапазон long long.");
                continue;
            default:
                print_error(st_kod);
                continue;
        }

        if (!sum_overflow &&
            ((tek_chislo > 0 && obsh_sum > LLONG_MAX - tek_chislo) ||
             (tek_chislo < 0 && obsh_sum < LLONG_MIN - tek_chislo))) {
            puts("Переполнение суммы: сумму всех введённых чисел невозможно представить.");
            sum_overflow = 1;
        } else if (!sum_overflow) {
            obsh_sum += tek_chislo;
        }
        if (!byli_chisla || absolute_value(tek_chislo) > max_abs_znach) {
            max_po_mod = tek_chislo;
            max_abs_znach = absolute_value(tek_chislo);
            byli_chisla = 1;
        }
    }

    if (!byli_chisla) {
        puts("Числа не введены.");
        return 0;
    }
    printf("Максимальное по модулю число: %lld\n", max_po_mod);
    if (sum_overflow) {
        puts("Сумма недоступна из-за переполнения.");
        return 1;
    }
    printf("Сумма: %lld\n", obsh_sum);

    /* вывод результата в системах 9, 18, 27, 36 */
    for (i = 0; i < 4; ++i) {
        char buf_konv[70];
        size_t dlina_konv;
        StatusCode st_kod;

        st_kod = convert_to_base(max_po_mod, vyhod_bazy[i],
                                 buf_konv, &dlina_konv);
        switch (st_kod) {
            case STATUS_OK:
                printf("Максимальное число в системе счисления с основанием %d: ",
                       vyhod_bazy[i]);
                print_in_base(buf_konv, dlina_konv);
                break;
            default:
                print_error(st_kod);
                return 1;
        }
        st_kod = convert_to_base(obsh_sum, vyhod_bazy[i],
                                 buf_konv, &dlina_konv);
        switch (st_kod) {
            case STATUS_OK:
                printf("Сумма в системе счисления с основанием %d: ",
                       vyhod_bazy[i]);
                print_in_base(buf_konv, dlina_konv);
                break;
            default:
                print_error(st_kod);
                return 1;
        }
    }
    return 0;
}
