#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum {
    STATUS_OK = 0,
    STATUS_INVALID_ARGUMENT,
    STATUS_OVERFLOW,
    STATUS_NO_CONVERGENCE,
    STATUS_MEMORY_ERROR,
    STATUS_FILE_ERROR,
    STATUS_NO_ROOTS
} StatusCode;

/* парсинг вещественного числа из строки */
static int parse_double(const char *str_in, double *znach_out)
{
    char *ukaz_end;
    double chislo;

    if (str_in == NULL || str_in[0] == '\0') {
        return 0;
    }
    chislo = strtod(str_in, &ukaz_end);
    if (ukaz_end == str_in || *ukaz_end != '\0' || !isfinite(chislo)) {
        return 0;
    }
    *znach_out = chislo;
    return 1;
}

/* парсинг ненулевого целого числа */
static int parse_integer(const char *str_in, long long *znach_out)
{
    char *ukaz_end;
    long long chislo;

    if (str_in == NULL || str_in[0] == '\0') {
        return 0;
    }
    chislo = strtoll(str_in, &ukaz_end, 10);
    if (ukaz_end == str_in || *ukaz_end != '\0' ||
        chislo == LLONG_MAX || chislo == LLONG_MIN || chislo == 0) {
        return 0;
    }
    *znach_out = chislo;
    return 1;
}

/* проверка флага в аргументе */
static int is_flag(const char *arg_str, char flag_ch)
{
    return arg_str != NULL && (arg_str[0] == '-' || arg_str[0] == '/') &&
           arg_str[1] == flag_ch && arg_str[2] == '\0';
}

/* сравнение двух даблов через эпсилон */
static int equal_double(double pervoe, double vtoroe, double eps)
{
    return fabs(pervoe - vtoroe) <= eps;
}

/* решение квадратного уравнения с учетом вырожденных случаев */
static StatusCode solve_quadratic(double a, double b, double c,
                                  double eps, double korni_out[2],
                                  int *kolvo_korney_out)
{
    double diskr;

    /* валидация входных аргументов */
    if (korni_out == NULL || kolvo_korney_out == NULL || eps <= 0.0 ||
        !isfinite(a) || !isfinite(b) || !isfinite(c)) {
        return STATUS_INVALID_ARGUMENT;
    }
    *kolvo_korney_out = 0;
    if (fabs(a) <= eps) {
        if (fabs(b) <= eps) {
            if (fabs(c) <= eps) {
                /* бесконечное множество решений */
                *kolvo_korney_out = -1;
                return STATUS_OK;
            }
            return STATUS_NO_ROOTS;
        }
        korni_out[0] = -c / b;
        *kolvo_korney_out = 1;
        return STATUS_OK;
    }

    diskr = b * b - 4.0 * a * c;
    if (!isfinite(diskr)) {
        return STATUS_OVERFLOW;
    }
    if (diskr > eps) {
        double koren_d = sqrt(diskr);
        korni_out[0] = (-b - koren_d) / (2.0 * a);
        korni_out[1] = (-b + koren_d) / (2.0 * a);
        *kolvo_korney_out = 2;
    } else if (fabs(diskr) <= eps) {
        korni_out[0] = -b / (2.0 * a);
        *kolvo_korney_out = 1;
    }
    return STATUS_OK;
}

/* проверка уникальности тройки коэффициентов */
static int already_printed(double uzhe_bylo[][3], int kolvo,
                           const double tek_koeff[3], double eps)
{
    int stroka;
    int stolbec;

    for (stroka = 0; stroka < kolvo; ++stroka) {
        for (stolbec = 0; stolbec < 3; ++stolbec) {
            if (!equal_double(uzhe_bylo[stroka][stolbec], tek_koeff[stolbec], eps)) {
                break;
            }
        }
        if (stolbec == 3) {
            return 1;
        }
    }
    return 0;
}

/* вывод сообщения об ошибке */
static void print_error(StatusCode st_kod)
{
    switch (st_kod) {
        case STATUS_INVALID_ARGUMENT:
            puts("Недопустимые данные.");
            break;
        case STATUS_OVERFLOW:
            puts("Переполнение при вычислении.");
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
            puts("Действительных корней нет.");
            break;
        case STATUS_OK:
            break;
    }
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "Использование: %s -q|-m|-t ...\n", argv[0]);
        return 1;
    }

    /* перебор перестановок коэффициентов квадратного уравнения */
    if (is_flag(argv[1], 'q')) {
        const int perestanovki[6][3] = {
            {0, 1, 2}, {0, 2, 1}, {1, 0, 2},
            {1, 2, 0}, {2, 0, 1}, {2, 1, 0}
        };
        double tochnost_eps;
        double vhod_koeff[3];
        double uzhe_bylo[6][3];
        int kolvo_pech = 0;
        int i;

        if (argc != 6 || !parse_double(argv[2], &tochnost_eps) || tochnost_eps <= 0.0) {
            fprintf(stderr, "Использование: %s -q <эпсилон> <a> <b> <c>\n", argv[0]);
            return 1;
        }
        for (i = 0; i < 3; ++i) {
            if (!parse_double(argv[i + 3], &vhod_koeff[i])) {
                puts("Коэффициенты должны быть конечными действительными числами.");
                return 1;
            }
        }

        for (i = 0; i < 6; ++i) {
            double tek_koeff[3];
            double korni[2];
            int kolvo_korney = 0;
            int j;
            StatusCode st_kod;

            for (j = 0; j < 3; ++j) {
                tek_koeff[j] = vhod_koeff[perestanovki[i][j]];
            }
            if (already_printed(uzhe_bylo, kolvo_pech, tek_koeff, tochnost_eps)) {
                continue;
            }
            for (j = 0; j < 3; ++j) {
                uzhe_bylo[kolvo_pech][j] = tek_koeff[j];
            }
            ++kolvo_pech;

            printf("Уравнение %.10g*x^2%+.10g*x%+.10g = 0:\n",
                   tek_koeff[0], tek_koeff[1], tek_koeff[2]);
            st_kod = solve_quadratic(tek_koeff[0], tek_koeff[1], tek_koeff[2],
                                     tochnost_eps, korni, &kolvo_korney);
            switch (st_kod) {
                case STATUS_OK:
                    if (kolvo_korney == -1) {
                        puts("Бесконечно много решений");
                    } else if (kolvo_korney == 0) {
                        puts("Действительных корней нет");
                    } else {
                        for (j = 0; j < kolvo_korney; ++j) {
                            double tek_koren = korni[j];
                            if (fabs(tek_koren) <= tochnost_eps) {
                                tek_koren = 0.0;
                            }
                            printf("%.10g\n", tek_koren);
                        }
                    }
                    break;
                case STATUS_NO_ROOTS:
                    puts("Действительных корней нет");
                    break;
                default:
                    print_error(st_kod);
                    return 1;
            }
        }
        return 0;
    }

    /* проверка кратности двух целых чисел */
    if (is_flag(argv[1], 'm')) {
        long long pervoe_ch;
        long long vtoroe_ch;

        if (argc != 4 || !parse_integer(argv[2], &pervoe_ch) ||
            !parse_integer(argv[3], &vtoroe_ch)) {
            fprintf(stderr, "Использование: %s -m <ненулевое целое> <ненулевое целое>\n", argv[0]);
            return 1;
        }
        puts(pervoe_ch % vtoroe_ch == 0 ? "Первое число делится на второе."
                                        : "Первое число не делится на второе.");
        return 0;
    }

    /* проверка сторон прямоугольного треугольника по теореме пифагора */
    if (is_flag(argv[1], 't')) {
        double tochnost_eps;
        double storony[3];
        double sum_kvadr = 0.0;
        double max_kvadr;
        int idx_max = 0;
        int i;

        if (argc != 6 || !parse_double(argv[2], &tochnost_eps) || tochnost_eps <= 0.0) {
            fprintf(stderr, "Использование: %s -t <эпсилон> <сторона1> <сторона2> <сторона3>\n", argv[0]);
            return 1;
        }
        for (i = 0; i < 3; ++i) {
            if (!parse_double(argv[i + 3], &storony[i]) || storony[i] <= 0.0) {
                puts("Стороны треугольника должны быть положительными конечными числами.");
                return 1;
            }
            if (storony[i] > storony[idx_max]) {
                idx_max = i;
            }
        }
        for (i = 0; i < 3; ++i) {
            if (i != idx_max) {
                sum_kvadr += storony[i] * storony[i];
            }
        }
        max_kvadr = storony[idx_max] * storony[idx_max];
        puts(equal_double(sum_kvadr, max_kvadr, tochnost_eps)
                 ? "Из этих сторон можно составить прямоугольный треугольник."
                 : "Из этих сторон нельзя составить прямоугольный треугольник.");
        return 0;
    }

    puts("Неизвестный флаг. Используйте -q, -m или -t.");
    return 1;
}
