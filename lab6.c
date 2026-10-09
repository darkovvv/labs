#include <limits.h>
#include <math.h>
#include <stdarg.h>
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

/* вычисление векторного произведения для тройки точек */
static double cross_product(double ax, double ay, double bx, double by,
                            double cx, double cy)
{
    return (bx - ax) * (cy - ay) - (by - ay) * (cx - ax);
}

/* проверка выпуклости многоугольника по обходу вершин */
static int is_convex(double eps, int kolvo_vershin, ...)
{
    double koord_x[100];
    double koord_y[100];
    va_list args;
    int znak_povorota = 0;
    int i;

    if (!isfinite(eps) || eps <= 0.0 || kolvo_vershin < 3 || kolvo_vershin > 100) {
        return -1;
    }
    va_start(args, kolvo_vershin);
    for (i = 0; i < kolvo_vershin; ++i) {
        koord_x[i] = va_arg(args, double);
        koord_y[i] = va_arg(args, double);
        if (!isfinite(koord_x[i]) || !isfinite(koord_y[i])) {
            va_end(args);
            return -1;
        }
    }
    va_end(args);

    /* проверка сохранения знака векторного произведения */
    for (i = 0; i < kolvo_vershin; ++i) {
        int sled1 = (i + 1) % kolvo_vershin;
        int sled2 = (i + 2) % kolvo_vershin;
        double vekt_pr = cross_product(koord_x[i], koord_y[i],
                                       koord_x[sled1], koord_y[sled1],
                                       koord_x[sled2], koord_y[sled2]);

        if (vekt_pr > eps) {
            if (znak_povorota < 0) {
                return 0;
            }
            znak_povorota = 1;
        } else if (vekt_pr < -eps) {
            if (znak_povorota > 0) {
                return 0;
            }
            znak_povorota = -1;
        }
    }
    return znak_povorota != 0;
}

/* вычисление многочлена в точке по схеме горнера */
static int polynomial(double tochka_x, int stepen, double *otv_out, ...)
{
    va_list args;
    double tek_val;
    int i;

    if (otv_out == NULL || !isfinite(tochka_x) || stepen < 0 || stepen > 100) {
        return 0;
    }
    va_start(args, otv_out);
    tek_val = va_arg(args, double);
    for (i = stepen - 1; i >= 0; --i) {
        tek_val = tek_val * tochka_x + va_arg(args, double);
    }
    va_end(args);
    if (!isfinite(tek_val)) {
        return 0;
    }
    *otv_out = tek_val;
    return 1;
}

/* перевод символа цифры в числовое значение */
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

/* парсинг числа в заданной системе счисления */
static int number_from_string(const char *str_in, int osn_ss,
                              unsigned long long *chislo_out)
{
    unsigned long long tek_val = 0;
    size_t i;

    if (str_in == NULL || str_in[0] == '\0' || chislo_out == NULL ||
        osn_ss < 2 || osn_ss > 36) {
        return 0;
    }
    for (i = 0; str_in[i] != '\0'; ++i) {
        int cifra = digit_value(str_in[i]);
        if (cifra < 0 || cifra >= osn_ss ||
            tek_val > (ULLONG_MAX - (unsigned long long)cifra) /
                      (unsigned long long)osn_ss) {
            return 0;
        }
        tek_val = tek_val * (unsigned long long)osn_ss + (unsigned long long)cifra;
    }
    *chislo_out = tek_val;
    return 1;
}

/* проверка строки на число капрекара */
static int is_kaprekar(const char *str_in, int osn_ss)
{
    unsigned long long chislo;
    unsigned long long ishodn_ch;
    unsigned long long kvadrat;
    unsigned long long delitel = 1;
    int dlina_ch = 0;
    int i;

    if (!number_from_string(str_in, osn_ss, &chislo)) {
        return -1;
    }
    if (chislo != 0 && chislo > ULLONG_MAX / chislo) {
        return -1;
    }
    ishodn_ch = chislo;
    kvadrat = chislo * chislo;
    do {
        ++dlina_ch;
        chislo /= (unsigned)osn_ss;
    } while (chislo != 0);

    for (i = 0; i < dlina_ch; ++i) {
        if (delitel > ULLONG_MAX / (unsigned)osn_ss) {
            return -1;
        }
        delitel *= (unsigned)osn_ss;
    }
    return kvadrat / delitel + kvadrat % delitel == ishodn_ch;
}

/* отбор чисел капрекара из переменного списка строк */
static int find_kaprekar(int osn_ss, const char **nayd_out, int kolvo_str, ...)
{
    va_list args;
    int kolvo_nayd = 0;
    int i;

    if (osn_ss < 2 || osn_ss > 36 || kolvo_str < 0 || nayd_out == NULL) {
        return -1;
    }
    va_start(args, kolvo_str);
    for (i = 0; i < kolvo_str; ++i) {
        const char *tek_str = va_arg(args, const char *);
        int flag_kapr = is_kaprekar(tek_str, osn_ss);
        if (flag_kapr < 0) {
            va_end(args);
            return -1;
        }
        if (flag_kapr) {
            nayd_out[kolvo_nayd++] = tek_str;
        }
    }
    va_end(args);
    return kolvo_nayd;
}

/* вычисление среднего геометрического */
static int geometric_mean(double *otv_out, int kolvo_chisel, ...)
{
    va_list args;
    double proizv = 1.0;
    int i;

    if (otv_out == NULL || kolvo_chisel <= 0) {
        return 0;
    }
    va_start(args, kolvo_chisel);
    for (i = 0; i < kolvo_chisel; ++i) {
        double tek_ch = va_arg(args, double);
        if (!isfinite(tek_ch) || tek_ch <= 0.0) {
            va_end(args);
            return 0;
        }
        proizv *= tek_ch;
        if (!isfinite(proizv)) {
            va_end(args);
            return 0;
        }
    }
    va_end(args);
    *otv_out = pow(proizv, 1.0 / kolvo_chisel);
    return isfinite(*otv_out);
}

/* рекурсивное быстрое возведение в неотрицательную степень */
static int fast_power_positive(double osn, unsigned long long stepen,
                               double *otv_out)
{
    double polovina;

    if (otv_out == NULL) {
        return 0;
    }
    if (stepen == 0) {
        *otv_out = 1.0;
        return 1;
    }
    if (!fast_power_positive(osn, stepen / 2, &polovina)) {
        return 0;
    }
    *otv_out = polovina * polovina;
    if (!isfinite(*otv_out)) {
        return 0;
    }
    if (stepen % 2 == 1) {
        *otv_out *= osn;
    }
    return isfinite(*otv_out);
}

/* быстрое возведение в целую степень со знаком */
static int fast_power(double osn, long long stepen, double *otv_out)
{
    unsigned long long polozh_stepen;
    double znach;

    if (otv_out == NULL || !isfinite(osn) || (osn == 0.0 && stepen < 0)) {
        return 0;
    }
    if (stepen < 0) {
        if (stepen == LLONG_MIN) {
            polozh_stepen = (unsigned long long)LLONG_MAX + 1;
        } else {
            polozh_stepen = (unsigned long long)(-stepen);
        }
    } else {
        polozh_stepen = (unsigned long long)stepen;
    }
    if (!fast_power_positive(osn, polozh_stepen, &znach)) {
        return 0;
    }
    *otv_out = stepen < 0 ? 1.0 / znach : znach;
    return isfinite(*otv_out);
}

typedef double (*UravnFunc)(double);

/* поиск корня на отрезке методом дихотомии */
static StatusCode bisection(double lev_gran, double prav_gran, double eps,
                            UravnFunc func, double *koren_out)
{
    double znach_lev;
    double znach_prav;
    int i;

    if (koren_out == NULL || func == NULL || !isfinite(lev_gran) ||
        !isfinite(prav_gran) || !isfinite(eps) || eps <= 0.0 ||
        lev_gran >= prav_gran) {
        return STATUS_INVALID_ARGUMENT;
    }
    znach_lev = func(lev_gran);
    znach_prav = func(prav_gran);
    if (!isfinite(znach_lev) || !isfinite(znach_prav)) {
        return STATUS_NO_CONVERGENCE;
    }
    if ((znach_lev > 0.0 && znach_prav > 0.0) ||
        (znach_lev < 0.0 && znach_prav < 0.0)) {
        return STATUS_NO_ROOTS;
    }

    for (i = 0; i < 100000; ++i) {
        double sered = lev_gran + (prav_gran - lev_gran) / 2.0;
        double znach_ser = func(sered);

        if (!isfinite(znach_ser)) {
            return STATUS_NO_CONVERGENCE;
        }
        if (fabs(znach_ser) <= eps || prav_gran - lev_gran <= eps) {
            *koren_out = sered;
            return STATUS_OK;
        }
        if ((znach_lev <= 0.0 && znach_ser >= 0.0) ||
            (znach_lev >= 0.0 && znach_ser <= 0.0)) {
            prav_gran = sered;
        } else {
            lev_gran = sered;
            znach_lev = znach_ser;
        }
    }
    return STATUS_NO_CONVERGENCE;
}

/* тестовые функции для дихотомии */
static double equation_square(double x)
{
    return x * x - 2.0;
}

static double equation_cosine(double x)
{
    return cos(x) - x;
}

static double equation_exponential(double x)
{
    return exp(x) - 3.0;
}

/* вывод результата подзадачи */
static void print_status(const char *nazv, StatusCode st_kod, double znach)
{
    switch (st_kod) {
        case STATUS_OK:
            printf("%s: %.10g\n", nazv, znach);
            break;
        case STATUS_INVALID_ARGUMENT:
            printf("%s: неверные данные\n", nazv);
            break;
        case STATUS_OVERFLOW:
            printf("%s: переполнение\n", nazv);
            break;
        case STATUS_NO_CONVERGENCE:
            printf("%s: вычисление не сошлось\n", nazv);
            break;
        case STATUS_MEMORY_ERROR:
            printf("%s: недостаточно памяти\n", nazv);
            break;
        case STATUS_FILE_ERROR:
            printf("%s: ошибка файла\n", nazv);
            break;
        case STATUS_NO_ROOTS:
            printf("%s: корень не найден\n", nazv);
            break;
    }
}

int main(void)
{
    double otv = 0.0;
    int vypukl_flag;
    const char *kapr_spisok[8];
    int kolvo_kapr;
    StatusCode st_kod;

    vypukl_flag = is_convex(1e-9, 4,
                            0.0, 0.0, 4.0, 0.0,
                            4.0, 3.0, 0.0, 3.0);
    if (vypukl_flag < 0) {
        print_status("1. Многоугольник", STATUS_INVALID_ARGUMENT, 0.0);
    } else {
        puts(vypukl_flag ? "1. Многоугольник выпуклый"
                         : "1. Многоугольник невыпуклый");
    }

    st_kod = polynomial(2.0, 3, &otv, 1.0, -2.0, 0.0, 5.0)
                 ? STATUS_OK : STATUS_INVALID_ARGUMENT;
    print_status("2. Значение многочлена", st_kod, otv);

    printf("3. Числа Капрекара: ");
    kolvo_kapr = find_kaprekar(10, kapr_spisok, 8,
                               "1", "9", "10", "45",
                               "55", "99", "100", "101");
    if (kolvo_kapr < 0) {
        print_status("ошибка", STATUS_INVALID_ARGUMENT, 0.0);
    } else {
        int i;
        for (i = 0; i < kolvo_kapr; ++i) {
            printf("%s%s", i == 0 ? "" : " ", kapr_spisok[i]);
        }
        putchar('\n');
    }

    st_kod = geometric_mean(&otv, 4, 1.0, 2.0, 4.0, 8.0)
                 ? STATUS_OK : STATUS_INVALID_ARGUMENT;
    print_status("4. Среднее геометрическое", st_kod, otv);

    st_kod = fast_power(2.0, -5, &otv)
                 ? STATUS_OK : STATUS_INVALID_ARGUMENT;
    print_status("5. Значение степени", st_kod, otv);

    st_kod = bisection(0.0, 2.0, 1e-9, equation_square, &otv);
    print_status("6a. x^2 - 2", st_kod, otv);
    st_kod = bisection(0.0, 1.0, 1e-9, equation_cosine, &otv);
    print_status("6b. cos(x) - x", st_kod, otv);
    st_kod = bisection(0.0, 2.0, 1e-9, equation_exponential, &otv);
    print_status("6c. exp(x) - 3", st_kod, otv);
    return 0;
}
