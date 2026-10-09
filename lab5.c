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

#define MAX_ITERS 10000000ULL

typedef double (*PodintegrFunc)(double);

/* чтение вещественного числа из строки */
static int read_double(const char *str_in, double *znach_out)
{
    char *ukaz_end;
    double chislo;

    if (str_in == NULL || znach_out == NULL || str_in[0] == '\0') {
        return 0;
    }

    chislo = strtod(str_in, &ukaz_end);
    if (ukaz_end == str_in || *ukaz_end != '\0' || !isfinite(chislo)) {
        return 0;
    }

    *znach_out = chislo;
    return 1;
}

/* вычисление суммы ряда a */
static StatusCode sum_a(double x, double eps, double *res_out)
{
    long double obsh_sum = 1.0L;
    long double tek_slag = 1.0L;
    unsigned long long n;

    if (res_out == NULL || !isfinite(x) || !isfinite(eps) || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    for (n = 1; n < MAX_ITERS; ++n) {
        long double sled_slag;
        long double mnozh_q;
        long double ost_hvost;

        tek_slag *= (long double)x / (long double)n;
        obsh_sum += tek_slag;

        if (!isfinite(tek_slag) || !isfinite(obsh_sum)) {
            return STATUS_OVERFLOW;
        }

        sled_slag = fabsl(tek_slag * (long double)x / (long double)(n + 1));
        mnozh_q = fabsl((long double)x) / (long double)(n + 2);

        if (mnozh_q < 1.0L) {
            ost_hvost = sled_slag / (1.0L - mnozh_q);
            if (ost_hvost <= (long double)eps) {
                *res_out = (double)obsh_sum;
                return isfinite(*res_out) ? STATUS_OK : STATUS_OVERFLOW;
            }
        }
    }

    return STATUS_NO_CONVERGENCE;
}

/* вычисление суммы ряда b */
static StatusCode sum_b(double x, double eps, double *res_out)
{
    long double obsh_sum = 1.0L;
    long double tek_slag = 1.0L;
    unsigned long long n;

    if (res_out == NULL || !isfinite(x) || !isfinite(eps) || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    for (n = 1; n < MAX_ITERS; ++n) {
        long double znamen =
            (2.0L * (long double)n - 1.0L) * (2.0L * (long double)n);
        long double sled_slag;

        tek_slag *= -(long double)x * (long double)x / znamen;
        obsh_sum += tek_slag;

        if (!isfinite(tek_slag) || !isfinite(obsh_sum)) {
            return STATUS_OVERFLOW;
        }

        sled_slag = fabsl(tek_slag) * fabsl((long double)x) *
                    fabsl((long double)x) /
                    ((2.0L * (long double)n + 1.0L) *
                     (2.0L * (long double)n + 2.0L));

        if (sled_slag <= (long double)eps) {
            *res_out = (double)obsh_sum;
            return isfinite(*res_out) ? STATUS_OK : STATUS_OVERFLOW;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

/* коэффициент перехода для ряда c */
static long double sum_c_ratio(long double x_kvadr, unsigned long long n)
{
    long double n_ld = (long double)n;

    return 27.0L * (n_ld + 1.0L) * (n_ld + 1.0L) * (n_ld + 1.0L) *
           x_kvadr /
           ((3.0L * n_ld + 1.0L) * (3.0L * n_ld + 2.0L) * (3.0L * n_ld + 3.0L));
}

/* вычисление суммы ряда c */
static StatusCode sum_c(double x, double eps, double *res_out)
{
    long double x_kvadr;
    long double obsh_sum = 1.0L;
    long double tek_slag = 1.0L;
    unsigned long long n;

    if (res_out == NULL || !isfinite(x) || !isfinite(eps) || eps <= 0.0 ||
        fabsl((long double)x) >= 1.0L) {
        return STATUS_INVALID_ARGUMENT;
    }

    x_kvadr = (long double)x * (long double)x;

    for (n = 0; n < MAX_ITERS; ++n) {
        long double otnosh = sum_c_ratio(x_kvadr, n);
        long double sled_otnosh;
        long double sled_slag;
        long double ost_hvost;

        tek_slag *= otnosh;
        obsh_sum += tek_slag;

        if (!isfinite(tek_slag) || !isfinite(obsh_sum)) {
            return STATUS_OVERFLOW;
        }

        sled_otnosh = sum_c_ratio(x_kvadr, n + 1ULL);
        sled_slag = fabsl(tek_slag) * sled_otnosh;

        /* оценка геометрического хвоста */
        if (sled_otnosh < 1.0L) {
            ost_hvost = sled_slag / (1.0L - sled_otnosh);
            if (ost_hvost <= (long double)eps) {
                *res_out = (double)obsh_sum;
                return isfinite(*res_out) ? STATUS_OK : STATUS_OVERFLOW;
            }
        }
    }

    return STATUS_NO_CONVERGENCE;
}

/* вычисление суммы ряда d */
static StatusCode sum_d(double x, double eps, double *res_out)
{
    long double x_kvadr;
    long double tek_slag;
    long double obsh_sum;
    unsigned long long n;

    if (res_out == NULL || !isfinite(x) || !isfinite(eps) || eps <= 0.0 ||
        fabsl((long double)x) >= 1.0L) {
        return STATUS_INVALID_ARGUMENT;
    }

    x_kvadr = (long double)x * (long double)x;
    tek_slag = -x_kvadr / 2.0L;
    obsh_sum = tek_slag;

    if (fabsl(tek_slag) <= (long double)eps) {
        *res_out = (double)obsh_sum;
        return STATUS_OK;
    }

    for (n = 1; n < MAX_ITERS; ++n) {
        long double otnosh =
            -x_kvadr * (2.0L * (long double)n + 1.0L) /
            (2.0L * (long double)n + 2.0L);
        long double sled_otnosh;
        long double sled_slag;

        tek_slag *= otnosh;
        obsh_sum += tek_slag;

        if (!isfinite(tek_slag) || !isfinite(obsh_sum)) {
            return STATUS_OVERFLOW;
        }

        sled_otnosh =
            -x_kvadr * (2.0L * (long double)(n + 1ULL) + 1.0L) /
            (2.0L * (long double)(n + 1ULL) + 2.0L);
        sled_slag = fabsl(tek_slag * sled_otnosh);

        if (sled_slag <= (long double)eps) {
            *res_out = (double)obsh_sum;
            return isfinite(*res_out) ? STATUS_OK : STATUS_OVERFLOW;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

/* подынтегральные функции */
static double integrand_a(double x)
{
    return x == 0.0 ? 1.0 : log1p(x) / x;
}

static double integrand_b(double x)
{
    return exp(-x * x / 2.0);
}

static double integrand_c(double t)
{
    return t * exp(-t);
}

static double integrand_d(double t)
{
    double x;

    if (t == 0.0) {
        return 0.0;
    }

    x = t * t;
    return 2.0 * t * exp(x * log(x));
}

/* формула парабол симпсона на отрезке */
static double simpson_value(double lev_g,
                            double prav_g,
                            double f_lev,
                            double f_ser,
                            double f_prav)
{
    return (prav_g - lev_g) * (f_lev + 4.0 * f_ser + f_prav) / 6.0;
}

/* рекурсивный шаг адаптивного метода симпсона */
static StatusCode adaptive_simpson_step(PodintegrFunc func,
                                        double lev_g,
                                        double prav_g,
                                        double f_lev,
                                        double f_ser,
                                        double f_prav,
                                        double celaya_plosh,
                                        double eps,
                                        int glubina,
                                        double *res_out)
{
    double sered = lev_g + (prav_g - lev_g) / 2.0;
    double lev_ser = lev_g + (sered - lev_g) / 2.0;
    double prav_ser = sered + (prav_g - sered) / 2.0;
    double f_lev_ser = func(lev_ser);
    double f_prav_ser = func(prav_ser);
    double lev_chast;
    double prav_chast;
    double raznica;
    double lev_res;
    double prav_res;
    StatusCode st_kod;

    if (!isfinite(f_lev_ser) || !isfinite(f_prav_ser) || glubina <= 0) {
        return STATUS_NO_CONVERGENCE;
    }

    lev_chast = simpson_value(lev_g, sered, f_lev, f_lev_ser, f_ser);
    prav_chast = simpson_value(sered, prav_g, f_ser, f_prav_ser, f_prav);
    raznica = lev_chast + prav_chast - celaya_plosh;

    if (fabs(raznica) <= 15.0 * eps) {
        *res_out = lev_chast + prav_chast + raznica / 15.0;
        return STATUS_OK;
    }

    st_kod = adaptive_simpson_step(func, lev_g, sered,
                                   f_lev, f_lev_ser, f_ser,
                                   lev_chast, eps / 2.0, glubina - 1, &lev_res);
    if (st_kod != STATUS_OK) {
        return st_kod;
    }

    st_kod = adaptive_simpson_step(func, sered, prav_g,
                                   f_ser, f_prav_ser, f_prav,
                                   prav_chast, eps / 2.0, glubina - 1, &prav_res);
    if (st_kod != STATUS_OK) {
        return st_kod;
    }

    *res_out = lev_res + prav_res;
    return STATUS_OK;
}

/* численное интегрирование на отрезке */
static StatusCode integrate(PodintegrFunc func,
                            double prav_g,
                            double eps,
                            double ost_hvost,
                            double *res_out)
{
    double lev_g = 0.0;
    double sered;
    double f_lev;
    double f_ser;
    double f_prav;
    double celaya_plosh;
    double znach;
    StatusCode st_kod;

    if (func == NULL || res_out == NULL ||
        !isfinite(prav_g) || prav_g <= 0.0 ||
        !isfinite(eps) || eps <= 0.0 ||
        !isfinite(ost_hvost) || ost_hvost < 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    sered = (lev_g + prav_g) / 2.0;
    f_lev = func(lev_g);
    f_ser = func(sered);
    f_prav = func(prav_g);

    if (!isfinite(f_lev) || !isfinite(f_ser) || !isfinite(f_prav)) {
        return STATUS_NO_CONVERGENCE;
    }

    celaya_plosh = simpson_value(lev_g, prav_g, f_lev, f_ser, f_prav);

    st_kod = adaptive_simpson_step(func, lev_g, prav_g,
                                   f_lev, f_ser, f_prav,
                                   celaya_plosh, eps / 2.0, 30, &znach);
    if (st_kod != STATUS_OK) {
        return st_kod;
    }

    *res_out = znach + ost_hvost;
    return isfinite(*res_out) ? STATUS_OK : STATUS_NO_CONVERGENCE;
}

/* выбор и вычисление одного из четырех интегралов */
static StatusCode calculate_integral(int nomer_int, double eps, double *res_out)
{
    double prav_g = 1.0;
    double ost_hvost = 0.0;

    if (nomer_int < 1 || nomer_int > 4 || res_out == NULL ||
        !isfinite(eps) || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    if (nomer_int == 1) {
        return integrate(integrand_a, prav_g, eps, ost_hvost, res_out);
    }
    if (nomer_int == 2) {
        return integrate(integrand_b, prav_g, eps, ost_hvost, res_out);
    }
    if (nomer_int == 4) {
        return integrate(integrand_d, prav_g, eps, ost_hvost, res_out);
    }

    prav_g = 1.0;
    while ((prav_g + 1.0) * exp(-prav_g) > eps / 2.0) {
        prav_g += 1.0;
        if (prav_g > 1000.0) {
            return STATUS_NO_CONVERGENCE;
        }
    }
    ost_hvost = (prav_g + 1.0) * exp(-prav_g);

    return integrate(integrand_c, prav_g, eps, ost_hvost, res_out);
}

/* вывод результата и статуса */
static void print_status(const char *imya_zadachi, StatusCode st_kod, double znach)
{
    switch (st_kod) {
        case STATUS_OK:
            printf("%s = %.12g\n", imya_zadachi, znach);
            break;
        case STATUS_INVALID_ARGUMENT:
            printf("%s: неверные данные\n", imya_zadachi);
            break;
        case STATUS_OVERFLOW:
            printf("%s: переполнение\n", imya_zadachi);
            break;
        case STATUS_NO_CONVERGENCE:
            printf("%s: вычисление не сошлось\n", imya_zadachi);
            break;
        case STATUS_MEMORY_ERROR:
            printf("%s: недостаточно памяти\n", imya_zadachi);
            break;
        case STATUS_FILE_ERROR:
            printf("%s: ошибка файла\n", imya_zadachi);
            break;
        case STATUS_NO_ROOTS:
            printf("%s: корни не найдены\n", imya_zadachi);
            break;
    }
}

int main(int argc, char **argv)
{
    double tochnost_eps;
    double tochka_x = 0.5;
    double tek_znach;
    StatusCode st_kod;
    StatusCode (*ryady_fn[4])(double, double, double *) = {
        sum_a, sum_b, sum_c, sum_d
    };
    int i;

    /* валидация параметров командной строки */
    if ((argc != 2 && argc != 3) ||
        !read_double(argv[1], &tochnost_eps) ||
        tochnost_eps <= 0.0) {
        fprintf(stderr,
                "Использование: %s <положительный эпсилон> [x, по умолчанию 0.5]\n",
                argv[0]);
        return 1;
    }

    if (argc == 3 && !read_double(argv[2], &tochka_x)) {
        fprintf(stderr, "x должно быть конечным действительным числом.\n");
        return 1;
    }

    printf("эпсилон = %.12g, x = %.12g\n\n", tochnost_eps, tochka_x);

    for (i = 0; i < 4; ++i) {
        char nazv[32];
        snprintf(nazv, sizeof(nazv), "сумма %c", (char)('a' + i));
        tek_znach = 0.0;
        st_kod = ryady_fn[i](tochka_x, tochnost_eps, &tek_znach);
        print_status(nazv, st_kod, tek_znach);
    }

    putchar('\n');

    for (i = 1; i <= 4; ++i) {
        char nazv[32];
        snprintf(nazv, sizeof(nazv), "интеграл %c", (char)('a' + i - 1));
        tek_znach = 0.0;
        st_kod = calculate_integral(i, tochnost_eps, &tek_znach);
        print_status(nazv, st_kod, tek_znach);
    }

    return 0;
}
