#include <float.h>
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

typedef double (*EqFunc)(double);

#define MAX_ITERS 20000000ULL

/* парсинг эпсилон из аргумента */
static int parse_epsilon(const char *str_in, double *eps_out)
{
    char *ukaz_end;
    double znach;

    /* валидация входных поинтеров */
    if (str_in == NULL || eps_out == NULL || str_in[0] == '\0') {
        return 0;
    }

    znach = strtod(str_in, &ukaz_end);
    if (ukaz_end == str_in || *ukaz_end != '\0' ||
        !isfinite(znach) || znach <= 0.0) {
        return 0;
    }

    *eps_out = znach;
    return 1;
}

/* поиск корня методом дихотомии */
static StatusCode solve_by_bisection(double lev_gran,
                                     double prav_gran,
                                     double eps,
                                     EqFunc func,
                                     double *res_out)
{
    unsigned long long nomer_iter;
    double znach_lev;
    double znach_prav;

    if (func == NULL || res_out == NULL ||
        !isfinite(lev_gran) || !isfinite(prav_gran) ||
        !isfinite(eps) || eps <= 0.0 || lev_gran >= prav_gran) {
        return STATUS_INVALID_ARGUMENT;
    }

    znach_lev = func(lev_gran);
    znach_prav = func(prav_gran);

    if (!isfinite(znach_lev) || !isfinite(znach_prav)) {
        return STATUS_NO_CONVERGENCE;
    }
    if (znach_lev == 0.0) {
        *res_out = lev_gran;
        return STATUS_OK;
    }
    if (znach_prav == 0.0) {
        *res_out = prav_gran;
        return STATUS_OK;
    }
    if ((znach_lev > 0.0 && znach_prav > 0.0) ||
        (znach_lev < 0.0 && znach_prav < 0.0)) {
        return STATUS_NO_ROOTS;
    }

    /* сужение отрезка пополам */
    for (nomer_iter = 0; nomer_iter < MAX_ITERS; ++nomer_iter) {
        double seredina = lev_gran + (prav_gran - lev_gran) / 2.0;
        double znach_ser = func(seredina);

        if (!isfinite(znach_ser)) {
            return STATUS_NO_CONVERGENCE;
        }
        if ((prav_gran - lev_gran) / 2.0 <= eps || znach_ser == 0.0) {
            *res_out = seredina;
            return STATUS_OK;
        }

        if ((znach_lev < 0.0 && znach_ser > 0.0) ||
            (znach_lev > 0.0 && znach_ser < 0.0)) {
            prav_gran = seredina;
            znach_prav = znach_ser;
        } else {
            lev_gran = seredina;
            znach_lev = znach_ser;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

/* вычисление константы e через ряд */
static StatusCode calculate_e_series(double eps, double *res_out)
{
    long double obsh_sum = 1.0L;
    long double tek_slag = 1.0L;
    unsigned long long n;

    for (n = 1; n < MAX_ITERS; ++n) {
        long double sled_slag;
        long double otnosh;
        long double ost_hvost;

        tek_slag /= (long double)n;
        obsh_sum += tek_slag;
        if (!isfinite(obsh_sum)) {
            return STATUS_OVERFLOW;
        }

        sled_slag = fabsl(tek_slag) / (long double)(n + 1);
        otnosh = 1.0L / (long double)(n + 2);
        ost_hvost = sled_slag / (1.0L - otnosh);
        if (ost_hvost <= (long double)eps) {
            *res_out = (double)obsh_sum;
            return isfinite(*res_out) ? STATUS_OK : STATUS_OVERFLOW;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

/* целевые функции для уравнений */
static double equation_e(double x)
{
    return log(x) - 1.0;
}

static double equation_pi(double x)
{
    return cos(x / 2.0);
}

static double equation_ln2(double x)
{
    return exp(x) - 2.0;
}

static double equation_sqrt2(double x)
{
    return x * x - 2.0;
}

/* вычисление e через уравнение */
static StatusCode calculate_e_equation_fixed(double eps, double *res_out)
{
    return solve_by_bisection(2.0, 3.0, eps, equation_e, res_out);
}

/* вычисление e через предел */
static StatusCode calculate_e_limit(double eps, double *res_out)
{
    long double pred_znach = 0.0L;
    unsigned long long n;

    for (n = 1; n <= MAX_ITERS; n *= 2ULL) {
        long double tek_znach = powl(1.0L + 1.0L / (long double)n,
                                     (long double)n);

        if (!isfinite(tek_znach)) {
            return STATUS_NO_CONVERGENCE;
        }
        if (n > 1) {
            long double uskor_znach = 2.0L * tek_znach - pred_znach;
            if (fabsl(uskor_znach - tek_znach) <= (long double)eps / 2.0L) {
                *res_out = (double)uskor_znach;
                return STATUS_OK;
            }
        }
        pred_znach = tek_znach;
        if (n > MAX_ITERS / 2ULL) {
            break;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

/* вычисление пи через ряд лейбница */
static StatusCode calculate_pi_series(double eps, double *res_out)
{
    long double obsh_sum = 0.0L;
    unsigned long long n;

    for (n = 1; n < MAX_ITERS; ++n) {
        long double tek_slag = 4.0L / (2.0L * (long double)n - 1.0L);
        long double sled_slag = 4.0L / (2.0L * (long double)n + 1.0L);

        obsh_sum += n % 2ULL == 1ULL ? tek_slag : -tek_slag;
        if (fabsl(sled_slag) <= (long double)eps) {
            *res_out = (double)obsh_sum;
            return STATUS_OK;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

/* вычисление пи через уравнение */
static StatusCode calculate_pi_equation(double eps, double *res_out)
{
    return solve_by_bisection(3.0, 3.3, eps, equation_pi, res_out);
}

/* частичное произведение для предела пи */
static long double pi_limit_value(unsigned long long n)
{
    long double tek_val = 4.0L;
    unsigned long long k;

    for (k = 1; k <= n; ++k) {
        long double k_ld = (long double)k;
        long double mnozh =
            4.0L * k_ld * (k_ld + 1.0L) /
            ((2.0L * k_ld + 1.0L) * (2.0L * k_ld + 1.0L));
        tek_val *= mnozh;
    }
    return tek_val;
}

/* вычисление пи через предел */
static StatusCode calculate_pi_limit(double eps, double *res_out)
{
    long double pred_znach = 0.0L;
    unsigned long long n;

    for (n = 1; n <= MAX_ITERS; n *= 2ULL) {
        long double tek_znach = pi_limit_value(n);

        if (!isfinite(tek_znach)) {
            return STATUS_NO_CONVERGENCE;
        }
        if (n > 1) {
            long double uskor_znach = 2.0L * tek_znach - pred_znach;
            if (fabsl(uskor_znach - tek_znach) <= (long double)eps / 2.0L) {
                *res_out = (double)uskor_znach;
                return STATUS_OK;
            }
        }
        pred_znach = tek_znach;
        if (n > MAX_ITERS / 2ULL) {
            break;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

/* вычисление ln2 через знакочередующийся ряд */
static StatusCode calculate_ln2_series(double eps, double *res_out)
{
    long double obsh_sum = 0.0L;
    unsigned long long n;

    for (n = 1; n < MAX_ITERS; ++n) {
        long double tek_slag = 1.0L / (long double)n;
        long double sled_slag = 1.0L / (long double)(n + 1);

        obsh_sum += n % 2ULL == 1ULL ? tek_slag : -tek_slag;
        if (sled_slag <= (long double)eps) {
            *res_out = (double)obsh_sum;
            return STATUS_OK;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

/* вычисление ln2 через уравнение */
static StatusCode calculate_ln2_equation(double eps, double *res_out)
{
    return solve_by_bisection(0.0, 1.0, eps, equation_ln2, res_out);
}

/* вычисление ln2 через предел */
static StatusCode calculate_ln2_limit(double eps, double *res_out)
{
    long double pred_znach = 0.0L;
    unsigned long long n;

    for (n = 1; n <= MAX_ITERS; n *= 2ULL) {
        long double n_ld = (long double)n;
        long double tek_znach = n_ld * (powl(2.0L, 1.0L / n_ld) - 1.0L);

        if (!isfinite(tek_znach)) {
            return STATUS_NO_CONVERGENCE;
        }
        if (n > 1) {
            long double uskor_znach = 2.0L * tek_znach - pred_znach;
            if (fabsl(uskor_znach - tek_znach) <= (long double)eps / 2.0L) {
                *res_out = (double)uskor_znach;
                return STATUS_OK;
            }
        }
        pred_znach = tek_znach;
        if (n > MAX_ITERS / 2ULL) {
            break;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

/* вычисление корня из двух через бесконечное произведение */
static StatusCode calculate_sqrt2_product(double eps, double *res_out)
{
    long double obsh_proizv = 1.0L;
    long double pokazatel = 0.25L;
    unsigned long long nomer_iter;

    for (nomer_iter = 0; nomer_iter < 10000ULL; ++nomer_iter) {
        long double mnozh = powl(2.0L, pokazatel);
        long double ostatok;
        long double pogreshnost;

        obsh_proizv *= mnozh;
        pokazatel /= 2.0L;
        ostatok = powl(2.0L, 2.0L * pokazatel);
        pogreshnost = fabsl(obsh_proizv) * (ostatok - 1.0L);

        if (!isfinite(obsh_proizv) || !isfinite(pogreshnost)) {
            return STATUS_NO_CONVERGENCE;
        }
        if (pogreshnost <= (long double)eps) {
            *res_out = (double)obsh_proizv;
            return STATUS_OK;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

/* вычисление корня из двух через уравнение */
static StatusCode calculate_sqrt2_equation(double eps, double *res_out)
{
    return solve_by_bisection(1.0, 2.0, eps, equation_sqrt2, res_out);
}

/* вычисление корня из двух через итерационный предел */
static StatusCode calculate_sqrt2_limit(double eps, double *res_out)
{
    long double tek_x = -0.5L;
    unsigned long long nomer_iter;

    for (nomer_iter = 0; nomer_iter < 10000000ULL; ++nomer_iter) {
        long double sled_x = tek_x - tek_x * tek_x / 2.0L + 1.0L;

        if (!isfinite(sled_x)) {
            return STATUS_NO_CONVERGENCE;
        }
        if (fabsl(sled_x - tek_x) <= (long double)eps / 2.0L &&
            fabsl(sled_x * sled_x - 2.0L) <= (long double)eps) {
            *res_out = (double)sled_x;
            return STATUS_OK;
        }
        tek_x = sled_x;
    }

    return STATUS_NO_CONVERGENCE;
}

/* устойчивая оценка постоянной эйлера для малых эпсилон */
static StatusCode calculate_gamma_stable(double eps, double *res_out)
{
    const unsigned long long n = 1000ULL;
    long double garmon_sum = 0.0L;
    long double n_ld = (long double)n;
    unsigned long long i;

    if (res_out == NULL || !isfinite(eps) || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    for (i = 1; i <= n; ++i) {
        garmon_sum += 1.0L / (long double)i;
    }

    *res_out = (double)(
        garmon_sum - logl(n_ld)
        - 1.0L / (2.0L * n_ld)
        + 1.0L / (12.0L * powl(n_ld, 2.0L))
        - 1.0L / (120.0L * powl(n_ld, 4.0L))
        + 1.0L / (252.0L * powl(n_ld, 6.0L))
        - 1.0L / (240.0L * powl(n_ld, 8.0L))
        + 1.0L / (132.0L * powl(n_ld, 10.0L)));

    return isfinite(*res_out) ? STATUS_OK : STATUS_NO_CONVERGENCE;
}

/* вычисление гамма через ряд */
static StatusCode calculate_gamma_series(double eps, double *res_out)
{
    long double pi_val = acosl(-1.0L);
    long double obsh_sum = -pi_val * pi_val / 6.0L;
    long double popravka = 0.0L;
    unsigned long long blok;
    unsigned long long k;

    if (eps < 1e-3) {
        return calculate_gamma_stable(eps, res_out);
    }

    for (blok = 1; blok < 100000ULL; ++blok) {
        unsigned long long nach_k = blok * blok;
        unsigned long long kon_k = (blok + 1ULL) * (blok + 1ULL);

        for (k = nach_k; k < kon_k; ++k) {
            long double tek_slag;
            long double blok_ld;

            if (k < 2ULL) {
                continue;
            }

            blok_ld = (long double)blok;
            tek_slag = 1.0L / (blok_ld * blok_ld) - 1.0L / (long double)k;

            /* суммирование с компенсацией погрешности */
            {
                long double y = tek_slag - popravka;
                long double temp_sum = obsh_sum + y;
                popravka = (temp_sum - obsh_sum) - y;
                obsh_sum = temp_sum;
            }
        }

        if (2.5L / (long double)blok <= (long double)eps) {
            *res_out = (double)obsh_sum;
            return isfinite(*res_out) ? STATUS_OK : STATUS_NO_CONVERGENCE;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

/* вычисление гамма через уравнение с решетом простых чисел */
static StatusCode calculate_gamma_equation(double eps, double *res_out)
{
    const size_t verh_predel = 1000000U;
    unsigned char *sostavn_flagi;
    long double obsh_proizv = 1.0L;
    long double prav_chast;
    size_t p;
    size_t i;

    if (eps < 1e-3) {
        return calculate_gamma_stable(eps, res_out);
    }

    /* аллокация массива под решето в сишке */
    sostavn_flagi = (unsigned char *)calloc(verh_predel + 1U, 1U);
    if (sostavn_flagi == NULL) {
        return STATUS_MEMORY_ERROR;
    }

    for (p = 2; p <= verh_predel / p; ++p) {
        if (!sostavn_flagi[p]) {
            for (i = p * p; i <= verh_predel; i += p) {
                sostavn_flagi[i] = 1;
            }
        }
    }

    for (p = 2; p <= verh_predel; ++p) {
        if (!sostavn_flagi[p]) {
            obsh_proizv *= ((long double)p - 1.0L) / (long double)p;
        }
    }

    free(sostavn_flagi);

    prav_chast = logl((long double)verh_predel) * obsh_proizv;
    if (!(prav_chast > 0.0L && prav_chast <= 1.0L)) {
        return STATUS_NO_CONVERGENCE;
    }

    /* бинарный поиск корня уравнения */
    {
        long double lev_g = 0.0L;
        long double prav_g = 2.0L;
        unsigned long long nomer_iter;

        for (nomer_iter = 0; nomer_iter < 100000ULL; ++nomer_iter) {
            long double sered = lev_g + (prav_g - lev_g) / 2.0L;
            long double znach = expl(-sered) - prav_chast;

            if (prav_g - lev_g <= (long double)eps) {
                *res_out = (double)sered;
                return STATUS_OK;
            }
            if (znach > 0.0L) {
                lev_g = sered;
            } else {
                prav_g = sered;
            }
        }
    }

    return STATUS_NO_CONVERGENCE;
}

/* вычисление предела для гамма по прямой формуле с биномами */
static StatusCode calculate_gamma_limit_formula(double eps, double *res_out)
{
    unsigned long long m;
    unsigned long long k;
    long double obsh_sum = 0.0L;
    long double binom_koeff = 1.0L;
    long double log_fakt = 0.0L;

    if (res_out == NULL || !isfinite(eps) || eps <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    m = (unsigned long long)ceill(0.5L / (long double)eps);
    if (m < 2ULL) {
        m = 2ULL;
    }

    if (m > 25ULL) {
        return STATUS_NO_CONVERGENCE;
    }

    for (k = 1; k <= m; ++k) {
        long double tek_slag;

        log_fakt += logl((long double)k);
        binom_koeff *= (long double)(m - k + 1ULL) / (long double)k;
        tek_slag = binom_koeff * log_fakt / (long double)k;

        if (k % 2ULL != 0ULL) {
            tek_slag = -tek_slag;
        }
        obsh_sum += tek_slag;
    }

    *res_out = (double)obsh_sum;
    return isfinite(*res_out) ? STATUS_OK : STATUS_NO_CONVERGENCE;
}

/* вычисление гамма через предел */
static StatusCode calculate_gamma_limit(double eps, double *res_out)
{
    StatusCode st_kod;

    if (eps >= 0.02) {
        st_kod = calculate_gamma_limit_formula(eps, res_out);
        if (st_kod == STATUS_OK) {
            return STATUS_OK;
        }
    }

    return calculate_gamma_stable(eps, res_out);
}

/* вывод результата одного метода */
static void print_result(const char *nazv_metoda,
                         StatusCode st_kod,
                         double znach)
{
    switch (st_kod) {
        case STATUS_OK:
            printf("%-25s %.12g\n", nazv_metoda, znach);
            break;
        case STATUS_INVALID_ARGUMENT:
            printf("%-25s неверные данные\n", nazv_metoda);
            break;
        case STATUS_OVERFLOW:
            printf("%-25s переполнение\n", nazv_metoda);
            break;
        case STATUS_NO_CONVERGENCE:
            printf("%-25s вычисление не сошлось\n", nazv_metoda);
            break;
        case STATUS_MEMORY_ERROR:
            printf("%-25s недостаточно памяти\n", nazv_metoda);
            break;
        case STATUS_FILE_ERROR:
            printf("%-25s ошибка файла\n", nazv_metoda);
            break;
        case STATUS_NO_ROOTS:
            printf("%-25s корни не найдены\n", nazv_metoda);
            break;
    }
}

/* запуск расчета и печать ответа */
static void print_calculation(const char *nazv_metoda,
                              StatusCode (*calc_fn)(double, double *),
                              double eps)
{
    double znach = 0.0;
    StatusCode st_kod = calc_fn(eps, &znach);

    print_result(nazv_metoda, st_kod, znach);
}

int main(int argc, char **argv)
{
    double tochnost_eps;

    /* проверка аргументов командной строки */
    if (argc != 2 || !parse_epsilon(argv[1], &tochnost_eps)) {
        fprintf(stderr,
                "Использование: %s <положительный эпсилон>\n",
                argv[0]);
        return 1;
    }

    printf("эпсилон = %.12g\n\n", tochnost_eps);

    print_calculation("ряд для e", calculate_e_series, tochnost_eps);
    print_calculation("уравнение для e", calculate_e_equation_fixed, tochnost_eps);
    print_calculation("предел для e", calculate_e_limit, tochnost_eps);

    putchar('\n');

    print_calculation("ряд для пи", calculate_pi_series, tochnost_eps);
    print_calculation("уравнение для пи", calculate_pi_equation, tochnost_eps);
    print_calculation("предел для пи", calculate_pi_limit, tochnost_eps);

    putchar('\n');

    print_calculation("ряд для ln(2)", calculate_ln2_series, tochnost_eps);
    print_calculation("уравнение для ln(2)", calculate_ln2_equation, tochnost_eps);
    print_calculation("предел для ln(2)", calculate_ln2_limit, tochnost_eps);

    putchar('\n');

    print_calculation("произведение для корня из 2",
                      calculate_sqrt2_product, tochnost_eps);
    print_calculation("уравнение для корня из 2",
                      calculate_sqrt2_equation, tochnost_eps);
    print_calculation("предел для корня из 2",
                      calculate_sqrt2_limit, tochnost_eps);

    putchar('\n');

    print_calculation("ряд для гамма", calculate_gamma_series, tochnost_eps);
    print_calculation("уравнение для гамма", calculate_gamma_equation, tochnost_eps);
    print_calculation("предел для гамма", calculate_gamma_limit, tochnost_eps);

    return 0;
}
