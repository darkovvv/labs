#include <errno.h>
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

#define MAX_ITERATIONS 10000000ULL

typedef double (*Integrand)(double);

static int read_double(const char *text, double *value)
{
    char *end;
    double number;

    if (text == NULL || value == NULL || text[0] == '\0') {
        return 0;
    }

    errno = 0;
    number = strtod(text, &end);
    if (errno == ERANGE || end == text || *end != '\0' ||
        !isfinite(number)) {
        return 0;
    }

    *value = number;
    return 1;
}

static StatusCode sum_a(double x, double epsilon, double *result)
{
    long double sum = 1.0L;
    long double term = 1.0L;
    unsigned long long n;

    if (result == NULL || !isfinite(x) ||
        !isfinite(epsilon) || epsilon <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    for (n = 1; n < MAX_ITERATIONS; ++n) {
        long double next_term;
        long double q;
        long double tail;

        term *= (long double)x / (long double)n;
        sum += term;

        if (!isfinite(term) || !isfinite(sum)) {
            return STATUS_OVERFLOW;
        }

        next_term = fabsl(term * (long double)x /
                          (long double)(n + 1));
        q = fabsl((long double)x) /
            (long double)(n + 2);

        if (q < 1.0L) {
            tail = next_term / (1.0L - q);
            if (tail <= (long double)epsilon) {
                *result = (double)sum;
                return isfinite(*result)
                    ? STATUS_OK : STATUS_OVERFLOW;
            }
        }
    }

    return STATUS_NO_CONVERGENCE;
}

static StatusCode sum_b(double x, double epsilon, double *result)
{
    long double sum = 1.0L;
    long double term = 1.0L;
    unsigned long long n;

    if (result == NULL || !isfinite(x) ||
        !isfinite(epsilon) || epsilon <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    for (n = 1; n < MAX_ITERATIONS; ++n) {
        long double denominator =
            (2.0L * (long double)n - 1.0L) *
            (2.0L * (long double)n);
        long double next_term;

        term *= -(long double)x * (long double)x / denominator;
        sum += term;

        if (!isfinite(term) || !isfinite(sum)) {
            return STATUS_OVERFLOW;
        }

        next_term = fabsl(term) * fabsl((long double)x) *
                    fabsl((long double)x) /
                    ((2.0L * (long double)n + 1.0L) *
                     (2.0L * (long double)n + 2.0L));

        if (next_term <= (long double)epsilon) {
            *result = (double)sum;
            return isfinite(*result)
                ? STATUS_OK : STATUS_OVERFLOW;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

static long double sum_c_ratio(long double x_squared,
                               unsigned long long n)
{
    long double value = (long double)n;

    return 27.0L * (value + 1.0L) *
           (value + 1.0L) * (value + 1.0L) *
           x_squared /
           ((3.0L * value + 1.0L) *
            (3.0L * value + 2.0L) *
            (3.0L * value + 3.0L));
}

static StatusCode sum_c(double x, double epsilon, double *result)
{
    long double x_squared;
    long double sum = 1.0L;
    long double term = 1.0L;
    unsigned long long n;

    if (result == NULL || !isfinite(x) ||
        !isfinite(epsilon) || epsilon <= 0.0 ||
        fabsl((long double)x) >= 1.0L) {
        return STATUS_INVALID_ARGUMENT;
    }

    x_squared = (long double)x * (long double)x;

    for (n = 0; n < MAX_ITERATIONS; ++n) {
        long double ratio = sum_c_ratio(x_squared, n);
        long double next_ratio;
        long double next_term;
        long double tail;

        term *= ratio;
        sum += term;

        if (!isfinite(term) || !isfinite(sum)) {
            return STATUS_OVERFLOW;
        }

        next_ratio = sum_c_ratio(x_squared, n + 1ULL);
        next_term = fabsl(term) * next_ratio;

        /* После достижения ratio < 1 последующие отношения
           убывают к x^2, поэтому геометрическая оценка применима. */
        if (next_ratio < 1.0L) {
            tail = next_term / (1.0L - next_ratio);
            if (tail <= (long double)epsilon) {
                *result = (double)sum;
                return isfinite(*result)
                    ? STATUS_OK : STATUS_OVERFLOW;
            }
        }
    }

    return STATUS_NO_CONVERGENCE;
}

static StatusCode sum_d(double x, double epsilon, double *result)
{
    long double x_squared;
    long double term;
    long double sum;
    unsigned long long n;

    if (result == NULL || !isfinite(x) ||
        !isfinite(epsilon) || epsilon <= 0.0 ||
        fabsl((long double)x) >= 1.0L) {
        return STATUS_INVALID_ARGUMENT;
    }

    x_squared = (long double)x * (long double)x;
    term = -x_squared / 2.0L;
    sum = term;

    if (fabsl(term) <= (long double)epsilon) {
        *result = (double)sum;
        return STATUS_OK;
    }

    for (n = 1; n < MAX_ITERATIONS; ++n) {
        long double ratio =
            -x_squared * (2.0L * (long double)n + 1.0L) /
            (2.0L * (long double)n + 2.0L);
        long double next_ratio;
        long double next_term;

        term *= ratio;
        sum += term;

        if (!isfinite(term) || !isfinite(sum)) {
            return STATUS_OVERFLOW;
        }

        next_ratio =
            -x_squared * (2.0L * (long double)(n + 1ULL) + 1.0L) /
            (2.0L * (long double)(n + 1ULL) + 2.0L);
        next_term = fabsl(term * next_ratio);

        if (next_term <= (long double)epsilon) {
            *result = (double)sum;
            return isfinite(*result)
                ? STATUS_OK : STATUS_OVERFLOW;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

static double integrand_a(double x)
{
    return x == 0.0 ? 1.0 : log1p(x) / x;
}

static double integrand_b(double x)
{
    return exp(-x * x / 2.0);
}

/* x = 1 - exp(-t), dx = exp(-t) dt. */
static double integrand_c(double t)
{
    return t * exp(-t);
}

/* x = t^2, dx = 2t dt. */
static double integrand_d(double t)
{
    double x;

    if (t == 0.0) {
        return 0.0;
    }

    x = t * t;
    return 2.0 * t * exp(x * log(x));
}

static double simpson_value(double left,
                            double middle,
                            double right,
                            double f_left,
                            double f_middle,
                            double f_right)
{
    return (right - left) *
           (f_left + 4.0 * f_middle + f_right) / 6.0;
}

static StatusCode adaptive_simpson_step(
    Integrand function,
    double left,
    double right,
    double f_left,
    double f_middle,
    double f_right,
    double whole,
    double epsilon,
    int depth,
    double *result)
{
    double middle = left + (right - left) / 2.0;
    double left_middle = left + (middle - left) / 2.0;
    double right_middle = middle + (right - middle) / 2.0;
    double f_left_middle = function(left_middle);
    double f_right_middle = function(right_middle);
    double left_part;
    double right_part;
    double delta;
    double left_result;
    double right_result;
    StatusCode status;

    if (!isfinite(f_left_middle) || !isfinite(f_right_middle)) {
        return STATUS_NO_CONVERGENCE;
    }
    if (depth <= 0) {
        return STATUS_NO_CONVERGENCE;
    }

    left_part = simpson_value(
        left, left_middle, middle,
        f_left, f_left_middle, f_middle);
    right_part = simpson_value(
        middle, right_middle, right,
        f_middle, f_right_middle, f_right);
    delta = left_part + right_part - whole;

    if (fabs(delta) <= 15.0 * epsilon) {
        *result = left_part + right_part + delta / 15.0;
        return STATUS_OK;
    }

    status = adaptive_simpson_step(
        function,
        left,
        middle,
        f_left,
        f_left_middle,
        f_middle,
        left_part,
        epsilon / 2.0,
        depth - 1,
        &left_result);

    if (status != STATUS_OK) {
        return status;
    }

    status = adaptive_simpson_step(
        function,
        middle,
        right,
        f_middle,
        f_right_middle,
        f_right,
        right_part,
        epsilon / 2.0,
        depth - 1,
        &right_result);

    if (status != STATUS_OK) {
        return status;
    }

    *result = left_result + right_result;
    return STATUS_OK;
}

static StatusCode integrate(Integrand function,
                            double right,
                            double epsilon,
                            double tail,
                            double *result)
{
    double left = 0.0;
    double middle;
    double f_left;
    double f_middle;
    double f_right;
    double whole;
    double value;
    StatusCode status;

    if (function == NULL || result == NULL ||
        !isfinite(right) || right <= 0.0 ||
        !isfinite(epsilon) || epsilon <= 0.0 ||
        !isfinite(tail) || tail < 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    middle = (left + right) / 2.0;
    f_left = function(left);
    f_middle = function(middle);
    f_right = function(right);

    if (!isfinite(f_left) || !isfinite(f_middle) ||
        !isfinite(f_right)) {
        return STATUS_NO_CONVERGENCE;
    }

    whole = simpson_value(
        left, middle, right,
        f_left, f_middle, f_right);

    status = adaptive_simpson_step(
        function,
        left,
        right,
        f_left,
        f_middle,
        f_right,
        whole,
        epsilon / 2.0,
        30,
        &value);

    if (status != STATUS_OK) {
        return status;
    }

    *result = value + tail;
    return isfinite(*result)
        ? STATUS_OK : STATUS_NO_CONVERGENCE;
}

static StatusCode calculate_integral(int number,
                                     double epsilon,
                                     double *result)
{
    double right = 1.0;
    double tail = 0.0;

    if (number < 1 || number > 4 || result == NULL ||
        !isfinite(epsilon) || epsilon <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    if (number == 1) {
        return integrate(integrand_a, right, epsilon, tail, result);
    }
    if (number == 2) {
        return integrate(integrand_b, right, epsilon, tail, result);
    }
    if (number == 4) {
        return integrate(integrand_d, right, epsilon, tail, result);
    }

    /* Для интеграла c интегрируем t*exp(-t) на [0, T].
       Хвост равен (T + 1) * exp(-T). */
    right = 1.0;
    while ((right + 1.0) * exp(-right) > epsilon / 2.0) {
        right += 1.0;
        if (right > 1000.0) {
            return STATUS_NO_CONVERGENCE;
        }
    }
    tail = (right + 1.0) * exp(-right);

    return integrate(integrand_c, right, epsilon, tail, result);
}

static void print_status(const char *name,
                         StatusCode status,
                         double value)
{
    switch (status) {
        case STATUS_OK:
            printf("%s = %.12g\n", name, value);
            break;
        case STATUS_INVALID_ARGUMENT:
            printf("%s: неверные данные\n", name);
            break;
        case STATUS_OVERFLOW:
            printf("%s: переполнение\n", name);
            break;
        case STATUS_NO_CONVERGENCE:
            printf("%s: вычисление не сошлось\n", name);
            break;
        case STATUS_MEMORY_ERROR:
            printf("%s: недостаточно памяти\n", name);
            break;
        case STATUS_FILE_ERROR:
            printf("%s: ошибка файла\n", name);
            break;
        case STATUS_NO_ROOTS:
            printf("%s: корни не найдены\n", name);
            break;
    }
}

int main(int argc, char **argv)
{
    double epsilon;
    double x = 0.5;
    double value;
    StatusCode (*series[4])(double, double, double *) = {
        sum_a, sum_b, sum_c, sum_d
    };
    int i;

    if ((argc != 2 && argc != 3) ||
        !read_double(argv[1], &epsilon) ||
        epsilon <= 0.0) {
        fprintf(stderr,
                "Использование: %s <положительный эпсилон> "
                "[x, по умолчанию 0.5]\n",
                argv[0]);
        return 1;
    }

    if (argc == 3 && !read_double(argv[2], &x)) {
        fprintf(stderr,
                "x должно быть конечным действительным числом.\n");
        return 1;
    }

    printf("эпсилон = %.12g, x = %.12g\n\n",
           epsilon, x);

    for (i = 0; i < 4; ++i) {
        char name[32];

        snprintf(name, sizeof(name), "сумма %c", (char)('a' + i));
        value = 0.0;
        print_status(name, series[i](x, epsilon, &value), value);
    }

    putchar('\n');

    for (i = 1; i <= 4; ++i) {
        char name[32];

        snprintf(name, sizeof(name), "интеграл %c",
                 (char)('a' + i - 1));
        value = 0.0;
        print_status(name,
                     calculate_integral(i, epsilon, &value),
                     value);
    }

    return 0;
}



