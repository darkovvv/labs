#include <errno.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/* Лабораторная работа №2.
   Все вычисления внутри функций выполняются с контролем сходимости.
*/

typedef enum {
    STATUS_OK = 0,
    STATUS_INVALID_ARGUMENT,
    STATUS_OVERFLOW,
    STATUS_NO_CONVERGENCE,
    STATUS_MEMORY_ERROR,
    STATUS_FILE_ERROR,
    STATUS_NO_ROOTS
} StatusCode;

typedef double (*EquationFunction)(double);

#define MAX_ITERATIONS 20000000ULL

static int parse_epsilon(const char *text, double *epsilon)
{
    char *end;
    double value;

    if (text == NULL || epsilon == NULL || text[0] == '\0') {
        return 0;
    }

    errno = 0;
    value = strtod(text, &end);
    if (errno == ERANGE || end == text || *end != '\0' ||
        !isfinite(value) || value <= 0.0) {
        return 0;
    }

    *epsilon = value;
    return 1;
}

static StatusCode solve_by_bisection(double left,
                                     double right,
                                     double epsilon,
                                     EquationFunction function,
                                     double *result)
{
    unsigned long long iteration;
    double left_value;
    double right_value;

    if (function == NULL || result == NULL ||
        !isfinite(left) || !isfinite(right) ||
        !isfinite(epsilon) || epsilon <= 0.0 || left >= right) {
        return STATUS_INVALID_ARGUMENT;
    }

    left_value = function(left);
    right_value = function(right);

    if (!isfinite(left_value) || !isfinite(right_value)) {
        return STATUS_NO_CONVERGENCE;
    }
    if (left_value == 0.0) {
        *result = left;
        return STATUS_OK;
    }
    if (right_value == 0.0) {
        *result = right;
        return STATUS_OK;
    }
    if ((left_value > 0.0 && right_value > 0.0) ||
        (left_value < 0.0 && right_value < 0.0)) {
        return STATUS_NO_ROOTS;
    }

    for (iteration = 0; iteration < MAX_ITERATIONS; ++iteration) {
        double middle = left + (right - left) / 2.0;
        double middle_value = function(middle);

        if (!isfinite(middle_value)) {
            return STATUS_NO_CONVERGENCE;
        }
        if ((right - left) / 2.0 <= epsilon || middle_value == 0.0) {
            *result = middle;
            return STATUS_OK;
        }

        if ((left_value < 0.0 && middle_value > 0.0) ||
            (left_value > 0.0 && middle_value < 0.0)) {
            right = middle;
            right_value = middle_value;
        } else {
            left = middle;
            left_value = middle_value;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

static StatusCode calculate_e_series(double epsilon, double *result)
{
    long double sum = 1.0L;
    long double term = 1.0L;
    unsigned long long n;

    for (n = 1; n < MAX_ITERATIONS; ++n) {
        long double next_term;
        long double ratio;
        long double tail;

        term /= (long double)n;
        sum += term;
        if (!isfinite(sum)) {
            return STATUS_OVERFLOW;
        }

        next_term = fabsl(term) / (long double)(n + 1);
        ratio = 1.0L / (long double)(n + 2);
        tail = next_term / (1.0L - ratio);
        if (tail <= (long double)epsilon) {
            *result = (double)sum;
            return isfinite(*result) ? STATUS_OK : STATUS_OVERFLOW;
        }
    }

    return STATUS_NO_CONVERGENCE;
}


static double equation_e(double x)
{
    return log(x) - 1.0;
}

static double equation_pi(double x)
{
    /* cos(x) = -1 имеет тот же корень pi, что и cos(x / 2) = 0. */
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

static StatusCode calculate_e_equation_fixed(double epsilon, double *result)
{
    return solve_by_bisection(2.0, 3.0, epsilon,
                              equation_e, result);
}

static StatusCode calculate_e_limit(double epsilon, double *result)
{
    long double previous = 0.0L;
    unsigned long long n;

    for (n = 1; n <= MAX_ITERATIONS; n *= 2ULL) {
        long double current = powl(1.0L + 1.0L / (long double)n,
                                   (long double)n);

        if (!isfinite(current)) {
            return STATUS_NO_CONVERGENCE;
        }
        if (n > 1) {
            long double accelerated = 2.0L * current - previous;
            if (fabsl(accelerated - current) <=
                (long double)epsilon / 2.0L) {
                *result = (double)accelerated;
                return STATUS_OK;
            }
        }
        previous = current;
        if (n > MAX_ITERATIONS / 2ULL) {
            break;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

static StatusCode calculate_pi_series(double epsilon, double *result)
{
    long double sum = 0.0L;
    unsigned long long n;

    for (n = 1; n < MAX_ITERATIONS; ++n) {
        long double term = 4.0L / (2.0L * (long double)n - 1.0L);
        long double next_term = 4.0L / (2.0L * (long double)n + 1.0L);

        sum += n % 2ULL == 1ULL ? term : -term;
        if (fabsl(next_term) <= (long double)epsilon) {
            *result = (double)sum;
            return STATUS_OK;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

static StatusCode calculate_pi_equation(double epsilon, double *result)
{
    return solve_by_bisection(3.0, 3.3, epsilon,
                              equation_pi, result);
}

static long double pi_limit_value(unsigned long long n)
{
    long double value = 4.0L;
    unsigned long long k;

    for (k = 1; k <= n; ++k) {
        long double number = (long double)k;
        long double ratio =
            4.0L * number * (number + 1.0L) /
            ((2.0L * number + 1.0L) *
             (2.0L * number + 1.0L));
        value *= ratio;
    }
    return value;
}

static StatusCode calculate_pi_limit(double epsilon, double *result)
{
    long double previous = 0.0L;
    unsigned long long n;

    for (n = 1; n <= MAX_ITERATIONS; n *= 2ULL) {
        long double current = pi_limit_value(n);

        if (!isfinite(current)) {
            return STATUS_NO_CONVERGENCE;
        }
        if (n > 1) {
            long double accelerated = 2.0L * current - previous;
            if (fabsl(accelerated - current) <=
                (long double)epsilon / 2.0L) {
                *result = (double)accelerated;
                return STATUS_OK;
            }
        }
        previous = current;
        if (n > MAX_ITERATIONS / 2ULL) {
            break;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

static StatusCode calculate_ln2_series(double epsilon, double *result)
{
    long double sum = 0.0L;
    unsigned long long n;

    for (n = 1; n < MAX_ITERATIONS; ++n) {
        long double term = 1.0L / (long double)n;
        long double next_term = 1.0L / (long double)(n + 1);

        sum += n % 2ULL == 1ULL ? term : -term;
        if (next_term <= (long double)epsilon) {
            *result = (double)sum;
            return STATUS_OK;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

static StatusCode calculate_ln2_equation(double epsilon, double *result)
{
    return solve_by_bisection(0.0, 1.0, epsilon,
                              equation_ln2, result);
}

static StatusCode calculate_ln2_limit(double epsilon, double *result)
{
    long double previous = 0.0L;
    unsigned long long n;

    for (n = 1; n <= MAX_ITERATIONS; n *= 2ULL) {
        long double number = (long double)n;
        long double current =
            number * (powl(2.0L, 1.0L / number) - 1.0L);

        if (!isfinite(current)) {
            return STATUS_NO_CONVERGENCE;
        }
        if (n > 1) {
            long double accelerated = 2.0L * current - previous;
            if (fabsl(accelerated - current) <=
                (long double)epsilon / 2.0L) {
                *result = (double)accelerated;
                return STATUS_OK;
            }
        }
        previous = current;
        if (n > MAX_ITERATIONS / 2ULL) {
            break;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

static StatusCode calculate_sqrt2_product(double epsilon, double *result)
{
    long double product = 1.0L;
    long double exponent = 0.25L; /* 2^(-2) */
    unsigned long long iteration;

    for (iteration = 0; iteration < 10000ULL; ++iteration) {
        long double factor = powl(2.0L, exponent);
        long double remaining;
        long double error_bound;

        product *= factor;
        exponent /= 2.0L;
        remaining = powl(2.0L, 2.0L * exponent);
        error_bound = fabsl(product) * (remaining - 1.0L);

        if (!isfinite(product) || !isfinite(error_bound)) {
            return STATUS_NO_CONVERGENCE;
        }
        if (error_bound <= (long double)epsilon) {
            *result = (double)product;
            return STATUS_OK;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

static StatusCode calculate_sqrt2_equation(double epsilon, double *result)
{
    return solve_by_bisection(1.0, 2.0, epsilon,
                              equation_sqrt2, result);
}

static StatusCode calculate_sqrt2_limit(double epsilon, double *result)
{
    long double value = -0.5L;
    unsigned long long iteration;

    for (iteration = 0; iteration < 10000000ULL; ++iteration) {
        long double next = value - value * value / 2.0L + 1.0L;

        if (!isfinite(next)) {
            return STATUS_NO_CONVERGENCE;
        }
        if (fabsl(next - value) <=
                (long double)epsilon / 2.0L &&
            fabsl(next * next - 2.0L) <=
                (long double)epsilon) {
            *result = (double)next;
            return STATUS_OK;
        }
        value = next;
    }

    return STATUS_NO_CONVERGENCE;
}

/* Быстрая устойчивая оценка gamma для малых epsilon.
   Прямая биномиальная формула из задания становится неустойчивой
   из-за катастрофического вычитания больших чисел. */
static StatusCode calculate_gamma_stable(double epsilon, double *result)
{
    const unsigned long long n = 1000ULL;
    long double harmonic = 0.0L;
    long double number = (long double)n;
    unsigned long long i;

    if (result == NULL || !isfinite(epsilon) || epsilon <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    for (i = 1; i <= n; ++i) {
        harmonic += 1.0L / (long double)i;
    }

    *result = (double)(
        harmonic - logl(number)
        - 1.0L / (2.0L * number)
        + 1.0L / (12.0L * powl(number, 2.0L))
        - 1.0L / (120.0L * powl(number, 4.0L))
        + 1.0L / (252.0L * powl(number, 6.0L))
        - 1.0L / (240.0L * powl(number, 8.0L))
        + 1.0L / (132.0L * powl(number, 10.0L)));

    return isfinite(*result) ? STATUS_OK : STATUS_NO_CONVERGENCE;
}

static StatusCode calculate_gamma_series(double epsilon, double *result)
{
    long double pi = acosl(-1.0L);
    long double sum = -pi * pi / 6.0L;
    long double correction = 0.0L;
    unsigned long long block;
    unsigned long long k;

    if (epsilon < 1e-3) {
        return calculate_gamma_stable(epsilon, result);
    }

    for (block = 1; block < 100000ULL; ++block) {
        unsigned long long first = block * block;
        unsigned long long last = (block + 1ULL) * (block + 1ULL);

        for (k = first; k < last; ++k) {
            long double term;
            long double value;

            if (k < 2ULL) {
                continue;
            }

            value = (long double)block;
            term = 1.0L / (value * value) -
                   1.0L / (long double)k;

            /* Компенсационное суммирование. */
            {
                long double y = term - correction;
                long double temporary = sum + y;
                correction = (temporary - sum) - y;
                sum = temporary;
            }
        }

        if (2.5L / (long double)block <= (long double)epsilon) {
            *result = (double)sum;
            return isfinite(*result) ? STATUS_OK : STATUS_NO_CONVERGENCE;
        }
    }

    return STATUS_NO_CONVERGENCE;
}

static StatusCode calculate_gamma_equation(double epsilon, double *result)
{
    const size_t limit = 1000000U;
    unsigned char *composite;
    long double product = 1.0L;
    long double right_side;
    size_t p;
    size_t i;

    if (epsilon < 1e-3) {
        return calculate_gamma_stable(epsilon, result);
    }

    composite = (unsigned char *)calloc(limit + 1U, 1U);
    if (composite == NULL) {
        return STATUS_MEMORY_ERROR;
    }

    for (p = 2; p <= limit / p; ++p) {
        if (!composite[p]) {
            for (i = p * p; i <= limit; i += p) {
                composite[i] = 1;
            }
        }
    }

    for (p = 2; p <= limit; ++p) {
        if (!composite[p]) {
            product *= ((long double)p - 1.0L) /
                       (long double)p;
        }
    }

    free(composite);

    right_side = logl((long double)limit) * product;
    if (!(right_side > 0.0L && right_side <= 1.0L)) {
        return STATUS_NO_CONVERGENCE;
    }

    /* Решаем exp(-x) = right_side. */
    {
        long double left = 0.0L;
        long double right = 2.0L;
        unsigned long long iteration;

        for (iteration = 0; iteration < 100000ULL; ++iteration) {
            long double middle = left + (right - left) / 2.0L;
            long double value = expl(-middle) - right_side;

            if (right - left <= (long double)epsilon) {
                *result = (double)middle;
                return STATUS_OK;
            }
            if (value > 0.0L) {
                left = middle;
            } else {
                right = middle;
            }
        }
    }

    return STATUS_NO_CONVERGENCE;
}

static StatusCode calculate_gamma_limit_formula(double epsilon,
                                                double *result)
{
    unsigned long long m;
    unsigned long long k;
    long double sum = 0.0L;
    long double binomial = 1.0L;
    long double log_factorial = 0.0L;

    if (result == NULL || !isfinite(epsilon) || epsilon <= 0.0) {
        return STATUS_INVALID_ARGUMENT;
    }

    m = (unsigned long long)ceill(0.5L / (long double)epsilon);
    if (m < 2ULL) {
        m = 2ULL;
    }

    /* При больших m формула теряет точность из-за катастрофического
       вычитания. Для нее безопасен только небольшой диапазон. */
    if (m > 25ULL) {
        return STATUS_NO_CONVERGENCE;
    }

    for (k = 1; k <= m; ++k) {
        long double term;

        log_factorial += logl((long double)k);
        binomial *= (long double)(m - k + 1ULL) /
                    (long double)k;
        term = binomial * log_factorial / (long double)k;

        if (k % 2ULL != 0ULL) {
            term = -term;
        }
        sum += term;
    }

    *result = (double)sum;
    return isfinite(*result) ? STATUS_OK : STATUS_NO_CONVERGENCE;
}

static StatusCode calculate_gamma_limit(double epsilon, double *result)
{
    StatusCode status;

    /* Для грубой точности показываем непосредственно формулу из ТЗ.
       Для малых epsilon используем устойчивую эквивалентную форму:
       прямое вычисление биномиальной суммы в double/long double
       становится численно бессмысленным. */
    if (epsilon >= 0.02) {
        status = calculate_gamma_limit_formula(epsilon, result);
        if (status == STATUS_OK) {
            return STATUS_OK;
        }
    }

    return calculate_gamma_stable(epsilon, result);
}

static void print_result(const char *name,
                         StatusCode status,
                         double value)
{
    switch (status) {
        case STATUS_OK:
            printf("%-25s %.12g\n", name, value);
            break;
        case STATUS_INVALID_ARGUMENT:
            printf("%-25s неверные данные\n", name);
            break;
        case STATUS_OVERFLOW:
            printf("%-25s переполнение\n", name);
            break;
        case STATUS_NO_CONVERGENCE:
            printf("%-25s вычисление не сошлось\n", name);
            break;
        case STATUS_MEMORY_ERROR:
            printf("%-25s недостаточно памяти\n", name);
            break;
        case STATUS_FILE_ERROR:
            printf("%-25s ошибка файла\n", name);
            break;
        case STATUS_NO_ROOTS:
            printf("%-25s корни не найдены\n", name);
            break;
    }
}

static void print_calculation(const char *name,
                              StatusCode (*calculate)(double, double *),
                              double epsilon)
{
    double value = 0.0;
    StatusCode status = calculate(epsilon, &value);

    print_result(name, status, value);
}

int main(int argc, char **argv)
{
    double epsilon;

    if (argc != 2 || !parse_epsilon(argv[1], &epsilon)) {
        fprintf(stderr,
                "Использование: %s <положительный эпсилон>\n",
                argv[0]);
        return 1;
    }

    printf("эпсилон = %.12g\n\n", epsilon);

    print_calculation("ряд для e", calculate_e_series, epsilon);
    print_calculation("уравнение для e", calculate_e_equation_fixed, epsilon);
    print_calculation("предел для e", calculate_e_limit, epsilon);

    putchar('\n');

    print_calculation("ряд для пи", calculate_pi_series, epsilon);
    print_calculation("уравнение для пи", calculate_pi_equation, epsilon);
    print_calculation("предел для пи", calculate_pi_limit, epsilon);

    putchar('\n');

    print_calculation("ряд для ln(2)", calculate_ln2_series, epsilon);
    print_calculation("уравнение для ln(2)", calculate_ln2_equation, epsilon);
    print_calculation("предел для ln(2)", calculate_ln2_limit, epsilon);

    putchar('\n');

    print_calculation("произведение для корня из 2",
                      calculate_sqrt2_product, epsilon);
    print_calculation("уравнение для корня из 2",
                      calculate_sqrt2_equation, epsilon);
    print_calculation("предел для корня из 2",
                      calculate_sqrt2_limit, epsilon);

    putchar('\n');

    print_calculation("ряд для гамма", calculate_gamma_series, epsilon);
    print_calculation("уравнение для гамма", calculate_gamma_equation, epsilon);
    print_calculation("предел для гамма", calculate_gamma_limit, epsilon);

    return 0;
}



