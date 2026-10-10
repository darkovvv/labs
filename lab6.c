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

static double cross_product(double ax, double ay, double bx, double by,
                            double cx, double cy)
{
    return (bx - ax) * (cy - ay) - (by - ay) * (cx - ax);
}

static int is_convex(double epsilon, int count, ...)
{
    double x[100];
    double y[100];
    va_list args;
    int sign = 0;
    int i;

    if (!isfinite(epsilon) || epsilon <= 0.0 || count < 3 || count > 100) {
        return -1;
    }
    va_start(args, count);
    for (i = 0; i < count; ++i) {
        x[i] = va_arg(args, double);
        y[i] = va_arg(args, double);
        if (!isfinite(x[i]) || !isfinite(y[i])) {
            va_end(args);
            return -1;
        }
    }
    va_end(args);

    // Проверяем смену знака векторного произведения.
    for (i = 0; i < count; ++i) {
        int next = (i + 1) % count;
        int after = (i + 2) % count;
        double cross = cross_product(x[i], y[i], x[next], y[next],
                                     x[after], y[after]);

        if (cross > epsilon) {
            if (sign < 0) {
                return 0;
            }
            sign = 1;
        } else if (cross < -epsilon) {
            if (sign > 0) {
                return 0;
            }
            sign = -1;
        }
    }
    return sign != 0;
}

static int polynomial(double point, int degree, double *answer, ...)
{
    va_list args;
    double value;
    int i;

    if (answer == NULL || !isfinite(point) || degree < 0 || degree > 100) {
        return 0;
    }
    va_start(args, answer);
    value = va_arg(args, double);
    for (i = degree - 1; i >= 0; --i) {
        value = value * point + va_arg(args, double);
    }
    va_end(args);
    if (!isfinite(value)) {
        return 0;
    }
    *answer = value;
    return 1;
}

static int digit_value(char symbol)
{
    if (symbol >= '0' && symbol <= '9') {
        return symbol - '0';
    }
    if (symbol >= 'A' && symbol <= 'Z') {
        return symbol - 'A' + 10;
    }
    if (symbol >= 'a' && symbol <= 'z') {
        return symbol - 'a' + 10;
    }
    return -1;
}

static int number_from_string(const char *text, int base,
                              unsigned long long *number)
{
    unsigned long long value = 0;
    size_t i;

    if (text == NULL || text[0] == '\0' || number == NULL ||
        base < 2 || base > 36) {
        return 0;
    }
    for (i = 0; text[i] != '\0'; ++i) {
        int digit = digit_value(text[i]);
        if (digit < 0 || digit >= base ||
            value > (ULLONG_MAX - (unsigned long long)digit) /
                    (unsigned long long)base) {
            return 0;
        }
        value = value * (unsigned long long)base + (unsigned long long)digit;
    }
    *number = value;
    return 1;
}

static int is_kaprekar(const char *text, int base)
{
    unsigned long long number;
    unsigned long long original;
    unsigned long long square;
    unsigned long long divisor = 1;
    int length = 0;
    int i;

    if (!number_from_string(text, base, &number)) {
        return -1;
    }
    if (number != 0 && number > ULLONG_MAX / number) {
        return -1;
    }
    original = number;
    square = number * number;
    do {
        ++length;
        number /= (unsigned)base;
    } while (number != 0);

    for (i = 0; i < length; ++i) {
        if (divisor > ULLONG_MAX / (unsigned)base) {
            return -1;
        }
        divisor *= (unsigned)base;
    }
    return square / divisor + square % divisor == original;
}

static int find_kaprekar(int base, const char **result, int count, ...)
{
    va_list args;
    int found = 0;
    int i;

    if (base < 2 || base > 36 || count < 0 || result == NULL) {
        return -1;
    }
    va_start(args, count);
    for (i = 0; i < count; ++i) {
        const char *text = va_arg(args, const char *);
        int kaprekar = is_kaprekar(text, base);
        if (kaprekar < 0) {
            va_end(args);
            return -1;
        }
        if (kaprekar) {
            result[found++] = text;
        }
    }
    va_end(args);
    return found;
}

static int geometric_mean(double *answer, int count, ...)
{
    va_list args;
    double product = 1.0;
    int i;

    if (answer == NULL || count <= 0) {
        return 0;
    }
    va_start(args, count);
    for (i = 0; i < count; ++i) {
        double value = va_arg(args, double);
        if (!isfinite(value) || value <= 0.0) {
            va_end(args);
            return 0;
        }
        product *= value;
        if (!isfinite(product)) {
            va_end(args);
            return 0;
        }
    }
    va_end(args);
    *answer = pow(product, 1.0 / count);
    return isfinite(*answer);
}

static int fast_power_positive(double base, unsigned long long exponent,
                               double *answer)
{
    double half;

    if (answer == NULL) {
        return 0;
    }
    if (exponent == 0) {
        *answer = 1.0;
        return 1;
    }
    if (!fast_power_positive(base, exponent / 2, &half)) {
        return 0;
    }
    *answer = half * half;
    if (!isfinite(*answer)) {
        return 0;
    }
    if (exponent % 2 == 1) {
        *answer *= base;
    }
    return isfinite(*answer);
}

static int fast_power(double base, long long exponent, double *answer)
{
    unsigned long long positive_exponent;
    double value;

    if (answer == NULL || !isfinite(base) ||
        (base == 0.0 && exponent < 0)) {
        return 0;
    }
    if (exponent < 0) {
        if (exponent == LLONG_MIN) {
            positive_exponent = (unsigned long long)LLONG_MAX + 1;
        } else {
            positive_exponent = (unsigned long long)(-exponent);
        }
    } else {
        positive_exponent = (unsigned long long)exponent;
    }
    if (!fast_power_positive(base, positive_exponent, &value)) {
        return 0;
    }
    *answer = exponent < 0 ? 1.0 / value : value;
    return isfinite(*answer);
}

typedef double (*Equation)(double);

static StatusCode bisection(double left, double right, double epsilon,
                            Equation function, double *root)
{
    double left_value;
    double right_value;
    int i;

    if (root == NULL || function == NULL || !isfinite(left) ||
        !isfinite(right) || !isfinite(epsilon) || epsilon <= 0.0 ||
        left >= right) {
        return STATUS_INVALID_ARGUMENT;
    }
    left_value = function(left);
    right_value = function(right);
    if (!isfinite(left_value) || !isfinite(right_value)) {
        return STATUS_NO_CONVERGENCE;
    }
    if ((left_value > 0.0 && right_value > 0.0) ||
        (left_value < 0.0 && right_value < 0.0)) {
        return STATUS_NO_ROOTS;
    }

    for (i = 0; i < 100000; ++i) {
        double middle = left + (right - left) / 2.0;
        double middle_value = function(middle);

        if (!isfinite(middle_value)) {
            return STATUS_NO_CONVERGENCE;
        }
        if (fabs(middle_value) <= epsilon || right - left <= epsilon) {
            *root = middle;
            return STATUS_OK;
        }
        if ((left_value <= 0.0 && middle_value >= 0.0) ||
            (left_value >= 0.0 && middle_value <= 0.0)) {
            right = middle;
        } else {
            left = middle;
            left_value = middle_value;
        }
    }
    return STATUS_NO_CONVERGENCE;
}

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

static void print_status(const char *name, StatusCode status, double value)
{
    switch (status) {
        case STATUS_OK:
            printf("%s: %.10g\n", name, value);
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
            printf("%s: корень не найден\n", name);
            break;
    }
}

int main(void)
{
    double answer = 0.0;
    int convex;
    const char *kaprekar[8];
    int kaprekar_count;
    StatusCode status;

    convex = is_convex(1e-9, 4,
                       0.0, 0.0, 4.0, 0.0,
                       4.0, 3.0, 0.0, 3.0);
    if (convex < 0) {
        print_status("1. Многоугольник", STATUS_INVALID_ARGUMENT, 0.0);
    } else {
        puts(convex ? "1. Многоугольник выпуклый"
                    : "1. Многоугольник невыпуклый");
    }

    status = polynomial(2.0, 3, &answer, 1.0, -2.0, 0.0, 5.0)
                 ? STATUS_OK : STATUS_INVALID_ARGUMENT;
    print_status("2. Значение многочлена", status, answer);

    printf("3. Числа Капрекара: ");
    kaprekar_count = find_kaprekar(10, kaprekar, 8,
                                   "1", "9", "10", "45",
                                   "55", "99", "100", "101");
    if (kaprekar_count < 0) {
        print_status("ошибка", STATUS_INVALID_ARGUMENT, 0.0);
    } else {
        int i;
        for (i = 0; i < kaprekar_count; ++i) {
            printf("%s%s", i == 0 ? "" : " ", kaprekar[i]);
        }
        putchar('\n');
    }

    status = geometric_mean(&answer, 4, 1.0, 2.0, 4.0, 8.0)
                 ? STATUS_OK : STATUS_INVALID_ARGUMENT;
    print_status("4. Среднее геометрическое", status, answer);

    status = fast_power(2.0, -5, &answer)
                 ? STATUS_OK : STATUS_INVALID_ARGUMENT;
    print_status("5. Значение степени", status, answer);

    status = bisection(0.0, 2.0, 1e-9, equation_square, &answer);
    print_status("6a. x^2 - 2", status, answer);
    status = bisection(0.0, 1.0, 1e-9, equation_cosine, &answer);
    print_status("6b. cos(x) - x", status, answer);
    status = bisection(0.0, 2.0, 1e-9, equation_exponential, &answer);
    print_status("6c. exp(x) - 3", status, answer);
    return 0;
}

