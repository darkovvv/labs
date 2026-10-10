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

static int parse_double(const char *text, double *value)
{
    char *end;
    double number;

    if (text == NULL || text[0] == '\0') {
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

static int parse_integer(const char *text, long long *value)
{
    char *end;
    long long number;

    if (text == NULL || text[0] == '\0') {
        return 0;
    }
    errno = 0;
    number = strtoll(text, &end, 10);
    if (errno == ERANGE || end == text || *end != '\0' || number == 0) {
        return 0;
    }
    *value = number;
    return 1;
}

static int is_flag(const char *text, char flag)
{
    return text != NULL && (text[0] == '-' || text[0] == '/') &&
           text[1] == flag && text[2] == '\0';
}

static int equal_double(double first, double second, double epsilon)
{
    return fabs(first - second) <= epsilon;
}

static StatusCode solve_quadratic(double a, double b, double c,
                                  double epsilon, double roots[2],
                                  int *root_count)
{
    double discriminant;

    if (roots == NULL || root_count == NULL || epsilon <= 0.0 ||
        !isfinite(a) || !isfinite(b) || !isfinite(c)) {
        return STATUS_INVALID_ARGUMENT;
    }
    *root_count = 0;
    if (fabs(a) <= epsilon) {
        if (fabs(b) <= epsilon) {
            if (fabs(c) <= epsilon) {
                *root_count = -1; /* для бесконечно много решений */
                return STATUS_OK;
            }
            return STATUS_NO_ROOTS;
        }
        roots[0] = -c / b;
        *root_count = 1;
        return STATUS_OK;
    }

    discriminant = b * b - 4.0 * a * c;
    if (!isfinite(discriminant)) {
        return STATUS_OVERFLOW;
    }
    if (discriminant > epsilon) {
        double root = sqrt(discriminant);
        roots[0] = (-b - root) / (2.0 * a);
        roots[1] = (-b + root) / (2.0 * a);
        *root_count = 2;
    } else if (fabs(discriminant) <= epsilon) {
        roots[0] = -b / (2.0 * a);
        *root_count = 1;
    }
    return STATUS_OK;
}

static int already_printed(double printed[][3], int count,
                           const double current[3], double epsilon)
{
    int row;
    int column;

    for (row = 0; row < count; ++row) {
        for (column = 0; column < 3; ++column) {
            if (!equal_double(printed[row][column], current[column], epsilon)) {
                break;
            }
        }
        if (column == 3) {
            return 1;
        }
    }
    return 0;
}

static void print_error(StatusCode status)
{
    switch (status) {
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

    if (is_flag(argv[1], 'q')) {
        const int permutations[6][3] = {
            {0, 1, 2}, {0, 2, 1}, {1, 0, 2},
            {1, 2, 0}, {2, 0, 1}, {2, 1, 0}
        };
        double epsilon;
        double coefficients[3];
        double printed[6][3];
        int printed_count = 0;
        int i;

        if (argc != 6 || !parse_double(argv[2], &epsilon) || epsilon <= 0.0) {
            fprintf(stderr, "Использование: %s -q <эпсилон> <a> <b> <c>\n", argv[0]);
            return 1;
        }
        for (i = 0; i < 3; ++i) {
            if (!parse_double(argv[i + 3], &coefficients[i])) {
                puts("Коэффициенты должны быть конечными действительными числами.");
                return 1;
            }
        }

        for (i = 0; i < 6; ++i) {
            double current[3];
            double roots[2];
            int root_count = 0;
            int j;
            StatusCode status;

            for (j = 0; j < 3; ++j) {
                current[j] = coefficients[permutations[i][j]];
            }
            if (already_printed(printed, printed_count, current, epsilon)) {
                continue;
            }
            for (j = 0; j < 3; ++j) {
                printed[printed_count][j] = current[j];
            }
            ++printed_count;

            printf("Уравнение %.10g*x^2%+.10g*x%+.10g = 0:\n",
                   current[0], current[1], current[2]);
            status = solve_quadratic(current[0], current[1], current[2],
                                     epsilon, roots, &root_count);
            switch (status) {
                case STATUS_OK:
                    if (root_count == -1) {
                        puts("Бесконечно много решений");
                    } else if (root_count == 0) {
                        puts("Действительных корней нет");
                    } else {
                        for (j = 0; j < root_count; ++j) {
                            double root = roots[j];
                            if (fabs(root) <= epsilon) {
                                root = 0.0;
                            }
                            printf("%.10g\n", root);
                        }
                    }
                    break;
                case STATUS_NO_ROOTS:
                    puts("Действительных корней нет");
                    break;
                default:
                    print_error(status);
                    return 1;
            }
        }
        return 0;
    }

    if (is_flag(argv[1], 'm')) {
        long long first;
        long long second;

        if (argc != 4 || !parse_integer(argv[2], &first) ||
            !parse_integer(argv[3], &second)) {
            fprintf(stderr, "Использование: %s -m <ненулевое целое> <ненулевое целое>\n", argv[0]);
            return 1;
        }
        puts(first % second == 0 ? "Первое число делится на второе."
                                  : "Первое число не делится на второе.");
        return 0;
    }

    if (is_flag(argv[1], 't')) {
        double epsilon;
        double side[3];
        double sum = 0.0;
        double longest_square;
        int longest = 0;
        int i;

        if (argc != 6 || !parse_double(argv[2], &epsilon) || epsilon <= 0.0) {
            fprintf(stderr, "Использование: %s -t <эпсилон> <сторона1> <сторона2> <сторона3>\n", argv[0]);
            return 1;
        }
        for (i = 0; i < 3; ++i) {
            if (!parse_double(argv[i + 3], &side[i]) || side[i] <= 0.0) {
                puts("Стороны треугольника должны быть положительными конечными числами.");
                return 1;
            }
            if (side[i] > side[longest]) {
                longest = i;
            }
        }
        for (i = 0; i < 3; ++i) {
            if (i != longest) {
                sum += side[i] * side[i];
            }
        }
        longest_square = side[longest] * side[longest];
        puts(equal_double(sum, longest_square, epsilon)
                 ? "Из этих сторон можно составить прямоугольный треугольник."
                 : "Из этих сторон нельзя составить прямоугольный треугольник.");
        return 0;
    }

    puts("Неизвестный флаг. Используйте -q, -m или -t.");
    return 1;
}