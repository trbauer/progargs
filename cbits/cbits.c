#ifdef _WIN32
#include <Windows.h>

int get_console_width()
{
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    if (!GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE),&csbi)) {
        return 0;
    }
    return csbi.dwSize.X;
}
#else
#include <unistd.h>
#include <sys/ioctl.h>

int get_console_width()
{
    struct winsize w;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) != 0) {
        return 0;
    }
    return w.ws_col;
}
#endif

