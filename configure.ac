dnl -*- Autoconf -*-

AC_INIT(sqlite3, 0.1.0)
AC_CONFIG_SRCDIR(sqlite3.scm)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability])
AM_SILENT_RULES([yes])

GUILE_PROGS([2.2 2.0])
AC_SUBST([GUILE_EFFECTIVE_VERSION])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([env], [chmod +x env])

AC_OUTPUT
