dnl -*- Autoconf -*-

AC_INIT([Guile-SQLite3], [0.1.0],
  [guile-user@gnu.org], [guile-sqlite3],
  [https://notabug.org/civodul/guile-sqlite3])

AC_CONFIG_SRCDIR([sqlite3.scm.in])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability])
AM_SILENT_RULES([yes])

dnl We require pkg.m4 (from pkg-config) and guile.m4 (from Guile.)
dnl Make sure they are available.
m4_pattern_forbid([PKG_CHECK_MODULES])
m4_pattern_forbid([GUILE_PKG])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS

PKG_CHECK_MODULES([SQLITE], [sqlite3])
SQLITE_LIBDIR="`"$PKG_CONFIG" sqlite3 --variable libdir`"
AC_SUBST([SQLITE_LIBDIR])

if test "$cross_compiling" = "no"; then
   AC_MSG_CHECKING([whether '$SQLITE_LIBDIR/libsqlite3' is usable])
   GUILE_CHECK([retval],
   [(dynamic-func \"sqlite3_open_v2\"
                 (dynamic-link \"$SQLITE_LIBDIR/libsqlite3\"))])
   if test "$retval" != 0; then
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([failed to load '$SQLITE_LIBDIR/libsqlite3'])
   else
      AC_MSG_RESULT([yes])
   fi
else
   GUILE_TARGET="--target=$host_alias"
   AC_SUBST([GUILE_TARGET])
fi

AC_CONFIG_FILES([Makefile sqlite3.scm])
AC_CONFIG_FILES([env], [chmod +x env])

AC_OUTPUT
