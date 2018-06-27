include build-aux/guile.am

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

SOURCES =					\
	sqlite3.scm

TESTS = 					\
	tests/basic.scm

TEST_EXTENSIONS = .scm

SCM_LOG_DRIVER =						\
  $(top_builddir)/env $(GUILE) --no-auto-compile -e main	\
      $(top_srcdir)/build-aux/test-driver.scm

EXTRA_DIST +=					\
  $(TESTS) build-aux/test-driver.scm		\
  COPYING.LESSER
