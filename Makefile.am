include build-aux/guile.am

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/ccache

SOURCES =					\
	sqlite3.scm

TESTS = 					\
	tests/basic.test
TEST_LOG_COMPILER = $(abs_top_builddir)/env $(GUILE) --no-auto-compile

EXTRA_DIST += $(TESTS)
