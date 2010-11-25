moddir=$(prefix)/share/guile/site/2.0

SOURCES =					\
	sqlite3.scm
GOBJECTS = $(SOURCES:%.scm=%.go)
nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(GOBJECTS)
CLEANFILES = $(GOBJECTS)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

TESTS = 					\
	tests/basic.test
TESTS_ENVIRONMENT = $(abs_top_builddir)/env $(GUILE) --no-autocompile

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES) $(TESTS)
