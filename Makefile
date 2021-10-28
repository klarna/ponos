## -*- mode: Makefile; fill-column: 80; comment-column: 67; -*-
REBAR ?= $(shell which rebar 2> /dev/null || which ./rebar)

DEPS_DIR = $(CURDIR)/deps

DIALYZER_OPTIONS := --fullpath --no_native  -Wunderspecs

ERLANG_DIALYZER_APPS := compiler \
                        crypto   \
                        erts     \
                        kernel   \
                        stdlib
DIALYZER_PLT         := ./dialyzer.plt

.PHONY: default
default: compile

.PHONY: all
all: compile test doc typecheck

########################################################################
# Compilation and Dependency Management

.PHONY: compile
compile: get-deps
	$(REBAR) compile

.PHONY: real-clean
real-clean: clean clean-doc clean-plt clean-deps

.PHONY: clean
clean:
	$(REBAR) clean

.PHONY: clean-doc
clean-doc:
	- rm -rf doc

.PHONY: clean-plt
clean-plt:
	- rm -rf $(DIALYZER_PLT) $(DEPS_PLT)

.PHONY: clean-deps
clean-deps:
	rm -rf deps

.PHONY: doc
doc:
	$(REBAR) skip_deps=true doc

.PHONY: get-deps
get-deps:
	$(REBAR) get-deps

########################################################################
# Test

.PHONY: test
test: compile xref eunit typecheck

.PHONY: eunit
eunit: compile
	$(REBAR) skip_deps=true eunit -v

.PHONY: ct
ct: compile
	$(REBAR) skip_deps=true ct

.PHONY: xref
xref:
	$(REBAR) xref


.PHONY: typecheck
typecheck: compile $(DIALYZER_PLT)
	dialyzer $(DIALYZER_OPTIONS) --plt $(DIALYZER_PLT) --src src

$(DIALYZER_PLT):
	dialyzer --build_plt --apps $(ERLANG_DIALYZER_APPS) \
		--output_plt $(@)

#####################################################################
# Relase

release_major: real-clean test
	./bin/release.sh major

release_minor: real-clean test
	./bin/release.sh minor

release: real-clean test
	./bin/release.sh patch
