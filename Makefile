## -*- mode: Makefile; fill-column: 80; comment-column: 67; -*-
PROJECT = ponos
PLT_APPS = compiler crypto

include erlang.mk

DIALYZER_OPTS += --fullpath --no_native -Wunderspecs
