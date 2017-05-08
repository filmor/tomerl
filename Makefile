#!/usr/bin/make -f

#-----------------------------------------------------------------------------

ifeq ($(wildcard .*.plt),)
#DIALYZER_PLT = ~/.dialyzer_plt
else
DIALYZER_PLT = ~/.dialyzer_plt $(wildcard .*.plt)
endif
DIALYZER_OPTS = --no_check_plt $(if $(DIALYZER_PLT),--plts $(DIALYZER_PLT))

APP_VERSION = $(shell _install/app_version ebin/$(PROJECT).app)
BEAM_INSTALL_ROOT = $(shell _install/lib_dir)
DOCDIR = /usr/share/doc/erlang-$(PROJECT)

#-----------------------------------------------------------------------------

PROJECT = toml
ERLC_OPTS =

include erlang.mk

src/toml_lexer.erl::
	$(verbose)grep -q @private $@ || sed -i -e '1i%%% @private' $@

#-----------------------------------------------------------------------------

.PHONY: dialyzer
YECC_ERL_FILES = $(subst .yrl,.erl,$(subst .xrl,.erl,$(wildcard src/*.[xy]rl)))
ERL_SOURCE_FILES = $(filter-out $(YECC_ERL_FILES),$(wildcard src/*.erl))
dialyzer:
	@echo "dialyzer $(strip $(DIALYZER_OPTS)) --src src/*.erl"
	@dialyzer $(strip $(DIALYZER_OPTS)) --src $(ERL_SOURCE_FILES)

#-----------------------------------------------------------------------------

.PHONY: doc
doc: edoc

#-----------------------------------------------------------------------------

.PHONY: install install-doc install-erlang

install: install-doc install-erlang

install-erlang: app
	$(foreach F,$(wildcard ebin/*),$(call install,644,$F,$(DESTDIR)$(BEAM_INSTALL_ROOT)/$(PROJECT)-$(APP_VERSION)))

install-doc: edoc
	mkdir -p $(DESTDIR)$(DOCDIR)/html
	cp doc/* $(DESTDIR)$(DOCDIR)/html

define install
install -D -m $1 $2 $3/$2

endef

#-----------------------------------------------------------------------------
# vim:ft=make
