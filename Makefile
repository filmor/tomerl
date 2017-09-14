#!/usr/bin/make -f

#-----------------------------------------------------------------------------

ifeq ($(wildcard .*.plt),)
#DIALYZER_PLT = ~/.dialyzer_plt
else
DIALYZER_PLT = ~/.dialyzer_plt $(wildcard .*.plt)
endif
DIALYZER_OPTS = --no_check_plt $(if $(DIALYZER_PLT),--plts $(DIALYZER_PLT))

#-----------------------------------------------------------------------------

PROJECT = toml
APP_VERSION = $(call app-version,ebin/$(PROJECT).app)
ERL_INSTALL_LIB_DIR = $(ERL_LIB_DIR)/$(PROJECT)-$(APP_VERSION)
DOCDIR = /usr/share/doc/erlang-$(PROJECT)

ERLC_OPTS = +debug_info
EDOC_OPTS := {overview, "src/overview.edoc"}, \
             {source_path, ["src", "examples"]}, \
             todo
ifneq ($(devel),)
EDOC_OPTS := $(EDOC_OPTS), private
endif

include erlang.mk
include erlang.install.mk

src/toml_lexer.erl::
	$(verbose)if ! grep -q @private $@; then echo '%%% @private' > $@.erl_gen_tmp; cat $@ >> $@.erl_gen_tmp; cat $@.erl_gen_tmp > $@; rm -f $@.erl_gen_tmp; fi

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

.PHONY: install install-erlang install-doc

install: install-erlang install-doc

install-erlang: app
	$(call install-wildcard,644,ebin/*,$(DESTDIR)$(ERL_INSTALL_LIB_DIR)/ebin)

install-doc: edoc
	$(call install-wildcard,644,doc/*.html doc/*.png doc/*.css,$(DESTDIR)$(DOCDIR)/html)

#-----------------------------------------------------------------------------
# vim:ft=make
