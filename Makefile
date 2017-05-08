#!/usr/bin/make -f

#-----------------------------------------------------------------------------

ifeq ($(wildcard .*.plt),)
#DIALYZER_PLT = ~/.dialyzer_plt
else
DIALYZER_PLT = ~/.dialyzer_plt $(wildcard .*.plt)
endif
DIALYZER_OPTS = --no_check_plt $(if $(DIALYZER_PLT),--plts $(DIALYZER_PLT))

#BEAM_INSTALL_ROOT = <detected with $(ERL)>
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

APP_VERSION = $(shell $(ERL) -eval ' \
	{ok, AppFile} = init:get_argument(file), \
	{ok, [{application,_,KeyList}]} = file:consult(AppFile), \
	io:put_chars([proplists:get_value(vsn, KeyList), "\n"]), \
	halt(). \
' -file ebin/$(PROJECT).app)

BEAM_INSTALL_ROOT = $(shell $(ERL) -noinput -eval ' \
	io:put_chars([code:lib_dir(), "\n"]), \
	halt(). \
')

#-----------------------------------------------------------------------------
# vim:ft=make
