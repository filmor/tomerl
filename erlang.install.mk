#!/usr/bin/make -f
#
# Installation helper for Erlang projects.
# Intended to be used along with erlang.mk.
#
#-----------------------------------------------------------------------------
# variables to be used in main makefile or to be set for functions in next
# sections

ERL_LIB_DIR = $(shell $(ERL) -noinput -eval ' \
	io:put_chars([code:lib_dir(), "\n"]), \
	halt(). \
')
ESCRIPT_PATH = /usr/bin/escript

#-----------------------------------------------------------------------------
# information reading helpers

# APP_VERSION = $(call app-version,ebin/$(PROJECT).app)
define app-version
$(shell $(ERL) -eval ' \
	{ok, AppFile} = init:get_argument(app_file), \
	{ok, [{application,_,KeyList}]} = file:consult(AppFile), \
	io:put_chars([proplists:get_value(vsn, KeyList), "\n"]), \
	halt(). \
' -app_file $1)
endef

#-----------------------------------------------------------------------------
# build rule helpers

inst_silent_0 = true
inst_silent = $(inst_silent_$(V))

# $(call install-wildcard,$(MODE),$(WILDCARD),$(DESTDIR))
define install-wildcard
$(if $(inst_silent),@)mkdir -p $3
$(foreach F,$(wildcard $2),$(call install,$1,$F,$3,skip_mkdir))
endef

# $(call install,$(MODE),$(FILE),$(DESTDIR))
#
# Note that this function preserves the filename. If you need to install the
# file under a different name (e.g. "*.example"), use /usr/bin/install
# directly.
define install
$(if $4,,$(if $(inst_silent),@)mkdir -p $3)
$(if $(inst_silent),@echo " INSTALL " $3/$(notdir $2); )install -m $1 $2 $3/$(notdir $2)

endef

# $(call install-escript,$(FILE),$(DESTFILE),$(BEAM_OPTS))
#
# Remember to set in $(BEAM_OPTS) at least "-args_file ...", so the VM args
# can be overriden.
define install-escript
$(if $(inst_silent),@echo " ESCRIPT " $2)
$(if $(inst_silent),@)mkdir -p $(dir $2)
$(if $(inst_silent),@)printf '#!%s\n%%%%! %s\n' '$(ESCRIPT_PATH)' '$3' > $2
$(if $(inst_silent),@)head -n 3 $1 | sed -e '/^#!/d' -e '/^%*!/d' >> $2
$(if $(inst_silent),@)tail -n +4 $1 >> $2
$(if $(inst_silent),@)chmod 755 $2
endef

#-----------------------------------------------------------------------------
# vim:ft=make
