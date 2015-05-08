MKINCDIR ?= $(PWD)

indir = .
outdir = .

SHELL = bash
GIT ?= git
ECHO ?= echo

export VERBOSE = @
export DEVNULL = >/dev/null
export GITSUPPRES = -q
SILENT = -s

ifeq "$(V)" "1"
VERBOSE =
DEVNULL =
SILENT = 
GITSUPPRES =
endif

DEPSDST ?= deps

DEPS ?= jsx erleveapi_parser
DEP_jsx_SOURCE ?= "git@github.com:talentdeficit/jsx.git"
DEP_erleveapi_parser_SOURCE ?= "git@github.com:skolot/erleveapi_parser.git"

APPS = .
SUBDIRS := $(addprefix $(DEPSDST)/,$(DEPS)) $(APPS) test

GET_DEPS = $(addprefix get-deps-, $(DEPS))
UPDATE_DEPS = $(addprefix update-deps-, $(DEPS))

all: prepare get-deps update-deps compile

$(GET_DEPS):
	$(VERBOSE)$(ECHO) "[get dep] $(subst get-deps-,,$@)"; \
		[ ! -r $(DEPSDST)/$(subst get-deps-,,$@)/.git ] && \
		$(GIT) clone $(GITSUPPRES) $(DEP_$(subst get-deps-,,$@)_SOURCE) $(DEPSDST)/$(subst get-deps-,,$@); exit 0

$(UPDATE_DEPS):
	$(VERBOSE)$(ECHO) "[update dep] $(subst update-deps-,,$@)"
	$(VERBOSE)[ -r $(DEPSDST)/$(subst update-deps-,,$@)/.git ] && \
		cd $(DEP_$(subst get-deps-,,$@)_SOURCE) $(DEPSDST)/$(subst update-deps-,,$@) && \
		$(GIT) pull $(GITSUPPRES) || \
		$(ECHO) "$(subst update-deps-,,$@) doesn't exist, please run \`make get-deps\' before"

get-deps: $(GET_DEPS)

update-deps: $(UPDATE_DEPS)


prepare: mkdir

mkdir: $(DEPSDST)

$(DEPSDST):
	$(VERBOSE)$(ECHO) "[mkdir] $@"
	$(VERBOSE)mkdir -p $@

clean compile:
	$(VERBOSE)cd $(indir) && \
	for d in $(SUBDIRS); do \
		$(MAKE) $(SILENT) -C $(indir)/$${d} -f $(MKINCDIR)/erlang.mk indir=$(indir)/$${d} outdir=$(outdir)/$${d} subdir=$${d} $@ || break; \
	done

test: compile
	$(VERBOSE)erl -noshell -pa ebin -eval 'eunit:test(eveapi_parser_SUITE)' -s init stop

.PHONY: get-deps update-deps prepare mkdir clean compile $(GET_DEPS) $(UPDATE_DEPS)
