APP = erleveapi
erleveapi_DIR = .

DEPS ?= jsx 

jsx_SOURCE ?= "git@github.com:talentdeficit/jsx.git"
jsx_DIR ?= $(DEPSDST)/jsx
jsx_ERLCFLAGS ?= -Dmaps_support

