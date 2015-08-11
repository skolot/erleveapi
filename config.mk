APP = erleveapi
erleveapi_DIR = .

DEPS ?= jsx cowlib ranch cowboy

jsx_SOURCE ?= "git@github.com:talentdeficit/jsx.git"
jsx_DIR ?= $(DEPSDST)/jsx
jsx_ERLCFLAGS ?= -Dmaps_support

cowlib_SOURCE ?= "git@github.com:ninenines/cowlib.git"
cowlib_DIR ?= $(DEPSDST)/cowlib
cowlib_GIT_CLONE_OPT ?= --branch 1.3.0

ranch_SOURCE ?= "git@github.com:ninenines/ranch.git"
ranch_DIR ?= $(DEPSDST)/ranch
ranch_GIT_CLONE_OPT ?= --branch 1.0.0

cowboy_SOURCE ?= "git@github.com:ninenines/cowboy.git"
cowboy_DIR ?= $(DEPSDST)/cowboy
