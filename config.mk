APP = erleveapi
erleveapi_DIR = .

DEPS ?= jsx erleveapi_parser

jsx_SOURCE ?= "git@github.com:talentdeficit/jsx.git"
jsx_DIR ?= $(DEPSDST)/jsx
jsx_ERLCFLAGS ?= -Dmaps_support

erleveapi_parser_SOURCE ?= "git@github.com:skolot/erleveapi_parser.git"
erleveapi_parser_DIR ?= $(DEPSDST)/erleveapi_parser

