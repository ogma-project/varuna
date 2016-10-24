BUNDLE   = bundle
COMMIT   = $(shell git rev-parse --short HEAD)
INDEX    = ${BUNDLE}/index.html
# purescript
SRC_PURS  = $(shell find src -name '*.purs')
JS_SRC    = $(shell find src -name '*.js')
# varuna
VARUNA_NAME   = js/${COMMIT}-varuna.js
VARUNA_TARGET = ${BUNDLE}/${VARUNA_NAME}
# jquery
JQUERY_VERSION = $(shell cat package.json|grep jquery|sed -e 's/.*: "^//' -e 's/", *//')
JQUERY_NAME    = js/${JQUERY_VERSION}-jquery.min.js
JQUERY_TARGET  = ${BUNDLE}/${JQUERY_NAME}
JQUERY_SRC     = $(shell find . -name jquery.min.js)
# bootstrap
BOOTSTRAP_VERSION    = $(shell cat package.json|grep bootstrap|sed -e 's/.*: "^//' -e 's/", *//')
BOOTSTRAPCSS_NAME    = css/${BOOTSTRAP_VERSION}-bootstrap.min.css
BOOTSTRAPCSS_TARGET  = ${BUNDLE}/${BOOTSTRAPCSS_NAME}
BOOTSTRAPCSS_SRC     = $(shell find . -name bootstrap.min.css)
BOOTSTRAPJS_NAME     = js/${BOOTSTRAP_VERSION}-bootstrap.min.js
BOOTSTRAPJS_TARGET   = ${BUNDLE}/${BOOTSTRAPJS_NAME}
BOOTSTRAPJS_SRC      = $(shell find . -name bootstrap.min.js)
# sass
SCSS_SRC       = $(shell find assets/sass/ -name '*.sass')
SCSS_MAIN      = assets/sass/varuna.sass
CSS_NAME       = css/${COMMIT}-varuna.css
CSS_TARGET     = ${BUNDLE}/${CSS_NAME}

# use npm binaries
export PATH := $(shell npm bin):${PATH}

# build rules
build: ${VARUNA_TARGET} .assets ${INDEX} ${CSS_TARGET}
	@$(call done)

${CSS_TARGET}: ${SCSS_SRC}
	@$(call build_echo,varuna.css)
	@mkdir -p bundle/css
	@node-sass --quiet ${SCSS_MAIN} $@

.assets: ${JQUERY_SRC} ${BOOTSTRAPCSS_SRC} ${BOOTSTRAPJS_SRC}
	@$(call build_echo,assets)
	@mkdir -p $(BUNDLE)/js
	@cp ${JQUERY_SRC} ${JQUERY_TARGET}
	@cp ${BOOTSTRAPJS_SRC} ${BOOTSTRAPJS_TARGET}
	@mkdir -p $(BUNDLE)/css
	@cp ${BOOTSTRAPCSS_SRC} ${BOOTSTRAPCSS_TARGET}
	@touch .assets

${INDEX}: assets/html/index.html ${VARUNA_TARGET}
	@$(call build_echo,index.html)
	@sed -e 's/@VARUNAJS@/$(subst /,\/,${VARUNA_NAME})/' \
	     -e 's/@VARUNACSS@/$(subst /,\/,${CSS_NAME})/' \
	     -e 's/@BOOTSTRAPCSS@/$(subst /,\/,${BOOTSTRAPCSS_NAME})/' \
	     -e 's/@BOOTSTRAPJS@/$(subst /,\/,${BOOTSTRAPJS_NAME})/' \
	     -e 's/@JQUERYJS@/$(subst /,\/,${JQUERY_NAME})/' $< > $@

${VARUNA_TARGET}: ${SRC_PURS} ${JS_SRC} .bower .npm
	@$(call build_echo,varuna.js)
	@mkdir -p $(BUNDLE)/js
	@pulp browserify -O --to $@

.bower: bower.json
	@$(call deps_echo,bower)
	@bower install --quiet
	@touch .bower

.npm: package.json
	@$(call deps_echo,npm)
	@npm --quiet install
	@touch .npm

clean:
	@$(call rm_echo,built bundle)
	@rm -rf bundle/*
	@rm -f .assets

mrproper: clean
	@$(call rm_echo, node modules)
	@rm -rf ./node_modules
	@rm -f .npm
	@$(call rm_echo, bower components)
	@rm -rf ./bower_components
	@rm -f .bower

dev-env:
	@$(call deps_echo,install git hooks)
	@ln -sf ../../.template/hooks/commit-msg .git/hooks/commit-msg

electron:
	@$(call color_echo,${BLUE},spawn an electron instance)
	@electron .

firefox:
	@$(call color_echo,${BLUE},spawn firefox)
	@firefox ${INDEX}

.PHONY: clean mrproper build dev-env electron firefox

# colored print functions
define color_echo
	echo -e $(1)$(2)$(DEFAULT)
endef

define build_echo
	$(call color_echo,${YELLOW},build: ${1})
endef

define rm_echo
	$(call color_echo,${RED},rm: ${1})
endef

define deps_echo
	$(call color_echo,${BLUE},deps: ${1})
endef

define done
	$(call color_echo,${GREEN},done)
endef

GREEN = "\e[32m"
YELLOW = "\e[33m"
WHITE = "\e[37m"
RED = "\e[31m"
BLUE = "\e[94m"
DEFAULT = "\e[0m"
