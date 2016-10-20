SRCPURS  = $(shell find src -name '*.purs')
SRCJS    = $(shell find src -name '*.js')
VARUNAJS = bundle/varuna.js

build:
	@npm run --silent build

${VARUNAJS}: ${SRCPURS} ${SRCJS} .bower .npm
	@$(call build_echo,varuna.js)
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
	@$(call rm_echo,${VARUNAJS})
	@rm -f ${VARUNAJS}

mrproper: clean
	@$(call rm_echo, node modules)
	@rm -rf ./node_modules
	@rm -f .npm
	@$(call rm_echo, bower components)
	@rm -rf ./bower_components
	@rm -f .bower

.PHONY: clean mrproper build

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

GREEN = "\e[32m"
YELLOW = "\e[33m"
WHITE = "\e[37m"
RED = "\e[31m"
BLUE = "\e[94m"
DEFAULT = "\e[0m"