
sources = $(shell find . -name '*hs')
bin			= dist/build/hTags/hTags
setup		= dist/setup-config

include ../../mk/cabal.mk

.PHONY : default

default : $(bin)

$(setup) : hTags.cabal
	$(CABAL_CMD) $(CABAL_INSTALL_CMD) --only-dependencies
	$(CABAL_CMD) configure

$(bin) : $(setup) $(sources)
	$(CABAL_CMD) $(CABAL_BUILD_CMD)

