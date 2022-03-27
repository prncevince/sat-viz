# needs GNU Make 4.3 for grouped targets 
.PHONY: all renv

ifeq ($(OS), Windows_NT)          # is Windows_NT on XP, 2000, 7, Vista, 10...
detected_OS := Windows
else
detected_OS = $(shell uname)
endif

ifeq ($(detected_OS), Windows)
TMP := $(TEMP)
TYPE := .tar.gz
PATTERN := *[0-9]$(TYPE)
endif

ifeq ($(detected_OS), Darwin)     # Mac OS X
TMP = $(TMPDIR)
TYPE := .tgz
PATTERN := *$(TYPE)
endif

ifeq ($(detected_OS), Linux)      # Ubuntu 18.04
TMP := /tmp
TYPE := .tar.gz
PATTERN := *gnu$(TYPE)
endif

NODE_PATHS = node_modules/hdf5-r/$(PATTERN)
RENV_PATHS = renv/local/$(PATTERN)

all: renv

clean:
	rm -f $(RENV_PATHS)
	mkdir -p renv/local

$(NODE_PATHS): clean

$(RENV_PATHS) &: $(NODE_PATHS)
	cp $? renv/local

renv: $(RENV_PATHS)
	Rscript --no-init-file \
	-e "if (! 'renv' %in% rownames(installed.packages())) install.packages('renv')"
	Rscript --no-init-file -e 'renv::consent(TRUE)'
ifdef DOCKER
	Rscript --no-init-file -e "d <- 'renv/local/'; renv::install(paste0(d, grep('Rhdf5', list.files(d), value = T)))"
	Rscript --no-init-file -e "d <- 'renv/local/'; renv::install(paste0(d, grep('rhdf5', list.files(d), value = T)))"
endif
