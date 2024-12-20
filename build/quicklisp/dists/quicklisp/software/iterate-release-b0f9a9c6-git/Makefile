FILES = package.lisp iterate.lisp iterate-test.lisp iterate-pg.lisp iterate.asd
TEXFILES = doc/iter-man.tex doc/iter-bare.tex doc/aimemo.sty doc/GNUmakefile
RCSFILES = package.lisp,v iterate.lisp,v iterate-test.lisp,v iterate-pg.lisp,v doc/iter-man.tex,v
PDFFILES = doc/iter-man.pdf doc/iter-bare.pdf


# If you need to sudo in order to use docker, modify this.
DOCKER ?= docker

sourceDirectory := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))


l ?= sbcl

ifeq ($(l), abcl)
	command ?= abcl
	loadfile = --noinit --nosystem --noinform --load

else ifeq ($(l), allegro)
	command ?= alisp
	loadfile = -q -L

else ifeq ($(l), ccl)
	loadfile = --no-init --quiet --load
        command ?= ccl

else ifeq ($(l), clasp)
	command ?= 
	loadfile = --norc --noinit --load

else ifeq ($(l), clisp)
	command ?= clisp
	loadfile = -norc --silent -ansi -I -c -l

else ifeq ($(l), cmucl)
	command ?= cmucl
	loadfile = -noinit -batch --load

else ifeq ($(l), ecl)
	command ?= ecl
	loadfile = --norc --load

else ifeq ($(l), lispworks)
	command ?= lispworks # this is just a convention...
	loadfile = -siteinit = -init -

else ifeq ($(l), sbcl)
	command ?= sbcl
	loadfile = --no-userinit --no-sysinit --load
else
	$(error Don\'t know how to operate on implementation $(l))
endif

.PHONY: test

distrib:
	tar czf iterate.tgz $(FILES) $(TEXFILES) $(PDFFILES)

devel:
	tar czf iterate-rcs.tgz $(FILES) $(RCSFILES) $(TEXFILES) Makefile

test:
	$(command) $(loadfile) test.lisp


# Useful for reproducing test failures with Docker.
test-docker-repl:
	@${DOCKER} run --rm -i -t --pull always -u $(shell id -u):$(shell id -g) -v $(sourceDirectory):$(sourceDirectory) -w $(sourceDirectory)/test clfoundation/${l}:latest

test-docker-lisp:
	@${DOCKER} run --rm -i -t --pull always -u $(shell id -u):$(shell id -g) -v $(sourceDirectory):$(sourceDirectory) -w $(sourceDirectory) clfoundation/${l}:latest make test l=${l} t=${t}
