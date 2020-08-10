# Makefile for badwm - see LICENSE for license and copyright information

VERSION = 0.4.2
WMNAME  = badwm

PREFIX ?= /usr/local
BINDIR ?= ${PREFIX}/bin

X11INC = -I/usr/X11R6/include
X11LIB = -L/usr/X11R6/lib -lX11

INCS = -I. -I/usr/include ${X11INC} -lpthread
LIBS = -L/usr/lib -lc ${X11LIB} -lpthread

# CFLAGS   = -std=c99 -pedantic -Wall -Wextra ${INCS} -DVERSION=\"${VERSION}\"
CFLAGS	 = -pedantic -Wall -Wextra ${INCS} -DVERSION=\"${VERSION}\"
LDFLAGS  = ${LIBS}

CC 	 = cc
EXEC = ${WMNAME}

SRC = ${WMNAME}.c
OBJ = ${SRC:.c=.o}

all: CFLAGS += -Os
all: LDFLAGS += -s
all: options ${WMNAME}

debug: CFLAGS += -O0 -g
debug: options ${WMNAME}

options:
	@echo ${WMNAME} build options:
	@echo "CFLAGS   = ${CFLAGS}"
	@echo "LDFLAGS  = ${LDFLAGS}"
	@echo "CC       = ${CC}"

.c.o:
	@echo CC $<
	@${CC} -c ${CFLAGS} $<

${OBJ}: config.h

config.h:
	@echo creating $@ from config.def.h
	@cp config.def.h $@

${WMNAME}: ${OBJ}
	@echo CC -o $@
	@${CC} -o $@ ${OBJ} ${LDFLAGS}

dist: clean
	mkdir -p badwm-$(VERSION)
	cp LICENSE Makefile README.md config.def.h badwm.c badwm-$(VERSION)
	tar -cf badwm-$(VERSION).tar badwm-$(VERSION)
	gzip badwm-$(VERSION).tar
	rm -rf badwm-$(VERSION)

clean:
	@echo cleaning
	@rm -fv ${WMNAME} ${OBJ} ${WMNAME}-${VERSION}.tar.gz

install: all
	@echo installing executable file to ${DESTDIR}${PREFIX}/bin
	@install -Dm755 ${WMNAME} ${DESTDIR}${PREFIX}/bin/${WMNAME}

uninstall:
	@echo removing executable file from ${DESTDIR}${PREFIX}/bin
	@rm -f ${DESTDIR}${PREFIX}/bin/${WMNAME}

.PHONY: all options dist clean install uninstall
