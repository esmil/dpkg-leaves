# dpkg-leaves - show packages not required by any other
#
# Copyright (c) 2022 - 2023 Emil Renner Berthing
#
# This is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

MAKEFLAGS += rR

PACKAGE          = dpkg-leaves
PACKAGE_RELEASE  = 0.1
DPKGLEAVES       = dpkg-leaves
ADMINDIR         = /var/lib/dpkg

O           = .
S          := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))

CC          = $(CROSS_COMPILE)gcc
STRIP       = $(CROSS_COMPILE)strip
PKGCONFIG   = $(CROSS_COMPILE)pkg-config
SED         = sed
GZIP        = gzip -9c
INSTALL     = install
MKDIR_P     = mkdir -p
RM_F        = rm -f
RMDIR       = rmdir
echo        = @echo '$1'
runonce     = $(eval $1 := $$(shell $2))$($1)

prefix      = /usr/local
exec_prefix = $(prefix)
bindir      = $(exec_prefix)/bin
datarootdir = $(prefix)/share
mandir      = $(datarootdir)/man
man1dir     = $(mandir)/man1

LIBDPKG        = libdpkg
LIBDPKG_CFLAGS = $(call runonce,LIBDPKG_CFLAGS,$(PKGCONFIG) --cflags $(LIBDPKG))
LIBDPKG_LIBS   = $(call runonce,LIBDPKG_LIBS,$(PKGCONFIG) --libs $(LIBDPKG))

LIBMD          = libmd
LIBMD_CFLAGS   = $(call runonce,LIBMD_CFLAGS,$(PKGCONFIG) --cflags $(LIBMD))
LIBMD_LIBS     = $(call runonce,LIBMD_LIBS,$(PKGCONFIG) --libs $(LIBMD))

ARCHFLAGS   =
OPT         = -O2
DEPENDS     = -MMD -MP
WARNINGS    = -Wall -Wextra -Wshadow -Wpointer-arith -Wformat=2 -Wformat-truncation=2 -Wundef -Wno-unused-parameter
CFLAGS     ?= $(ARCHFLAGS) $(OPT) -ggdb -pipe $(WARNINGS)
CFLAGS     += $(DEPENDS)
CPPFLAGS   += $(foreach V,PACKAGE PACKAGE_RELEASE DPKGLEAVES ADMINDIR,-D'$V="$($V)"')
LDFLAGS    ?= $(ARCHFLAGS) $(OPT)
SFLAGS      = --strip-all --strip-unneeded

CFLAGS     += $(LIBDPKG_CFLAGS)
LIBS       += $(LIBDPKG_LIBS)

V_1_20_0    = $(call runonce,V_1_20_0,$(PKGCONFIG) --exists '$(LIBDPKG) >= 1.20.0' && echo yes)
CPPFLAGS   += $(if $(V_1_20_0),,-DMISSING_PKG_FORMAT_NEEDS_DB_FSYS)

V_1_21_2    = $(call runonce,V_1_21_2,$(PKGCONFIG) --exists '$(LIBDPKG) >= 1.21.2' && echo yes)
CPPFLAGS   += $(if $(V_1_21_2),,-DMISSING_PKG_FORMAT_PRINT)

V_1_21_10   = $(call runonce,V_1_21_10,$(PKGCONFIG) --exists '$(LIBDPKG) >= 1.21.10' && echo yes)
CPPFLAGS   += $(if $(V_1_21_10),,-DMISSING_SET_ROOT)
CFLAGS     += $(if $(V_1_21_10),$(LIBMD_CFLAGS))
LIBS       += $(if $(V_1_21_10),$(LIBMD_LIBS))

V_1_22_7    = $(call runonce,V_1_22_7,$(PKGCONFIG) --exists '$(LIBDPKG) >= 1.22.7' && echo yes)
CPPFLAGS   += $(if $(V_1_22_7),,-DMISSING_VARBUF_STR)

objects = $(patsubst $S/%.c,$O/%.o,$(wildcard $S/*.c))
clean   = $O/*.d $O/*.o $O/$(DPKGLEAVES)

# use make V=1 to see raw commands or make -s for silence
ifeq ($V$(findstring s,$(word 1,$(MAKEFLAGS))),)
Q := @
else
echo =
endif

#.SECONDEXPANSION:
.PHONY: all static strip install clean
.PRECIOUS: $O/%.o

all: $O/$(DPKGLEAVES)

static: LDFLAGS += -static
static: $O/$(DPKGLEAVES)

strip: $O/$(DPKGLEAVES)
	$(call echo,  STRIP $<)
	$Q$(STRIP) $(SFLAGS) $<

install: $(DESTDIR)$(bindir)/$(DPKGLEAVES) $(DESTDIR)$(man1dir)/$(DPKGLEAVES).1.gz

$O/$(DPKGLEAVES): $(objects)
	$(call echo,  CCLD  $@)
	$Q$(CC) -o $@ $(LDFLAGS) $^ $(LIBS)

$O/%.o: $S/%.c $(MAKEFILE_LIST) | $O/
	$(call echo,  CC    $<)
	$Q$(CC) -o $@ $(CFLAGS) $(CPPFLAGS) -c $<

$O/:
	$(call echo,  MKDIR $@)
	$Q$(MKDIR_P) $@

$(DESTDIR)$(bindir)/$(DPKGLEAVES): $O/$(DPKGLEAVES) | $(DESTDIR)$(bindir)/
	$(call echo,  INSTALL $@)
	$Q$(INSTALL) -m755 $< $@

$(DESTDIR)$(man1dir)/$(DPKGLEAVES).1.gz: $S/dpkg-leaves.1.in | $(DESTDIR)$(man1dir)/
	$(call echo,  INSTALL $@)
	$Q$(SED) \
	  -e 's|@ADMINDIR@|$(ADMINDIR)|g' \
	  -e 's|@DPKGLEAVES@|$(subst -,\\-,$(DPKGLEAVES))|g' \
	  $< | $(GZIP) | $(INSTALL) -m644 /dev/stdin $@

$(DESTDIR)%/:
	$(call echo,  INSTALL $@)
	$Q$(INSTALL) -dm755 $@

clean:
	$(call echo,  RM    $(clean:./%=%))
	$Q$(RM_F) $(clean)
	$(if $(O:.=),$Q$(RMDIR) $O/ >/dev/null 2>&1 || true)

-include $O/*.d
