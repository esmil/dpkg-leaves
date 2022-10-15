# dpkg-leaves - show packages not required by any other
#
# Copyright (c) 2022, Emil Renner Berthing
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

O         = .
S        := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
TARGET    = $O/$(DPKGLEAVES)

ARCHFLAGS =
OPT       = -O2
DEPENDS   = -MMD -MP
WARNINGS  = -Wall -Wextra -Wshadow -Wpointer-arith -Wformat=2 -Wformat-truncation=2 -Wundef -Wno-unused-parameter
CPPFLAGS  = $(foreach V,PACKAGE PACKAGE_RELEASE DPKGLEAVES ADMINDIR,-D'$V="$($V)"')
CFLAGS    = $(ARCHFLAGS) $(OPT) -ggdb -pipe $(DEPENDS) $(WARNINGS) $(CPPFLAGS)
LDFLAGS   = $(ARCHFLAGS) $(OPT)
SFLAGS    = --strip-all --strip-unneeded

CC        = $(CROSS_COMPILE)gcc
STRIP     = $(CROSS_COMPILE)strip
PKGCONFIG = $(CROSS_COMPILE)pkg-config
MKDIR_P   = mkdir -p
RM_F      = rm -f
RMDIR     = rmdir
echo      = @echo '$1'

LIBDPKG   = libdpkg
CFLAGS   += $(shell $(PKGCONFIG) --cflags $(LIBDPKG))
LIBS     += $(shell $(PKGCONFIG) --libs $(LIBDPKG))

HAVE := $(shell $(PKGCONFIG) --exists '$(LIBDPKG) >= 1.20.0' && echo '-DHAVE_PKG_FORMAT_NEEDS_DB_FSYS')
HAVE += $(shell $(PKGCONFIG) --exists '$(LIBDPKG) >= 1.21.2' && echo '-DHAVE_PKG_FORMAT_PRINT')
CPPFLAGS += $(HAVE)

objects = $(patsubst $S/%.c,$O/%.o,$(wildcard $S/*.c))
clean   = $O/*.d $O/*.o $(TARGET)

# use make V=1 to see raw commands or make -s for silence
ifeq ($V$(findstring s,$(word 1,$(MAKEFLAGS))),)
Q := @
else
echo =
endif

#.SECONDEXPANSION:
.PHONY: all static strip clean
.PRECIOUS: $O/%.o

all: $(TARGET)

static: LDFLAGS += -static
static: $(TARGET)

strip: $(TARGET)
	$(call echo,  STRIP $<)
	$Q$(STRIP) $(SFLAGS) $<

$(TARGET): $(objects)
	$(call echo,  CCLD  $@)
	$Q$(CC) -o $@ $(LDFLAGS) $^ $(LIBS)

$O/%.o: $S/%.c $(MAKEFILE_LIST) | $O/
	$(call echo,  CC    $<)
	$Q$(CC) -o $@ $(CFLAGS) -c $<

$O/:
	$(call echo,  MKDIR $@)
	$Q$(MKDIR_P) $@

clean:
	$(call echo,  RM    $(clean:./%=%))
	$Q$(RM_F) $(clean)
	$(if $(O:.=),$Q$(RMDIR) $O/ >/dev/null 2>&1 || true)

-include $O/*.d
