# dpkg-leaves

## Synopsis
Show a succinct list of installed packages such that removing anything from the
system (without breaking dependencies) must include at least one of them.

## Why?

The list gives you a nice overview of what is installed on your system without
flooding you with anything required by packages already shown. The following
list of arguments basically says the same thing in different ways:

* If you want to uninstall anything from your system (without breaking
  dependencies) it must include at least one package on this list.
* If there is anything installed on the system which is not needed it must be
  on this list -- otherwise it would be required as a dependency by another
  package.
* All the packages on this list is either needed by you, other users of the
  system or not needed at all -- if it was required by another installed
  package it would not be on the list.

## How?

Naively we'd just show all packages without any reverse dependencies.
Unfortunately there are at least 2 cases where that list doesn't have the
property that removing anything from the system must include at least one
package from the list:

- **Case 1:** The *debhelper* package depends on *dh-autoreconf* which depends
  on *debhelper* in a cycle. This means once *debhelper* (or *dh-autoreconf*)
  is installed none of these packages would show up on the list even if nothing
  else depends on them and they can be safely uninstalled.
- **Case 2:** The *base-files* package pre-depends on *awk* which is provided
  by both *mawk* and *gawk*. So if both *mawk* and *gawk* is installed none of
  them would show up on the list even though one of them could be safely
  uninstalled.

This program solves the first case by constructing the directed graph of
installed packages and their dependencies and then grouping the nodes into
[*strongly connected components*][scc] and showing all packages in components
without any outside reverse dependencies. These SCCs are generalized cycles and
most of them only contain a single package, but in the example above
*debhelper* and *dh-autoreconf* (and *dh-strip-nondeterminism*) would be grouped
and all of them shown if no other package depends on any of them.

The second case is solved by only adding edges to the graph for dependencies
that are satisfied by exactly one installed package.

[scc]: https://en.wikipedia.org/wiki/Strongly_connected_component

## Building
```sh
sudo apt-get install libdpkg-dev make gcc
git clone https://github.com/esmil/dpkg-leaves.git
cd dpkg-leaves
make
```

## License

This program uses libdpkg heavily and is licensed under [GPL v2][gpl2] just like
the [dpkg][] itself.

[gpl2]: https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html
[dpkg]: https://wiki.debian.org/Teams/Dpkg
