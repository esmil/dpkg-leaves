/*
 * dpkg-leaves - show packages not required by other packages
 *
 * Copyright © 2022 Emil Renner Berthing <esmil@mailme.dk>
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#define LIBDPKG_VOLATILE_API
#include <dpkg/db-fsys.h>
#include <dpkg/dpkg.h>
#include <dpkg/dpkg-db.h>
#include <dpkg/options.h>
#include <dpkg/pager.h>
#include <dpkg/pkg-array.h>
#include <dpkg/pkg-format.h>
#include <dpkg/pkg-show.h>

#ifdef HAVE_DPKG_I18N_H
#include <dpkg/i18n.h>
#else
static inline void dpkg_locales_init(const char *package) {}
static inline void dpkg_locales_done(void) {}
#define _(str) str
#define N_(str) str
#endif

#define BITS_PER_LONG (8 * sizeof(unsigned long))
#define MULTIPLE_PROVIDERS ((struct pkginfo *)1U)

static int leavespriority;
static int leavesedges = 2;
static const char *leavesformat;

static inline int
bitmap_end(int len)
{
  return len / BITS_PER_LONG + !!(len % BITS_PER_LONG);
}

static unsigned long *
bitmap_new(int len)
{
  return m_calloc(bitmap_end(len), sizeof(unsigned long));
}

static void
bitmap_free(unsigned long *bitmap)
{
  free(bitmap);
}

static void
bitmap_setbit(unsigned long *bitmap, int i)
{
  bitmap[i / BITS_PER_LONG] |= DPKG_BIT(i % BITS_PER_LONG);
}

static void
bitmap_clearbit(unsigned long *bitmap, int i)
{
  bitmap[i / BITS_PER_LONG] &= ~DPKG_BIT(i % BITS_PER_LONG);
}

static bool
bitmap_has(const unsigned long *bitmap, int i)
{
  return bitmap[i / BITS_PER_LONG] & DPKG_BIT(i % BITS_PER_LONG);
}

static bool
bitmap_empty(const unsigned long *bitmap, int len)
{
  int end = bitmap_end(len);
  int i;

  for (i = 0; i < end; i++) {
    if (bitmap[i])
      return false;
  }
  return true;
}

static void
bitmap_clear(unsigned long *bitmap, int len)
{
  memset(bitmap, 0, bitmap_end(len) * sizeof(bitmap[0]));
}

static void
pkg_array_init(struct pkg_array *array, size_t size)
{
  array->n_pkgs = 0;
  array->pkgs = m_malloc(size * sizeof(array->pkgs[0]));
}

static void
pkg_array_push(struct pkg_array *array, struct pkginfo *pkg)
{
  array->pkgs[array->n_pkgs++] = pkg;
}

static void
pkg_array_reset(struct pkg_array *array)
{
  array->n_pkgs = 0;
}

static int
pkg_sorter_by_nonambig_name_arch_installed_first(const void *a, const void *b)
{
  const struct pkginfo *const *x = a;
  const struct pkginfo *const *y = b;
  int ret = (int)(x[0]->status == PKG_STAT_NOTINSTALLED) - (int)(y[0]->status == PKG_STAT_NOTINSTALLED);

  if (ret)
    return ret;

  return pkg_sorter_by_nonambig_name_arch(a, b);
}

static bool
dep_is_valid_graph_edge(struct dependency *dep)
{
  static const enum deptype edgetypes[] = { dep_depends, dep_predepends, dep_recommends };
  enum deptype type = dep->type;
  int i;

  for (i = 0; i < leavesedges; i++) {
    if (type == edgetypes[i])
      return true;
  }
  return false;
}

struct graph {
  int n_nodes;
  int edges[];
};

static struct graph *
graph_new(int nodes, int edges)
{
  struct graph *g = m_malloc(offsetof(struct graph, edges) + (nodes + edges) * sizeof(g->edges[0]));

  g->n_nodes = nodes;
  memset(g->edges, 0, nodes * sizeof(g->edges[0]));
  return g;
}

static void
graph_free(struct graph *graph)
{
  free(graph);
}

static int
graph_edges_from(const struct graph *graph, int u)
{
  return (u == 0) ? graph->n_nodes : graph->edges[u - 1];
}

static int
graph_edges_end(const struct graph *graph, int u)
{
  return graph->edges[u];
}

static int
graph_n_edges(const struct graph *graph)
{
  return graph->edges[graph->n_nodes - 1] - graph->n_nodes;
}

static struct graph *
graph_reverse(const struct graph *graph)
{
  struct graph *rgraph = graph_new(graph->n_nodes, graph_n_edges(graph));
  int u, i;

  for (u = 0; u < graph->n_nodes; u++) {
    for (i = graph_edges_from(graph, u); i < graph_edges_end(graph, u); i++) {
      int v = graph->edges[i];
      rgraph->edges[v] += 1;
    }
  }

  for (u = 0, i = graph_edges_from(graph, 0); u < graph->n_nodes; u++) {
    int n = rgraph->edges[u];
    rgraph->edges[u] = i;
    i += n;
  }

  for (u = 0; u < graph->n_nodes; u++) {
    for (i = graph_edges_from(graph, u); i < graph_edges_end(graph, u); i++) {
      int v = graph->edges[i];
      rgraph->edges[rgraph->edges[v]++] = u;
    }
  }

  return rgraph;
}

static void DPKG_ATTR_UNUSED
graph_dump(const struct graph *graph, const struct pkg_array *array)
{
  int u, i;

  printf("n_nodes = %d n_edges = %d\n", graph->n_nodes, graph_n_edges(graph));
  for (u = 0; u < graph->n_nodes; u++) {
    for (i = graph_edges_from(graph, u); i < graph_edges_end(graph, u); i++) {
      int v = graph->edges[i];

      printf("(%4d, %4d) %s -> %s\n", u, v,
          pkg_name(array->pkgs[u], pnaw_nonambig),
          pkg_name(array->pkgs[v], pnaw_nonambig));
    }
  }
}

static struct graph *
graph_build_depends(struct pkg_array *array)
{
  struct pkg_array deps;
  struct graph *graph;
  int max_deps = 0;
  int u, i;

  /* sort packages with installed packages first */
  pkg_array_sort(array, pkg_sorter_by_nonambig_name_arch_installed_first);

  /* estimate max number of edges pr. node and total edges */
  for (u = 0, i = 0; u < array->n_pkgs; u++) {
    struct pkginfo *pkg = array->pkgs[u];
    struct dependency *dep;
    int n_deps = 0;

    if (pkg->status == PKG_STAT_NOTINSTALLED)
      break;

    for (dep = pkg->installed.depends; dep; dep = dep->next)
      n_deps += (int)dep_is_valid_graph_edge(dep);

    if (max_deps < n_deps)
      max_deps = n_deps;
    i += n_deps;
  }

  /* the graph will only have installed packages as nodes */
  graph = graph_new(u, i);

  pkg_array_init(&deps, max_deps + 1);

  /* go through each installed package */
  for (u = 0, i = graph_edges_from(graph, 0); u < graph->n_nodes; u++) {
    struct pkginfo *pkg = array->pkgs[u];
    struct dependency *dep;
    int v, j;

    /* and for each dependency */
    for (dep = pkg->installed.depends; dep; dep = dep->next) {
      struct pkginfo *res = NULL;
      struct deppossi *possi;

      /* that is a Pre-Depends, Depends or possibly Recommends */
      if (!dep_is_valid_graph_edge(dep))
        continue;

      /* go through the different choices (foo | bar | ..) */
      for (possi = dep->list; possi; possi = possi->next) {
        struct pkginfo *dpkg;

        /* and find the package(s) satisfying the dependency */
        for (dpkg = &possi->ed->pkg; dpkg; dpkg = dpkg->arch_next) {
          struct deppossi *pp;

          /* it may be a direct dependency */
          if (dpkg->status != PKG_STAT_NOTINSTALLED &&
              versionsatisfied(&dpkg->installed, possi) &&
              archsatisfied(&dpkg->installed, possi)) {
            if (res == NULL)
              res = dpkg;
            else if (res != dpkg)
              res = MULTIPLE_PROVIDERS;
          }

          /* or be Provided by installed packages */
          for (pp = dpkg->set->depended.installed; pp; pp = pp->rev_next) {
            struct dependency *pdep = pp->up;
            struct pkginfo *ppkg = pdep->up;

            if (pdep->type != dep_provides)
              continue;
            if (!pkg_virtual_deppossi_satisfied(possi, pp))
              continue;
            if (!archsatisfied(&ppkg->installed, possi))
              continue;

            if (res == NULL)
              res = ppkg;
            else if (res != ppkg)
              res = MULTIPLE_PROVIDERS;
          }
        }
      }

      /*
      if (res == NULL) {
        struct varbuf vb;

        varbuf_init(&vb, 128);
        varbuf_printf(&vb, "WARNING: Nothing provides ");
        varbufdependency(&vb, dep);
        varbuf_printf(&vb, " needed by ");
        varbuf_add_pkgbin_name(&vb, pkg, &pkg->installed, pnaw_nonambig);
        puts(varbuf_str(&vb));
        varbuf_destroy(&vb);
      }
      */

      /* if the dependency is satisfied by exactly one installed package add it
       * to the list of critical dependencies (edges) for this package (node) */
      if (res && res != MULTIPLE_PROVIDERS && res != pkg)
        pkg_array_push(&deps, res);
    }

    /* sort the dependencies */
    pkg_array_sort(&deps, pkg_sorter_by_nonambig_name_arch_installed_first);

    /* and add the dependencies to the graph */
    deps.pkgs[deps.n_pkgs] = NULL;
    for (j = 0, v = 0; j < deps.n_pkgs; j++) {
      struct pkginfo *dpkg = deps.pkgs[j];

      /* skipping duplicates */
      if (dpkg == deps.pkgs[j + 1])
        continue;

      while (dpkg != array->pkgs[v])
        v++;

      graph->edges[i++] = v++;
    }
    graph->edges[u] = i;
    pkg_array_reset(&deps);
  }

  pkg_array_destroy(&deps);
  return graph;
}

static struct graph *
graph_build_rdepends(struct pkg_array *array)
{
  struct graph *graph = graph_build_depends(array);
  struct graph *rgraph = graph_reverse(graph);

  graph_free(graph);
  return rgraph;
}

struct leaves {
  int n_sccs;
  int cap_sccs;
  int **sccs;
};

static void
leaves_init(struct leaves *leaves)
{
  leaves->n_sccs = 0;
  leaves->cap_sccs = 64;
  leaves->sccs = m_malloc(leaves->cap_sccs * sizeof(leaves->sccs[0]));
}

static void
leaves_destroy(struct leaves *leaves)
{
  int i;

  for (i = 0; i < leaves->n_sccs; i++)
    free(leaves->sccs[i]);
  free(leaves->sccs);
}

static int
int_sorter_ascending(const void *a, const void *b)
{
  const int *x = a;
  const int *y = b;

  return *x - *y;
}

static void
leaves_add(struct leaves *leaves, const int *scc, int len)
{
  int *entry = m_malloc((len + 1) * sizeof(entry[0]));

  memcpy(entry, scc, len * sizeof(entry[0]));
  qsort(entry, len, sizeof(entry[0]), int_sorter_ascending);
  entry[len] = -1;

  if (leaves->n_sccs + 1 >= leaves->cap_sccs) {
    leaves->cap_sccs *= 2;
    leaves->sccs = m_realloc(leaves->sccs, leaves->cap_sccs * sizeof(leaves->sccs[0]));
  }
  leaves->sccs[leaves->n_sccs++] = entry;
}

static void
leaves_sort(struct leaves *leaves, int (*compare)(const void *, const void *))
{
  qsort(leaves->sccs, leaves->n_sccs, sizeof(leaves->sccs[0]), compare);
}

static int
leaves_sorter_by_first_node(const void *a, const void *b)
{
  const int *const *x = a;
  const int *const *y = b;

  return x[0][0] - y[0][0];
}

static void DPKG_ATTR_UNUSED
naive_leaves(struct leaves *leaves, const struct graph *rgraph, bool allcycles)
{
  int u;

  leaves_init(leaves);
  if (allcycles)
    return;

  for (u = 0; u < rgraph->n_nodes; u++) {
    if (graph_edges_from(rgraph, u) == graph_edges_end(rgraph, u))
      leaves_add(leaves, &u, 1);
  }
}

static void DPKG_ATTR_UNUSED
kosaraju(struct leaves *leaves, const struct graph *rgraph, bool allcycles)
{
  struct graph *graph = graph_reverse(rgraph);
  const int N = rgraph->n_nodes;
  int *rstack = m_malloc(N * sizeof(rstack[0]));
  int *stack = m_malloc(N * sizeof(stack[0]));
  unsigned long *tag = bitmap_new(N);
  unsigned long *sccredges;
  int r = N;
  int u;

  /*
   * do depth-first searches in the graph and push nodes to rstack
   * "on the way up" until all nodes have been pushed.
   * tag nodes as they're processed so we don't visit them more than once
   */
  for (u = 0; u < N; u++) {
    int top, i;

    if (bitmap_has(tag, u))
      continue;

    top = 0;
    i = graph_edges_from(graph, u);
    bitmap_setbit(tag, u);
    while (true) {
      if (i < graph_edges_end(graph, u)) {
        int v = graph->edges[i++];

        if (!bitmap_has(tag, v)) {
          stack[top] = u;
          rstack[top++] = i;
          u = v;
          i = graph_edges_from(graph, u);
          bitmap_setbit(tag, u);
        }
      } else {
        rstack[--r] = u;
        if (!top)
          break;
        u = stack[--top];
        i = rstack[top];
      }
    }
  }
  graph_free(graph);

  /*
   * now searches beginning at nodes popped from rstack in the graph with all
   * edges reversed will give us the strongly connected components.
   * this time all nodes are tagged, so let's remove the tags as we visit each
   * node.
   * the incoming edges to each component is the union of incoming edges to
   * each node in the component minus the incoming edges from component nodes
   * themselves.
   * if there are no such incoming edges the component is a leaf and we
   * add it to the array of leaves.
   */
  sccredges = bitmap_new(N);
  leaves_init(leaves);
  for (; r < N; r++) {
    int k, n, i;

    u = rstack[r];
    if (!bitmap_has(tag, u))
      continue;

    n = 0;
    stack[n++] = u;
    bitmap_clearbit(tag, u);
    for (k = 0; k < n; k++) {
      u = stack[k];
      for (i = graph_edges_from(rgraph, u); i < graph_edges_end(rgraph, u); i++) {
        int v = rgraph->edges[i];

        bitmap_setbit(sccredges, v);
        if (bitmap_has(tag, v)) {
          stack[n++] = v;
          bitmap_clearbit(tag, v);
        }
      }
    }

    if (!allcycles) {
      for (k = 0; k < n; k++)
        bitmap_clearbit(sccredges, stack[k]);

      if (bitmap_empty(sccredges, N))
        leaves_add(leaves, stack, n);
      else
        bitmap_clear(sccredges, N);
    } else {
      if (n > 1)
        leaves_add(leaves, stack, n);
    }
  }

  bitmap_free(sccredges);
  bitmap_free(tag);
  free(stack);
  free(rstack);
}

static void
tarjan(struct leaves *leaves, const struct graph *rgraph, bool allcycles)
{
  const int N = rgraph->n_nodes;
  int *stack = m_malloc(N * sizeof(stack[0]));
  int *estack = m_malloc(N * sizeof(estack[0]));
  unsigned int *rindex = m_calloc(N, sizeof(rindex[0]));
  unsigned long *sccredges = bitmap_new(N);
  int r = N;
  int u;

  leaves_init(leaves);

  for (u = 0; u < N; u++) {
    unsigned int idx, uidx;
    int top, i;

    if (rindex[u])
      continue;

    top = 0;
    i = graph_edges_from(rgraph, u);
    idx = 2;
    rindex[u] = idx;
    while (true) {
      if (i < graph_edges_end(rgraph, u)) {
        int v = rgraph->edges[i++];
        if (!rindex[v]) {
          stack[top] = u;
          estack[top++] = i;
          u = v;
          i = graph_edges_from(rgraph, u);
          idx += 2;
          rindex[u] = idx;
        } else if (rindex[v] < rindex[u])
          rindex[u] = rindex[v] | 1U;
        continue;
      }

      stack[--r] = u;
      uidx = rindex[u];
      if (!(uidx & 1U)) {
        int scc = r;

        do {
          int v = stack[r++];
          int j;

          rindex[v] = ~0U;
          for (j = graph_edges_from(rgraph, v); j < graph_edges_end(rgraph, v); j++)
            bitmap_setbit(sccredges, rgraph->edges[j]);
        } while (r < N && uidx <= rindex[stack[r]]);

        if (!allcycles) {
          for (i = scc; i < r; i++)
            bitmap_clearbit(sccredges, stack[i]);

          if (bitmap_empty(sccredges, N))
            leaves_add(leaves, &stack[scc], r - scc);
          else
            bitmap_clear(sccredges, N);
        } else {
          if (r - scc > 1)
            leaves_add(leaves, &stack[scc], r - scc);
        }
      }

      if (!top)
        break;
      uidx = rindex[u];
      u = stack[--top];
      i = estack[top];
      if (uidx < rindex[u])
        rindex[u] = uidx | 1U;
    }
  }

  bitmap_free(sccredges);
  free(rindex);
  free(estack);
  free(stack);
}

static struct pkg_format_node *
get_format(struct leaves *leaves, struct pkg_array *array)
{
  char fstr[64];
  struct dpkg_error err;
  struct pkg_format_node *fmt;
  int maxwidth = 0;
  int i, j;

  if (leavesformat) {
    fmt = pkg_format_parse(leavesformat, &err);
    goto out;
  }

  for (i = 0; i < leaves->n_sccs; i++) {
    const int *scc = leaves->sccs[i];

    for (j = *scc++; j >= 0; j = *scc++) {
      struct pkginfo *pkg = array->pkgs[j];
      int len;

      if (pkg->priority < leavespriority && pkg->priority >= 0)
        continue;

      len = strlen(pkg_name(pkg, pnaw_nonambig));
      if (len > maxwidth)
        maxwidth = len;
    }
  }

  sprintf(fstr, "${binary:Package;-%d}  ${Priority;-9}  ${Version}", maxwidth);
  fmt = pkg_format_parse(fstr, &err);

out:
  if (!fmt) {
    notice(_("error in format: %s"), err.str);
    dpkg_error_destroy(&err);
  }
  return fmt;
}

#ifdef MISSING_PKG_FORMAT_NEEDS_DB_FSYS
static inline bool
pkg_format_needs_db_fsys(struct pkg_format_node *fmt)
{
  return false;
}
#endif

#ifdef MISSING_PKG_FORMAT_PRINT
#define varbuf_init(vb, size) ((void)(vb))
#define varbuf_destroy(vb)    ((void)(vb))
#define showpkg(vb, fmt, pkg, mark) showpkg_(fmt, pkg, mark)
static void
showpkg_(struct pkg_format_node *fmt, struct pkginfo *pkg, char mark)
{
  putchar(mark);
  putchar(' ');
  pkg_format_show(fmt, pkg, &pkg->installed);
  putchar('\n');
}
#else

#ifdef MISSING_VARBUF_STR
static inline const char *
varbuf_str(struct varbuf *vb)
{
  return varbuf_get_str(vb);
}
#endif

static void
showpkg(struct varbuf *vb, struct pkg_format_node *fmt,
        struct pkginfo *pkg, char mark)
{
  varbuf_add_char(vb, mark);
  varbuf_add_char(vb, ' ');
  pkg_format_print(vb, fmt, pkg, &pkg->installed);
  puts(varbuf_str(vb));
  varbuf_reset(vb);
}
#endif

static int
showsccs(bool allcycles)
{
  struct pkg_array array;
  struct graph *rgraph;
  struct leaves leaves;
  struct varbuf vb;
  struct pkg_format_node *fmt;
  struct pager *pager;
  bool format_needs_db_fsys;
  int i, j;
  int ret = 0;

  modstatdb_open(msdbrw_readonly);
  pkg_array_init_from_hash(&array);
  rgraph = graph_build_rdepends(&array);
  tarjan(&leaves, rgraph, allcycles);
  leaves_sort(&leaves, leaves_sorter_by_first_node);

  fmt = get_format(&leaves, &array);
  if (!fmt) {
    ret = 1;
    goto out;
  }
  format_needs_db_fsys = pkg_format_needs_db_fsys(fmt);

  varbuf_init(&vb, 64);
  pager = pager_spawn(_("showing leaves list on pager"));

  for (i = 0; i < leaves.n_sccs; i++) {
    const int *scc = leaves.sccs[i];
    char mark = '-';

    for (j = *scc++; j >= 0; j = *scc++) {
      struct pkginfo *pkg = array.pkgs[j];

      if (pkg->priority < leavespriority && pkg->priority >= 0)
        continue;

      if (format_needs_db_fsys)
        ensure_packagefiles_available(pkg);

      showpkg(&vb, fmt, pkg, mark);
      mark = ' ';
    }
  }

  m_output(stdout, _("<standard output>"));
  m_output(stderr, _("<standard error>"));

  pager_reap(pager);
  varbuf_destroy(&vb);
  pkg_format_free(fmt);

out:
  leaves_destroy(&leaves);
  graph_free(rgraph);
  pkg_array_destroy(&array);
  modstatdb_shutdown();
  return ret;
}

static int
showcycles(const char *const *argv)
{
  if (argv[0])
    badusage(_("unknown command '%s'"), argv[0]);

  return showsccs(true);
}

static int
showleaves(const char *const *argv)
{
  if (argv[0])
    badusage(_("unknown command '%s'"), argv[0]);

  return showsccs(false);
}

#ifdef MISSING_SET_ROOT
static void
set_admindir(const struct cmdinfo *cip, const char *value)
{
  dpkg_db_set_dir(value);
}

static void
set_root(const struct cmdinfo *cip, const char *value)
{
  char *db_dir;

  /* Initialize the root directory. */
  dpkg_fsys_set_dir(value);

  /* Set the database directory based on the new root directory. */
  db_dir = dpkg_fsys_get_path(ADMINDIR);
  dpkg_db_set_dir(db_dir);
}
#endif

static void
set_no_pager(const struct cmdinfo *ci, const char *value)
{
  pager_enable(false);
}

static void
set_priority(const struct cmdinfo *ci, const char *value)
{
  long v = dpkg_options_parse_arg_int(ci, value);

  if (v < 0 || v >= PKG_PRIO_UNKNOWN)
    badusage(_("invalid prority %ld"), v);

  leavespriority = v;
}

static void
set_recommends(const struct cmdinfo *ci, const char *value)
{
  leavesedges = 3;
}

static void DPKG_ATTR_NORET
printversion(const struct cmdinfo *ci, const char *value)
{
  printf(_("%s version %s - show packages not required by any other package.\n"),
         DPKGLEAVES, PACKAGE_RELEASE);
  printf(_(
"This is free software; see the GNU General Public License version 2 or\n"
"later for copying conditions. There is NO warranty.\n"));

  m_output(stdout, _("<standard output>"));

  exit(0);
}

static void DPKG_ATTR_NORET
usage(const struct cmdinfo *ci, const char *value)
{
  printf(_(
"Usage: %s [<option>...] [<command>]\n"
"\n"), DPKGLEAVES);

  printf(_(
"Commands:\n"
"      --cycles                     Show all dependency cycles found.\n"
"  -?, --help                       Show this help message.\n"
"      --version                    Show the version.\n"
"\n"));

  printf(_(
"Options:\n"
"  --admindir=<directory>           Use <directory> instead of %s.\n"
"  --root=<directory>               Use <directory> instead of %s.\n"
"  --no-pager                       Disables the use of any pager.\n"
"  -p|--priority=<priority>         Show only packages with this priority or higher.\n"
"  -r|--recommends                  Treat recommends as dependencies.\n"
"  -f|--format=<format>             Use alternative format.\n"
"\n"), dpkg_db_get_dir(), "/");

  printf(_(
"Format syntax:\n"
"  A format is a string that will be output for each package. The format\n"
"  can include the standard escape sequences \\n (newline), \\r (carriage\n"
"  return) or \\\\ (plain backslash). Package information can be included\n"
"  by inserting variable references to package fields using the ${var[;width]}\n"
"  syntax. Fields will be right-aligned unless the width is negative in which\n"
"  case left alignment will be used.\n"));

  m_output(stdout, _("<standard output>"));

  exit(0);
}

static const char printforhelp[] = N_("Use --help for help.");

static const struct cmdinfo cmdinfos[]= {
  ACTION("cycles", 0, 2, showcycles),
  { "admindir",      0,  1,  NULL,  NULL,           set_admindir,    0,  NULL,  NULL },
  { "root",          0,  1,  NULL,  NULL,           set_root,        0,  NULL,  NULL },
  { "no-pager",      0,  0,  NULL,  NULL,           set_no_pager,    0,  NULL,  NULL },
  { "priority",    'p',  1,  NULL,  NULL,           set_priority,    0,  NULL,  NULL },
  { "recommends",  'r',  0,  NULL,  NULL,           set_recommends,  0,  NULL,  NULL },
  { "format",      'f',  1,  NULL,  &leavesformat,  NULL,            0,  NULL,  NULL },
  { "help",        '?',  0,  NULL,  NULL,           usage,           0,  NULL,  NULL },
  { "version",       0,  0,  NULL,  NULL,           printversion,    0,  NULL,  NULL },
  {  /* sentinel */ }
};

int main(int argc, const char *const *argv)
{
  int ret;

  dpkg_set_report_piped_mode(_IOFBF);
  dpkg_locales_init(PACKAGE);
  dpkg_program_init("dpkg-leaves");
  dpkg_options_parse(&argv, cmdinfos, printforhelp);

  if (cipaction)
    ret = cipaction->action(argv);
  else
    ret = showleaves(argv);

  dpkg_program_done();
  dpkg_locales_done();

  return !!ret;
}

/* vim: set ts=2 sw=2 et: */
