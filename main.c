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

static int
pkg_array_index_of(struct pkg_array *array, struct pkginfo *pkg)
{
  int i;

  for (i = 0; i < array->n_pkgs; i++) {
    if (array->pkgs[i] == pkg)
      return i;
  }
  return -1;
}

struct dep_array {
  int n_deps;
  struct dependency **deps;
};

static void
dep_array_destroy(struct dep_array *array)
{
  free(array->deps);
}

static int
dep_array_index_of(struct dep_array *array, struct dependency *dep)
{
  int i = 0;
  int j = array->n_deps;

  while (i != j) {
    int m = (i + j) / 2;
    struct dependency *mid = array->deps[m];

    if (dep == mid)
      return m;
    if (dep < mid)
      j = m;
    else
      i = m + 1;
  }
  return -1;
}

static int
dep_sorter_by_pointer(const void *a, const void *b)
{
  const struct dependency *x = *(const struct dependency *const *)a;
  const struct dependency *y = *(const struct dependency *const *)b;

  return (x < y) ? -1 : (x > y);
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

static struct pkginfo *
pkg_installed_provider(struct pkginfo *pkg)
{
  struct pkginfo *ret = NULL;
  struct deppossi *possi;

  if (pkg->status != PKG_STAT_NOTINSTALLED)
    ret = pkg;

  for (possi = pkg->set->depended.installed; possi; possi = possi->rev_next) {
    struct dependency *dep = possi->up;
    struct pkginfo *ppkg;

    if (dep->type != dep_provides)
      continue;

    ppkg = dep->up;
    if (ppkg->status == PKG_STAT_NOTINSTALLED)
      continue;

    if (ret)
      return MULTIPLE_PROVIDERS;

    ret = ppkg;
  }

  return ret;
}

struct edge {
  int u;
  int v;
};

static int
edge_sorter_by_nodes_invalid_last(const void *a, const void *b)
{
  const struct edge *x = a;
  const struct edge *y = b;
  int ret;

  ret = (int)(x->u < 0 || x->v < 0) - (int)(y->u < 0 || y->v < 0);
  if (ret)
    return ret;

  ret = x->u - y->u;
  if (ret)
    return ret;

  return x->v - y->v;
}

struct graph {
  int n_nodes;
  int n_edges;
  int *edges;
};

static void
graph_destroy(struct graph *graph)
{
  free(graph->edges);
}

static const int *
graph_edges_from(const struct graph *graph, int u)
{
  return &graph->edges[graph->edges[u]];
}

static void
graph_build_rdepends(struct graph *graph, struct pkg_array *array)
{
  struct dep_array deparray;
  struct edge *edges;
  int n_installed;
  int i, j, k;

  pkg_array_sort(array, pkg_sorter_by_nonambig_name_arch_installed_first);

  deparray.n_deps = 0;
  for (i = 0; i < array->n_pkgs; i++) {
    struct pkginfo *pkg = array->pkgs[i];
    struct dependency *dep;

    if (pkg->status == PKG_STAT_NOTINSTALLED)
      break;

    for (dep = pkg->installed.depends; dep; dep = dep->next) {
      if (dep_is_valid_graph_edge(dep))
        deparray.n_deps += 1;
    }
  }
  n_installed = i;

  deparray.deps = m_calloc(deparray.n_deps, sizeof(deparray.deps[0]));
  for (i = 0, j = 0; i < n_installed; i++) {
    struct dependency *dep;

    for (dep = array->pkgs[i]->installed.depends; dep; dep = dep->next) {
      if (dep_is_valid_graph_edge(dep))
        deparray.deps[j++] = dep;
    }
  }
  qsort(deparray.deps, deparray.n_deps, sizeof(deparray.deps[0]), dep_sorter_by_pointer);

  edges = m_malloc((deparray.n_deps + 1) * sizeof(edges[0]));
  for (i = 0; i < deparray.n_deps; i++) {
    edges[i].u = -1;
    edges[i].v = pkg_array_index_of(array, deparray.deps[i]->up);
  }
  edges[i].u = -1;
  edges[i].v = -1;

  for (i = 0; i < array->n_pkgs; i++) {
    struct pkginfo *pkg = array->pkgs[i];
    struct pkginfo *ppkg = pkg_installed_provider(pkg);
    struct deppossi *possi;
    int u;

    if (!ppkg)
      continue;

    u = (ppkg == MULTIPLE_PROVIDERS) ? -2 : pkg_array_index_of(array, ppkg);

    for (possi = pkg->set->depended.installed; possi; possi = possi->rev_next) {
      struct dependency *dep = possi->up;

      if (!dep_is_valid_graph_edge(dep))
        continue;

      j = dep_array_index_of(&deparray, dep);
      if (j < 0)
        continue;

      if (edges[j].u == -1)
        edges[j].u = u;
      else if (edges[j].u != u)
        edges[j].u = -2;
    }
  }

  qsort(edges, deparray.n_deps + 1, sizeof(edges[0]), edge_sorter_by_nodes_invalid_last);

  graph->n_nodes = n_installed;
  graph->n_edges = 0;
  for (i = 0; edges[i].u >= 0; i++) {
    if (edges[i].v != edges[i + 1].v || edges[i].u != edges[i + 1].u)
      graph->n_edges += 1;
  }
  graph->edges = m_malloc((2 * graph->n_nodes + graph->n_edges) * sizeof(graph->edges[0]));

  for (i = 0, j = 0, k = graph->n_nodes; i < graph->n_nodes; i++) {
    graph->edges[i] = k;
    for (; edges[j].u == i; j++) {
      if (edges[j].v != edges[j + 1].v || edges[j].u != edges[j + 1].u)
        graph->edges[k++] = edges[j].v;
    }
    graph->edges[k++] = -1;
  }

  /*
  for (i = 0; i < graph->n_nodes; i++) {
    const int *iedges = graph_edges_from(graph, i);

    for (j = *iedges++; j >= 0; j = *iedges++)
      printf("(%3d, %3d) %s -> %s\n", i, j,
          pkg_name(array->pkgs[i], pnaw_nonambig),
          pkg_name(array->pkgs[j], pnaw_nonambig));
  }
  */

  free(edges);
  dep_array_destroy(&deparray);
}

static void
graph_reverse(struct graph *rgraph, const struct graph *graph)
{
  const int *edges;
  int i, j;

  rgraph->n_nodes = graph->n_nodes;
  rgraph->n_edges = graph->n_edges;
  rgraph->edges = m_calloc(2 * rgraph->n_nodes + rgraph->n_edges, sizeof(rgraph->edges[0]));
  if (rgraph->n_nodes == 0)
    return;

  for (i = 0; i < graph->n_nodes; i++) {
    edges = graph_edges_from(graph, i);
    for (j = *edges++; j >= 0; j = *edges++)
      rgraph->edges[j] += 1;
  }
  for (i = 0, j = rgraph->n_nodes; i < rgraph->n_nodes; i++) {
    int len = rgraph->edges[i] + 1;
    rgraph->edges[i] = j;
    j += len;
  }

  for (i = 0; i < graph->n_nodes; i++) {
    edges = graph_edges_from(graph, i);
    for (j = *edges++; j >= 0; j = *edges++)
      rgraph->edges[rgraph->edges[j]++] = i;
  }
  for (i = 0; i < rgraph->n_nodes; i++)
    rgraph->edges[rgraph->edges[i]++] = -1;

  memmove(&rgraph->edges[1], &rgraph->edges[0], (rgraph->n_nodes - 1) * sizeof(rgraph->edges[0]));
  rgraph->edges[0] = rgraph->n_nodes;
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
    if (graph_edges_from(rgraph, u)[0] < 0)
      leaves_add(leaves, &u, 1);
  }
}

static void DPKG_ATTR_UNUSED
kosaraju(struct leaves *leaves, const struct graph *rgraph, bool allcycles)
{
  struct graph graph;
  const int N = rgraph->n_nodes;
  int *rstack = m_malloc(N * sizeof(rstack[0]));
  int *stack = m_malloc(N * sizeof(stack[0]));
  unsigned long *tag = bitmap_new(N);
  unsigned long *sccredges;
  int top = 0;
  int r = N;
  int i, j, u, v;

  graph_reverse(&graph, rgraph);

  /*
   * do depth-first searches in the graph and push nodes to rstack
   * "on the way up" until all nodes have been pushed.
   * tag nodes as they're processed so we don't visit them more than once
   */
  for (i = 0; i < N; i++) {
    if (bitmap_has(tag, i))
      continue;

    u = i;
    j = 0;
    bitmap_setbit(tag, u);
    while (true) {
      v = graph_edges_from(&graph, u)[j++];
      if (v >= 0) {
        if (!bitmap_has(tag, v)) {
          rstack[top] = j;
          stack[top++] = u;
          u = v;
          j = 0;
          bitmap_setbit(tag, u);
        }
      } else {
        rstack[--r] = u;
        if (!top)
          break;
        u = stack[--top];
        j = rstack[top];
      }
    }
  }
  graph_destroy(&graph);

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
    u = rstack[r];
    if (!bitmap_has(tag, u))
      continue;

    stack[top++] = u;
    bitmap_clearbit(tag, u);
    j = N;
    while (top) {
      const int *redges;

      u = stack[--j] = stack[--top];
      redges = graph_edges_from(rgraph, u);
      for (v = *redges++; v >= 0; v = *redges++) {
        bitmap_setbit(sccredges, v);
        if (bitmap_has(tag, v)) {
          stack[top++] = v;
          bitmap_clearbit(tag, v);
        }
      }
    }

    if (!allcycles) {
      for (i = j; i < N; i++)
        bitmap_clearbit(sccredges, stack[i]);

      if (bitmap_empty(sccredges, N))
        leaves_add(leaves, &stack[j], N - j);
      else
        bitmap_clear(sccredges, N);
    } else {
      if (N - j > 1)
        leaves_add(leaves, &stack[j], N - j);
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
  unsigned int idx, uidx;
  int r = N;
  int top = 0;
  int i, j, u, v;

  leaves_init(leaves);

  for (i = 0; i < N; i++) {
    if (rindex[i])
      continue;

    u = i;
    j = 0;
    idx = 2;
    rindex[u] = idx;
    while (true) {
      v = graph_edges_from(rgraph, u)[j++];
      if (v >= 0) {
        if (!rindex[v]) {
          estack[top] = j;
          stack[top++] = u;
          u = v;
          j = 0;
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
          const int *edges;
          int w;

          v = stack[r++];
          rindex[v] = ~0U;
          edges = graph_edges_from(rgraph, v);
          for (w = *edges++; w >= 0; w = *edges++)
            bitmap_setbit(sccredges, w);
        } while (r < N && uidx <= rindex[stack[r]]);

        if (!allcycles) {
          for (j = scc; j < r; j++)
            bitmap_clearbit(sccredges, stack[j]);

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
      v = u;
      u = stack[--top];
      j = estack[top];
      if (rindex[v] < rindex[u])
        rindex[u] = rindex[v] | 1U;
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

#ifndef HAVE_PKG_FORMAT_NEEDS_DB_FSYS
static bool
pkg_format_needs_db_fsys(struct pkg_format_node *fmt)
{
  return false;
}
#endif

#ifdef HAVE_PKG_FORMAT_PRINT
static void
showpkg(struct varbuf *vb, struct pkg_format_node *fmt,
        struct pkginfo *pkg, char mark)
{
  varbuf_add_char(vb, mark);
  varbuf_add_char(vb, ' ');
  pkg_format_print(vb, fmt, pkg, &pkg->installed);
  puts(varbuf_get_str(vb));
  varbuf_reset(vb);
}
#else
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
#endif

static int
showsccs(bool allcycles)
{
  struct pkg_array array;
  struct graph rgraph;
  struct leaves leaves;
  struct varbuf vb;
  struct pkg_format_node *fmt;
  struct pager *pager;
  bool format_needs_db_fsys;
  int i, j;
  int ret = 0;

  modstatdb_open(msdbrw_readonly);
  pkg_array_init_from_hash(&array);
  graph_build_rdepends(&rgraph, &array);
  tarjan(&leaves, &rgraph, allcycles);
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
  graph_destroy(&rgraph);
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

#ifndef HAVE_SET_ROOT
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
