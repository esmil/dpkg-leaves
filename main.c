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

static const char *admindir;
static const char *instdir;
static int leavespriority;
static const char *leavesformat = "${binary:Package;-35} ${Priority;-10} ${Version}\n";

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

static int
bitmap_count(const unsigned long *bitmap, int len)
{
  int end = bitmap_end(len);
  int ret = 0;
  int i;

  for (i = 0; i < end; i++)
    ret += __builtin_popcountl(bitmap[i]);
  return ret;
}

static void
bitmap_subtract(unsigned long *bitmap, const unsigned long *other, int len)
{
  int end = bitmap_end(len);
  int i;

  for (i = 0; i < end; i++)
    bitmap[i] &= ~other[i];
}

static int *
bitmap_to_array(const unsigned long *bitmap, int len)
{
  int *ret = m_malloc((bitmap_count(bitmap, len) + 1) * sizeof(ret[0]));
  int end = bitmap_end(len);
  unsigned long v;
  int i, j;

  for (i = 0, j = 0; i < end; i++) {
    for (v = bitmap[i]; v; v &= v - 1)
      ret[j++] = i * BITS_PER_LONG + __builtin_ctzl(v);
  }
  ret[j] = -1;
  return ret;
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
edge_sorter_by_nodes(const void *a, const void *b)
{
  const struct edge *x = a;
  const struct edge *y = b;
  int ret = x->u - y->u;

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
graph_build(struct graph *graph, struct pkg_array *array)
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
      if (dep->type == dep_predepends || dep->type == dep_depends)
        deparray.n_deps += 1;
    }
  }
  n_installed = i;

  deparray.deps = m_calloc(deparray.n_deps, sizeof(deparray.deps[0]));
  for (i = 0, j = 0; i < n_installed; i++) {
    struct dependency *dep;

    for (dep = array->pkgs[i]->installed.depends; dep; dep = dep->next) {
      if (dep->type == dep_predepends || dep->type == dep_depends)
        deparray.deps[j++] = dep;
    }
  }
  qsort(deparray.deps, deparray.n_deps, sizeof(deparray.deps[0]), dep_sorter_by_pointer);

  edges = m_malloc(deparray.n_deps * sizeof(edges[0]));
  for (i = 0; i < deparray.n_deps; i++) {
    edges[i].u = pkg_array_index_of(array, deparray.deps[i]->up);
    edges[i].v = -1;
  }

  for (i = 0; i < array->n_pkgs; i++) {
    struct pkginfo *pkg = array->pkgs[i];
    struct pkginfo *ppkg = pkg_installed_provider(pkg);
    struct deppossi *possi;
    int v;

    if (!ppkg)
      continue;

    v = (ppkg == MULTIPLE_PROVIDERS) ? -2 : pkg_array_index_of(array, ppkg);

    for (possi = pkg->set->depended.installed; possi; possi = possi->rev_next) {
      struct dependency *dep = possi->up;
      struct edge *edge;

      if (dep->type != dep_depends && dep->type != dep_predepends)
        continue;

      edge = &edges[dep_array_index_of(&deparray, dep)];
      if (edge->v != -1 && edge->v != v)
        edge->v = -2;
      else
        edge->v = v;
    }
  }

  qsort(edges, deparray.n_deps, sizeof(edges[0]), edge_sorter_by_nodes);

  graph->n_nodes = n_installed;
  for (i = 0, j = 0; i < deparray.n_deps; i++) {
    if (edges[i].v >= 0 && (j == 0 || memcmp(&edges[i], &edges[i - 1], sizeof(edges[0]))))
      j += 1;
  }
  graph->n_edges = j;
  graph->edges = m_calloc(2 * graph->n_nodes + graph->n_edges, sizeof(graph->edges[0]));

  for (i = 0, j = 0, k = graph->n_nodes; i < graph->n_nodes; i++) {
    graph->edges[i] = k;
    for (; j < deparray.n_deps && edges[j].u == i; j++) {
      if (edges[j].v >= 0 && (j == 0 || memcmp(&edges[j], &edges[j - 1], sizeof(edges[0]))))
        graph->edges[k++] += edges[j].v;
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

  dep_array_destroy(&deparray);
  free(edges);
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

static void
leaves_add(struct leaves *leaves, int *scc)
{
  if (leaves->n_sccs + 1 >= leaves->cap_sccs) {
    leaves->cap_sccs *= 2;
    leaves->sccs = m_realloc(leaves->sccs, leaves->cap_sccs * sizeof(leaves->sccs[0]));
  }
  leaves->sccs[leaves->n_sccs++] = scc;
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
naive_leaves(struct leaves *leaves, const struct graph *graph)
{
  int *incoming = m_calloc(graph->n_nodes, sizeof(incoming[0]));
  int *leaf;
  int u, v;

  for (u = 0; u < graph->n_nodes; u++) {
    const int *edges = graph_edges_from(graph, u);

    for (v = *edges++; v >= 0; v = *edges++)
      incoming[v] += 1;
  }

  leaves_init(leaves);
  for (u = 0; u < graph->n_nodes; u++) {
    if (incoming[u])
      continue;

    leaf = m_malloc(2 * sizeof(leaf[0]));
    leaf[0] = u;
    leaf[1] = -1;
    leaves_add(leaves, leaf);
  }

  free(incoming);
}

static void
kosaraju(struct leaves *leaves, const struct graph *graph)
{
  struct graph rgraph;
  int N = graph->n_nodes;
  int *rstack = m_malloc(N * sizeof(rstack[0]));
  int *stack = m_malloc(N * sizeof(stack[0]));
  int *eidx = m_calloc(N, sizeof(eidx[0]));
  unsigned long *tag = bitmap_new(N);
  unsigned long *scc;
  unsigned long *sccredges;
  int rtop = 0;
  int top = 0;
  int i;

  /*
   * do depth-first searches in the graph and push nodes to rstack
   * "on the way up" until all nodes have been pushed.
   * tag nodes as they're processed so we don't visit them more than once
   */
  for (i = 0; i < N; i++) {
    if (bitmap_has(tag, i))
      continue;

    stack[top++] = i;
    bitmap_setbit(tag, i);
    while (top) {
      int u = stack[top - 1];
      int j = eidx[top - 1];
      int v = graph_edges_from(graph, u)[j];

      if (v < 0) {
        eidx[--top] = 0;
        rstack[rtop++] = u;
        continue;
      }

      eidx[top - 1] = j + 1;
      if (bitmap_has(tag, v))
        continue;

      stack[top++] = v;
      bitmap_setbit(tag, v);
    }
  }
  free(eidx);

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
  graph_reverse(&rgraph, graph);
  scc = bitmap_new(N);
  sccredges = bitmap_new(N);
  leaves_init(leaves);
  while (rtop) {
    i = rstack[--rtop];
    if (!bitmap_has(tag, i))
      continue;

    stack[top++] = i;
    bitmap_clearbit(tag, i);
    while (top) {
      int u = stack[--top];
      const int *redges = graph_edges_from(&rgraph, u);
      int v;

      bitmap_setbit(scc, u);
      for (v = *redges++; v >= 0; v = *redges++) {
        bitmap_setbit(sccredges, v);
        if (!bitmap_has(tag, v))
          continue;

        stack[top++] = v;
        bitmap_clearbit(tag, v);
      }
    }

    bitmap_subtract(sccredges, scc, N);
    if (bitmap_empty(sccredges, N))
      leaves_add(leaves, bitmap_to_array(scc, N));
    else
      bitmap_clear(sccredges, N);

    bitmap_clear(scc, N);
  }

  bitmap_free(sccredges);
  bitmap_free(scc);
  graph_destroy(&rgraph);
  bitmap_free(tag);
  free(stack);
  free(rstack);
}

static int
showleaves(void)
{
  struct dpkg_error err;
  struct pkg_array array;
  struct graph graph;
  struct leaves leaves;
  struct pkg_format_node *fmt;
  struct pager *pager;
  bool format_needs_db_fsys;
  int i, j;

  fmt = pkg_format_parse(leavesformat, &err);
  if (!fmt) {
    notice(_("error in format: %s"), err.str);
    dpkg_error_destroy(&err);
    return 1;
  }
  format_needs_db_fsys = pkg_format_needs_db_fsys(fmt);

  modstatdb_open(msdbrw_readonly);
  pkg_array_init_from_hash(&array);
  graph_build(&graph, &array);
  kosaraju(&leaves, &graph);
  leaves_sort(&leaves, leaves_sorter_by_first_node);

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

      printf("%c ", mark);
      pkg_format_show(fmt, pkg, &pkg->installed);
      mark = ' ';
    }
  }

  m_output(stdout, _("<standard output>"));
  m_output(stderr, _("<standard error>"));

  pager_reap(pager);

  leaves_destroy(&leaves);
  graph_destroy(&graph);
  pkg_array_destroy(&array);
  modstatdb_shutdown();
  pkg_format_free(fmt);
  return 0;
}

static void
set_root(const struct cmdinfo *cip, const char *value)
{
  instdir = dpkg_fsys_set_dir(value);
  admindir = dpkg_fsys_get_path(ADMINDIR);
}

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
"  -?, --help                       Show this help message.\n"
"      --version                    Show the version.\n"
"\n"));

  printf(_(
"Options:\n"
"  --admindir=<directory>           Use <directory> instead of %s.\n"
"  --root=<directory>               Use <directory> instead of %s.\n"
"  --no-pager                       Disables the use of any pager.\n"
"  -p|--priority=<priority>         Show only packages with this priority or higher.\n"
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
  { "admindir",      0,  1,  NULL,  &admindir,      NULL,          0,  NULL,  NULL },
  { "root",          0,  1,  NULL,  NULL,           set_root,      0,  NULL,  NULL },
  { "no-pager",      0,  0,  NULL,  NULL,           set_no_pager,  0,  NULL,  NULL },
  { "priority",    'p',  1,  NULL,  NULL,           set_priority,  0,  NULL,  NULL },
  { "format",      'f',  1,  NULL,  &leavesformat,  NULL,          0,  NULL,  NULL },
  { "help",        '?',  0,  NULL,  NULL,           usage,         0,  NULL,  NULL },
  { "version",       0,  0,  NULL,  NULL,           printversion,  0,  NULL,  NULL },
  {  /* sentinel */ }
};

int main(int argc, const char *const *argv)
{
  int ret;

  dpkg_set_report_piped_mode(_IOFBF);
  dpkg_locales_init(PACKAGE);
  dpkg_program_init("dpkg-leaves");
  dpkg_options_parse(&argv, cmdinfos, printforhelp);
  if (argv[0])
    badusage(_("unknown command '%s'"), argv[0]);

  instdir = dpkg_fsys_set_dir(instdir);
  admindir = dpkg_db_set_dir(admindir);

  ret = showleaves();

  dpkg_program_done();
  dpkg_locales_done();

  return !!ret;
}

/* vim: set ts=2 sw=2 et */
