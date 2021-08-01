import numpy as np
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import Circle, Ellipse
from matplotlib.collections import PatchCollection

import argparse
import os
import pandas as pd
import re


class ParseError(Exception):
    pass


class LogError(Exception):
    pass


plt.style.use('default')
plt.rcParams['font.size'] = 12
plt.rcParams['axes.labelsize'] = 12
plt.rcParams['axes.labelweight'] = 'bold'
plt.rcParams['xtick.labelsize'] = 10
plt.rcParams['ytick.labelsize'] = 10
plt.rcParams['legend.fontsize'] = 12
plt.rcParams['figure.titlesize'] = 14

argparser = argparse.ArgumentParser()
argparser.add_argument("path",
                       default="_bench/",
                       help="Path to a stats.log file or a directory")
argparser.add_argument(
    "-R",
    action="store_true",
    help="Recursively follow directories in [path] to find *.log files")
argparser.add_argument("--with-log",
                       action="store_true",
                       help="Looks for b.log to plot additional information")
argparser.add_argument("--modules",
                       nargs="+",
                       choices=["Btree", "Store", "Nodes", "Utils"],
                       default=["Btree", "Store", "Nodes", "Utils"],
                       help="Plots only functions from provided modules")
argparser.add_argument("--names", help="Regexp on the functions name to plot.")
argparser.add_argument("--density",
                       default=0.1,
                       type=float,
                       help="Density of the plot.")
argparser.add_argument("--range",
                       help="Only plot the given second range",
                       nargs=2,
                       type=int)


def find_dirs(path, with_log=False, rec=False):
    if os.path.isfile(path):
        raise ParseError(f"{path} is not a directory.")
    elif os.path.isdir(path):
        paths = []
        if rec:
            for (dirpath, _dirnames, filenames) in os.walk(path):
                if "stats.log" in filenames:
                    paths.append(dirpath)
                    if with_log and not "b.log" in filenames:
                        raise LogError(
                            f"{dirpath} : unable to find matching \"b.log\" file"
                        )
        else:
            if "stats.log" in os.listdir(path):
                paths.append(path)
                if with_log and not "b.log" in os.listdir(path):
                    raise LogError(
                        f"{dirpath} : unable to find matching \"b.log\" file")
        return paths
    return []


def read_stats(path):
    columns = ["t", "module", "function", "histogram"]
    dtypes = {
        "t": np.float64,
        "module": str,
        "function": str,
        "histogram": object
    }
    return pd.read_csv(os.path.join(path, "stats.log"),
                       sep=";",
                       header=None,
                       names=columns,
                       dtype=dtypes)


def read_log(path):
    t, heights = [], []
    with open(os.path.join(path, "b.log"), "r") as log:
        for match in re.finditer(
                r"\[\+(\d+)ms\] \[INFO\] Btree height increases to (\d+)",
                log.read()):
            t.append(float(match.group(1)))
            heights.append(int(match.group(2)))
    return t, heights


cmap = plt.get_cmap("Set1")  # color palette


def legendpoint(color, label):
    # creates an Artist to override legend markers
    return Line2D([0], [0],
                  marker='o',
                  color='w',
                  label=label,
                  markerfacecolor=color,
                  markersize=5)


def get_handles(labels):
    return [legendpoint(cmap(i), label) for i, label in enumerate(labels)]


def set_labels(fig, xlabel, ylabel):
    # set global x and y axis labels over a grid of axis
    bx = fig.add_subplot(111, frameon=False, alpha=0)
    bx.set_yticklabels([])
    bx.set_xticklabels([])
    bx.set_yticks([])
    bx.set_xticks([])
    bx.grid(False)
    bx.set_xlabel(xlabel, labelpad=40)
    bx.set_ylabel(ylabel, labelpad=40)


def deserialize(histogram):
    # histogram has the form "[(1.2183, 3), (2.10293, 7), ...]"
    centers, counts = zip(*eval(histogram))
    return np.exp(np.array(centers)), np.array(counts)


def ellipses(x, y, w, h=None, rot=0.0, c='b', vmin=None, vmax=None, **kwargs):
    """
    Make a scatter plot of ellipses. 
    Parameters
    ----------
    x, y : scalar or array_like, shape (n, )
        Center of ellipses.
    w, h : scalar or array_like, shape (n, )
        Total length (diameter) of horizontal/vertical axis.
        `h` is set to be equal to `w` by default, ie. circle.
    rot : scalar or array_like, shape (n, )
        Rotation in degrees (anti-clockwise).
    c : color or sequence of color, optional, default : 'b'
        `c` can be a single color format string, or a sequence of color
        specifications of length `N`, or a sequence of `N` numbers to be
        mapped to colors using the `cmap` and `norm` specified via kwargs.
        Note that `c` should not be a single numeric RGB or RGBA sequence
        because that is indistinguishable from an array of values
        to be colormapped. (If you insist, use `color` instead.)
        `c` can be a 2-D array in which the rows are RGB or RGBA, however.
    vmin, vmax : scalar, optional, default: None
        `vmin` and `vmax` are used in conjunction with `norm` to normalize
        luminance data.  If either are `None`, the min and max of the
        color array is used.
    kwargs : `~matplotlib.collections.Collection` properties
        Eg. alpha, edgecolor(ec), facecolor(fc), linewidth(lw), linestyle(ls),
        norm, cmap, transform, etc.
    Returns
    -------
    paths : `~matplotlib.collections.PathCollection`
    Examples
    --------
    a = np.arange(11)
    ellipses(a, a, w=4, h=a, rot=a*30, c=a, alpha=0.5, ec='none')
    plt.colorbar()
    License
    --------
    This code is under [The BSD 3-Clause License]
    (http://opensource.org/licenses/BSD-3-Clause)
    """
    if np.isscalar(c):
        kwargs.setdefault('color', c)
        c = None

    if 'fc' in kwargs:
        kwargs.setdefault('facecolor', kwargs.pop('fc'))
    if 'ec' in kwargs:
        kwargs.setdefault('edgecolor', kwargs.pop('ec'))
    if 'ls' in kwargs:
        kwargs.setdefault('linestyle', kwargs.pop('ls'))
    if 'lw' in kwargs:
        kwargs.setdefault('linewidth', kwargs.pop('lw'))
    # You can set `facecolor` with an array for each patch,
    # while you can only set `facecolors` with a value for all.

    if h is None:
        h = w

    zipped = np.broadcast(x, y, w, h, rot)
    patches = [
        Ellipse((x_, y_), w_, h_, rot_) for x_, y_, w_, h_, rot_ in zipped
    ]
    collection = PatchCollection(patches, **kwargs)
    if c is not None:
        c = np.broadcast_to(c, zipped.shape).ravel()
        collection.set_array(c)
        collection.set_clim(vmin, vmax)

    ax = plt.gca()
    ax.add_collection(collection)
    ax.autoscale_view()
    plt.draw_if_interactive()
    if c is not None:
        plt.sci(collection)
    return collection


def lighten_color(color, amount=0.5):
    """
    Lightens the given color by multiplying (1-luminosity) by the given amount.
    Input can be matplotlib color string, hex string, or RGB tuple.

    Examples:
    >> lighten_color('g', 0.3)
    >> lighten_color('#F034A3', 0.6)
    >> lighten_color((.3,.55,.1), 0.5)
    """
    import matplotlib.colors as mc
    import colorsys
    try:
        c = mc.cnames[color]
    except:
        c = color
    c = colorsys.rgb_to_hls(*mc.to_rgb(c))
    return colorsys.hls_to_rgb(c[0], 1 - amount * (1 - c[1]), c[2])


def match(regexp, s):
    m = re.match(regexp, s)
    if m:
        return m.group(0) == s
    return False


def plot(df, modules, re_name, t_height, density, _range):
    whole_df = df[df.module.isin(modules)]
    if _range is not None:
        low, high = _range
        low, high = low * 1_000, high * 1_000  # sec to msec conversion
        whole_df = whole_df[(whole_df.t >= low) & (whole_df.t <= high)]
    n_ops = whole_df.histogram.apply(lambda x: deserialize(x)[1].sum())
    whole_df = whole_df.assign(n_ops=n_ops)
    groups = whole_df.groupby("module")
    n = len(groups)
    w = max(1, n // 2)
    h = max(1, n // w)
    H, W = 25, 20
    fig, axs = plt.subplots(h,
                            w,
                            figsize=(H, W),
                            squeeze=False,
                            sharex=False,
                            sharey=False)
    set_labels(fig, "Elapsed time (sec)", "Operation duration (sec)")

    for i, (name, group) in enumerate(groups):
        if not modules is None and not name in modules:
            continue

        ax = axs[i // w, i % w]
        funcs = group.groupby("function")
        labels = []

        max_y = 2 * max(group.histogram.apply(
            lambda x: max(deserialize(x)[0]))) / 1_000_000  #convert us to s
        min_y = 0.5 * min(
            group.histogram.apply(lambda x: min(deserialize(x)[0]))
        ) / 1_000_000  # convert us to s
        ax.set_ylim(min_y, max_y)

        delta = (whole_df.t.max() - whole_df.t.min()) / 1000 / 20
        ax.set_xlim(whole_df.t.min() / 1000 - delta,
                    whole_df.t.max() / 1000 + delta)

        ax_ = ax.twinx()

        ax.set_yscale("log")

        ax_.set_ylim(0, 1)
        plt.sca(ax_)  #set current axis

        j = -1
        for (fname, df) in funcs:

            if re_name is not None and not match(re_name, fname):
                continue
            j += 1

            sample_period = max(1, len(df) // (500 * density // axs.shape[1]))

            def plot_row(row):
                if row.name % sample_period == 0:  #don't overcrowd the graph
                    centers_us, counts = deserialize(row.histogram)
                    centers_s = centers_us / 1_000_000
                    centers_logscale = (np.log(centers_s) - np.log(min_y)) / (
                        np.log(max_y) - np.log(min_y)
                    )  # plot on [0,1] linearly as if it was on [10^min_y, 10^max_y] logly
                    xscale = (lambda x: x[1] - x[0])(
                        ax_.get_xlim()) * W / H * w / h
                    radii = np.cbrt(
                        counts + 10
                    ) / 500  # this crushes big value while making small one still visible
                    ellipses(row.t / 1_000,
                             centers_logscale,
                             radii * xscale,
                             radii,
                             color=cmap(j),
                             alpha=0.4,
                             ec=lighten_color(cmap(j), amount=1.5))

            df.reset_index().apply(plot_row, axis=1)

            labels.append(fname)

        handles = get_handles(labels)
        if name == "Btree":
            ax_prog = ax.twinx()  # different y-scale, but shared x axis
            progress = np.cumsum(group.n_ops) / np.sum(group.n_ops)
            j += 1
            ax_prog.plot(group.t / 1000,
                         progress,
                         color=cmap(j),
                         label="Task completion")
            for i, t in enumerate(t_height):
                ax_prog.plot(
                    [t / 1000, t / 1000], [0, 1],
                    '--',
                    color=cmap(42),
                    label="Btree height increments" if i == 0 else None)
            prog_handles, _ = ax_prog.get_legend_handles_labels()
            handles += prog_handles

        ax_.set_yticks([])
        ax_.set_yticks([])
        ax_.legend(handles=handles)
        ax.set_title(name)


if __name__ == "__main__":
    args = argparser.parse_args()
    dirs = find_dirs(args.path, rec=args.R, with_log=args.with_log)
    for dir in dirs:
        print(dir)
        df = read_stats(dir)
        t, heights = read_log(dir) if args.with_log else ([], [])
        plot(df, args.modules, args.names, t, args.density, args.range)
        plt.savefig(os.path.join(dir, 'stats.png'), dpi=300)
