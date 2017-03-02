# dependency
import matplotlib as mp

# figure parameters
fig_x = 15
fig_y = 10
font_size = 12
lineWidth = 1
rcParams["font.size"] = font_size
rcParams["axes.titlesize"] = font_size
rcParams["legend.fontsize"] = font_size
rcParams["legend.numpoints"] = 1
rcParams["xtick.labelsize"] = font_size
rcParams["ytick.labelsize"] = font_size
rcParams["lines.linewidth"] = lineWidth
rcParams["axes.color_cycle"] = ["k","b","g","r","c","m","y"]

# figure template
def new_fig():
    fig = figure(figsize=(fig_x/2.54,fig_y/2.54))
    ax = fig.add_subplot(1,1,1)
    fmt = mp.ticker.ScalarFormatter(useMathText=True)
    fmt.set_scientific(True)
    fmt.set_powerlimits((0,3))
    ax.yaxis.set_major_formatter(fmt)
    ax.xaxis.set_major_formatter(fmt)
    return fig, ax

# make figure of desired size
def make_fig():
  figure(figsize=(fig_x,fig_y))
if not "font_size" in globals():
	font_size = 7

# set fonts and use latex packages
font = {"family" : "serif",
        "serif":["Computer Modern"],
        "size" : font_size}
rc("font",**font)
params = {"legend.fontsize": font_size,
          "text.usetex" : True,
          "text.latex.preamble" : r"\usepackage{amsmath}"}
rcParams.update(params)
