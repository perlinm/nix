# dependency
import matplotlib.pyplot as plt

# set fonts and use latex packages
font_size = 10
params = {
    "font.family": "serif",
    "font.serif": "Computer Modern",
    "text.usetex": True,
    "text.latex.preamble": r"\usepackage{amsmath}",
    "font.size": font_size,
}
plt.rcParams.update(params)


# default color cycle and conversion between hex and RGB color values
color_cycle = plt.rcParams["axes.prop_cycle"].by_key()["color"]


def hex_to_rgb(color):
    return tuple(int(color[1 + 2 * jj : 1 + 2 * jj + 1], 16) / 16 for jj in range(3))


# color-blind-friendly color cycle, "New Tableau 10":
# https://www.tableau.com/about/blog/2016/7/colors-upgrade-tableau-10-56782
colors = dict(
    blue="#4E79A7",
    orange="#F28E2B",
    red="#E15759",
    cyan="#76B7B2",
    green="#59A14E",
    yellow="#EDC949",
    purple="#B07AA2",
    pink="#FF9DA7",
    brown="#9C755F",
    grey="#BAB0AC",
)
plt.rcParams["axes.prop_cycle"] = plt.cycler(color=colors.values())


# rasterizing figure, but not the text
plt.gca().set_rasterization_zorder(1)  # rasterized anything with zorder < 1
# plot(stuff, zorder = 0)
# xlabel(words, zorder = 1)
# savefig(things, rasterized = True, dpi = fig_dpi)

# figure parameters
fig_x = 15
fig_y = 10
font_size = 12
lineWidth = 1
plt.rcParams["font.size"] = font_size
plt.rcParams["axes.titlesize"] = font_size
plt.rcParams["legend.fontsize"] = font_size
plt.rcParams["legend.numpoints"] = 1
plt.rcParams["xtick.labelsize"] = font_size
plt.rcParams["ytick.labelsize"] = font_size
plt.rcParams["lines.linewidth"] = lineWidth
plt.rcParams["axes.color_cycle"] = ["k", "b", "g", "r", "c", "m", "y"]


# using scientific notation for plot axes
plt.ticklabel_format(axis="both", style="sci", scilimits=(-2, 2))


# setting colorbar limits (with pcolormesh) and scientific notation
cb = plt.colorbar()
plt.clim(0, 1)  # range from 0 to 1
cb.formatter.set_powerlimits((-2, 3))
cb.update_ticks()
