local wezterm = require 'wezterm'
local config = wezterm.config_builder()

-- configuration reference: https://wezfurlong.org/wezterm/config/lua/config/index.html

config.font_size = 14
config.window_background_opacity = 0.75

-- color scheme
-- https://wezfurlong.org/wezterm/config/appearance.html
-- https://wezfurlong.org/wezterm/colorschemes/index.html
local onedark_perlinm = wezterm.color.get_builtin_schemes()['Breeze (Gogh)']
onedark_perlinm.background = 'black'
onedark_perlinm.foreground = 'white'
config.color_schemes = {['onedark_perlinm'] = onedark_perlinm}
config.color_scheme = 'onedark_perlinm'

config.hide_tab_bar_if_only_one_tab = true
-- config.show_close_tab_button_in_tabs = false

-- broken: unbind <C-i> from <Tab>
-- https://github.com/wez/wezterm/issues/1851
config.keys = {
  {
    key = 'phys:i',
    mods = 'CTRL',
    action = wezterm.action.SendKey { key = 'phys:i', mods = 'CTRL' },
  },
}

return config
