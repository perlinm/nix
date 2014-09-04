#!/usr/bin/python3
import matplotlib as mp
import matplotlib.cm as cm
import numpy as np
import subprocess as sp
import time, sys, datetime, select, threading, re, alsaaudio, pyinotify

logfile = '/home/perlinm/scripts/.dzen-bar.log'
def log(str):
    with open(logfile,'a') as l:
        l.write('%s\n'%str)

left = 'xmonad'
center = 'cpu'
right = 'network bat light vol clock'

soundcard = 0
eth_interface = 'eno1'
wl_interface = 'wlp2s0'

network_width = 21
client_width = 58
tray_width = 8
tray_height = 16
background = '#111111'
workspaces = 11
char_width = 7 # Monospace-[8/9]
bar_height = 8 # volume, brightness, cpu, etc.

wifi_pad = 'alt+F3'
htop_pad = 'alt+F4'

ws_keys = ['grave']+[str(n) for n in range(1,10)]+['0']
left_keys = ['super+' + k for k in ws_keys+['z']*2]
mid_keys = ['']*12+['super+shift+slash']
right_keys = ['alt+z']*11+['super+space','super+slash']
scroll_up_keys = ['super+' + k for k in ['Right']*12+['w']]
scroll_down_keys = ['super+' + k for k in ['Left']*12+['f']]

# color definitions
fullBlack = [0]*3 # "#000000"
fullWhite = [1]*3 # "#ffffff"
black = [2/30.]*3 # "#111111"
white = [.87]*3 # "#dddddd"
gray = [.47]*3 # "#777777"
red = [.76, .11, .09] # "#c11b17"
green = [.21, .49, .09] # "#347c17"
blue = [0, .41, .55] # "#00688b"
yellow = [1., .73, 0] # "#ffbb00"

textcolor = mp.colors.rgb2hex(white)

color_maps = [m for m in cm.datad if not m.endswith("_r")]
def get_cmap(name):
  return cm.get_cmap(color_maps[color_maps.index(name)])

# color map test
testnum = 0
def test_cmap(cmap):
  nums = ''
  for i in range(0, 101, 2):
    color = mp.colors.rgb2hex(cmap(i/100.))
    nums += '^fg(' + color + ')' + str(i) + ' '
    print(nums)
  sys.stdout.flush()
  time.sleep(500)
  exit(1)

# format strings for dzen; return string and its display length
spacer = '  '
tail = '..'
default_color = textcolor
default_keysyms = [['','']]
def one_fstr(align,string,length,color,keysyms,current_keys,bar):
  pre_text = ''
  end_text = ''

  out_str = str(string).replace('\n','')
  pad = ['','']
  if not bar:
    # construct text and truncate and/or pad with spaces if necessary
    if len(out_str) > length:
      out_str = out_str[:length-len(tail)]+tail
    elif len(out_str) < length:
      pad[1 if align == 'l' else 0] = ' '*(length-len(out_str))

  # add keypress options if necessary
  if keysyms != default_keysyms:
    try:
      while current_keys[-1] not in keysyms[1]:
        pre_text += '^ca()'
        current_keys.pop()
    except: None
    for i in range(len(keysyms)):
      if keysyms[i][1] not in current_keys and keysyms[i][1] != '':
        pre_text += ('^ca(%s,xdotool key %s)'
                     %(str(keysyms[i][0]),keysyms[i][1]))
        current_keys.append(keysyms[i][1])

  # color text, if the color has changed
  if type(color) is list or type(color) is tuple:
    color = mp.colors.rgb2hex([color[0],color[1],color[2]])
  pre_text += '^fg(%s)'%color
  end_text += '^fg()'

  return pre_text + pad[0] + out_str + pad[1] + end_text, \
    length, current_keys

def fstr(align,string,length,color=default_color,
         keysyms=default_keysyms,bars=False):
  if type(keysyms[0]) is not list: keysyms = [keysyms]
  if not type(string) is list:
    out_str, out_length, current_keys = \
      one_fstr(align,string,length,color,keysyms,[],bars)
  else:
    if color == default_color:
      color = [color]*len(string)
    if keysyms == default_keysyms:
      multi_keysyms = [keysyms]*len(string)
    else:
      multi_keysyms = []
      for i in range(len(string)):
        multi_keysyms.append([])
        for j in range(len(keysyms)):
          multi_keysyms[i].append([keysyms[j][0],keysyms[j][1][i]])
    out_str = ''
    out_length = 0
    current_keys = []
    if type(bars) is not list:
      bars = [bars]*len(string)
    for i in range(len(string)):
      str_info = one_fstr(align,string[i],length[i],color[i],
                          multi_keysyms[i],current_keys,bars[i])
      out_str += str_info[0]
      out_length += str_info[1]
      current_keys = str_info[2]
      if i+1 < len(string):
        out_str += ' '
        out_length += 1
  out_str += '^ca()'*len(current_keys)
  return out_str, out_length

def make_bar(fill,chars):
  full_width = chars*char_width
  bar_width = int(fill*full_width)
  return '^r({1}x{0})^ro({2}x{0})'.format(
    bar_height,bar_width,full_width-bar_width)

# date and time
def clock(align):
  now = datetime.datetime.now()
  time = now.strftime('%a %Y-%m-%d %H:%M:%S')
  return fstr(align,time,23)

# volume
cm_light = get_cmap('cool')
p = re.compile('\w')
def volume(align):
  state = sp.getoutput('pamixer --get-mute')
  vol = int(sp.getoutput('pamixer --get-volume'))
  if state == 'false' and vol > 0:
    color = cm_light(1-int(vol)/100.)
  else:
    color = red

  chars = 4
  bar_text = make_bar(vol/100,chars)
  return fstr(align,bar_text,chars,color,
              [[1,'ctrl+slash'],[3,'ctrl+shift+slash'],
               [4,'ctrl+Down'],[5,'ctrl+Up']],True)

# screen brightness
def light(align):
  value = float(sp.getoutput('xbacklight'))
  chars = 4
  bar_text = make_bar(value/100,chars)
  return fstr(align,bar_text,chars,yellow,
              [[1,'alt+slash'],[3,'alt+shift+slash'],
               [4,'alt+Down'],[5,'alt+Up']],True)

# battery
cm_bat = get_cmap('autumn')
def battery(align):
  acpi = sp.getoutput('acpi').split()
  state = acpi[2][:-1]
  charge = acpi[3][:acpi[3].index('%')]
  charge_color = cm_bat(int(charge)/100.)
  time_color = textcolor

  if 'Charging' in state or 'Discharging' in state:
    time = acpi[4][:-3]
    time += '+' if 'Charging' in state else '-'
  elif 'Full' in state:
    time = ''
  else:
    time = '?????'

  chars = 4
  bar_text = make_bar(float(charge)/100,chars)
  return fstr(align,[time,bar_text],[6,chars],
              [time_color,charge_color],bars=[False,True])

# network
def network_up(interface):
  state = sp.getoutput('cat /sys/class/net/%s/operstate'%interface)
  return True if state == 'up' else False

def network(align):
  if network_up(eth_interface):
    string = 'ethernet'
  elif network_up(wl_interface):
    link_quality = '??'
    signal_strength = '??'
    with open('/proc/net/wireless','r') as f:
      for line in f:
        if wl_interface in line:
          line = line.split()
          link_quality = int(float(line[2])*100/70)
          signal_strength = int(float(line[3]))
    network_name = sp.getoutput('iwgetid -r')
    if not network_name:
      network_name = '...'
    string = (str(link_quality)+'% '+str(signal_strength)+'dBm '
              +network_name)
  else:
    string = 'offline'
  return fstr(align,string,network_width,keysyms=[1,wifi_pad])

# xmonad info
ws_color = {
  'c' : blue,
  'v' : yellow,
  'h' : green,
  'hnw' : white,
  'u' : red
}
layout_length = 6

def xmonad(line,align):
  info = line.split('|:|')
  ws = info[0].split('|')
  layout = info[1].split()[0]
  title = info[2] if len(info) > 2 else ''
  colors = []
  strings = []
  lengths = []
  for i in range(len(ws)):
    colors.append(ws_color[ws[i].split()[0]])
    strings.append(ws[i].split()[1])
    lengths.append(len(strings[i]))
  colors += [textcolor,yellow]
  strings += [layout,title]
  lengths += [layout_length,client_width]
  return fstr(align,strings,lengths,colors,
              [[4,scroll_down_keys],[5,scroll_up_keys],
               [3,right_keys],[2,mid_keys],[1,left_keys]])

# system tray
def systray():
  pad = width() - max_size(right)
  sp.call(['killall','stalonetray'])
  sp.Popen(['stalonetray','--background',background,'--geometry',
            str(int(tray_width*char_width/tray_height))
            + 'x1+' + str(int((pad+0.5)*char_width)) + '+0',
            '--lower-on-start'])

# cpu info
cm_cpu = get_cmap('hot')

def get_idles():
  stat = []
  with open('/proc/stat','r') as f:
    for line in f:
      if not 'cpu' in line:
        break
      stat.append(line)
  idles = np.array([int(s.split()[4]) for s in stat[1:]])
  return idles

with open('/proc/cpuinfo', 'r') as f:
  for line in f:
    if 'cpu cores' in line:
      ncores = int(line.split()[-1])

old_idles = get_idles()
old_cpu_time = datetime.datetime.min
threads_per_core = int(len(old_idles)/ncores)
cpu_len = (threads_per_core*3 + 2)*ncores + 1

def cpu(align):
  global old_idles, old_cpu_time
  idles = get_idles()
  cpu_time = datetime.datetime.now()
  dt = (cpu_time - old_cpu_time).total_seconds()
  vals = list(100 - (idles - old_idles)/dt)
  colors = []
  for i in range(len(vals)):
    colors.append([vals[i]/100]*3)

  width = 3
  lengths = [width]*len(vals)
  bar_texts = []
  for i in range(len(vals)):
    bar_texts.append(make_bar(float(vals[i])/100,width))

  old_idles = idles
  old_cpu_time = cpu_time
  return fstr(align,bar_texts,lengths,colors,
              [1,[htop_pad]*4],bars=True)

# function and value dictionaries
left = left.split()
center = center.split()
right = right.split()
used_funs = left+center+right

all_funs = {
  'bat' : lambda: battery(aligns['bat']),
  'clock' : lambda: clock(aligns['clock']),
  'cpu' : lambda: cpu(aligns['cpu']),
  'light' : lambda: light(aligns['light']),
  'mem' : lambda: memory(aligns['mem']),
  'network' : lambda: network(aligns['network']),
  'temp' : lambda: core_temp(aligns['temp']),
  'vol' : lambda: volume(aligns['vol']),
  'weather' : lambda: weather(aligns['weather']),
  'xmonad' : lambda line: xmonad(line,aligns['xmonad']),
}
arg_funs = ['xmonad']

all_timed_funs = [[1,['clock', 'cpu', 'mem',
                      'temp', 'network']],
                      [5,['bat']], [600,['weather']]]
timed_funs = []
for i in range(len(all_timed_funs)):
  timed_funs.append([all_timed_funs[i][0],
                     [f for f in all_timed_funs[i][1]
                      if f in used_funs]])
funs = dict((f, all_funs[f]) for f in used_funs)
aligns = dict((f, 'l' if f in left else 'r') for f in used_funs)
vals = dict((f, funs[f]() if f not in arg_funs
             else ['',0]) for f in funs)

# bar width in characters
def width():
  return int(int(sp.getoutput('xrandr').split()[7])/char_width)

# maximum size of left/right bars
def max_size(bar):
  size = (width()-section_length(center)[0])/2
  return int(np.floor(size)) if bar == left else int(np.ceil(size))

# length of sections in characters
def section_length(bar):
  if len(bar) == 0:
    return 0, 0
  length = len(spacer)*(len(bar)-1)
  for i in range(len(bar)):
    length += vals[bar[i]][1]

  truncation = 0
  # account for spacer to center section
  if bar == center:
    length += 2*len(spacer)
  # truncate left and right sections if necessary
  elif length > max_size(bar):
    truncation = length - max_size(bar)
    length = max_size(bar)

  return length, truncation

# section text
def section_text(bar):
  text = ''
  for i in range(len(bar)):
    text += vals[bar[i]][0]
    if i+1 < len(bar):
      text += spacer

  truncation = section_length(bar)[1]
  # add spacers to center section
  if bar == center:
    text = spacer + text + spacer
  # truncate left and right sections if necessary
  elif truncation != 0:
    if bar == left:
      if text[-truncation:] == ' '*truncation:
        text = text[:-truncation]
      else:
        text = text[:-(truncation+len(tail))] + tail
    if bar == right:
      if text[:truncation] == ' '*truncation:
        text = text[truncation:]
      else:
        text = tail + text[truncation+len(tail):]

  return text

old_width = 0
l_pad = 0
r_pad = 0
# compile text for entire bar
def bar_text(seconds):
  global l_pad, r_pad, old_width
  # if width has changed, adjust padding between sections
  if width() != old_width:
    old_width = width()
    l_pad = max_size(left) - section_length(left)[0]
    r_pad = max_size(right) - section_length(right)[0]
    systray()

  return (section_text(left) + ' '*l_pad + section_text(center)
          + ' '*r_pad + section_text(right))

# polling functions
seconds = 1
def second_poll():
  time.sleep(1)
  global seconds
  while True:
    now = time.time()
    for i in range(len(timed_funs)):
      if seconds % timed_funs[i][0] == 0:
        for j in range(len(timed_funs[i][1])):
          vals[timed_funs[i][1][j]] = funs[timed_funs[i][1][j]]()
    print(bar_text(seconds))
    sys.stdout.flush()
    seconds += 1
    elapsed = time.time() - now
    if elapsed < 1:
      time.sleep(1. - elapsed)


### FIXME: update volume corretly
#mixer = alsaaudio.Mixer(cardindex = soundcard)
def vol_poll():
  if 'vol' not in used_funs: return None
  global vals
  global mixer

  while True:
    vals['vol'] = funs['vol']()
    print(bar_text(seconds))
    sys.stdout.flush()
    time.sleep(1)

#  p = select.poll()
#  while True:
#    fd,em = mixer.polldescriptors()[0]
#    p.register(fd,em)
#    p.poll()
#    mixer = alsaaudio.Mixer(cardindex = soundcard)
#    vals['vol'] = funs['vol']()
#    print(bar_text(seconds))
#    sys.stdout.flush()
#    p.unregister(fd)

class BrightnessHandler(pyinotify.ProcessEvent):
  def process_IN_OPEN(self,event):
    vals['light'] = funs['light']()
    print(bar_text(seconds))
    sys.stdout.flush()

def light_poll():
  wm = pyinotify.WatchManager()
  wm.add_watch('/sys/class/backlight/intel_backlight/uevent',
               pyinotify.IN_OPEN)
  eh = BrightnessHandler()
  notifier = pyinotify.Notifier(wm, eh)
  notifier.loop()

def xmonad_poll():
  if 'xmonad' not in used_funs: return None
  global vals
  for line in sys.stdin:
    vals['xmonad'] = funs['xmonad'](line)
    print(bar_text(seconds))
    sys.stdout.flush()

second_thread = threading.Thread(target=second_poll)
second_thread.start()

vol_thread = threading.Thread(target=vol_poll)
vol_thread.start()

light_thread = threading.Thread(target=light_poll)
light_thread.start()

xmonad_thread = threading.Thread(target=xmonad_poll())
xmonad_thread.start()



'''
# weather
def weather(align):
  weath = pywapi.get_weather_from_noaa('KCVO')
  temp = str(int(float(weath['temp_c']))) + ' C'
  return fstr(align, textcolor, temp, 5)

# core temp
tempcols = [(0.0, (0, 1, 1)),
            (0.4, (0, 1, 1)),
            (0.8, (1, 1, 0)),
            (1.0, (1, 0, 0))]
cm_temp = mp.colors.LinearSegmentedColormap.from_list('temp', tempcols, 100)

def core_temp():
  info = sp.check_output(['acpi','-t']).split()
  temp = int(float(info[3]))
  color = mp.colors.rgb2hex(cm_temp(temp/100.))
  return fstr(color,temp+' C',4)

# memory
memcols = [(0.0, (0, 1, 1)),
            (0.4, (0, 1, 1)),
            (0.8, (1, 1, 0)),
            (1.0, (1, 0, 0))]
cm_mem = mp.colors.LinearSegmentedColormap.from_list('mem', memcols, 100)

mem_total = int(sp.check_output('free').split()[7])*2.**-20
mem_len = 5 if mem_total < 10 else 6

def memory():
  mem_free = int(sp.check_output('free').split()[16])*2.**-20
  used_frac = (mem_total - mem_free)/mem_total
  color = mp.colors.rgb2hex(cm_mem(used_frac))
  if mem_free < 1:
    post = ' M'
    mem = '%3i' %(mem_free*1024)
  else:
    post = ' G'
    mem = '%.1f' %(mem_free)
  return '^fg(' + color + ')' + mem + post, mem_len
'''
