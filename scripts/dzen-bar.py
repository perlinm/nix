#!/usr/bin/python3
import matplotlib as mp
import matplotlib.cm as cm
import numpy as np
import subprocess as sp
import time, sys, datetime, alsaaudio, argparse, select, pywapi, threading

# arguments
parser = argparse.ArgumentParser(
    description='Returns a string containing various system status ' \
    + 'for use in a dzen status bar.')

parser.add_argument(
    '-l', '--left', default='xmonad',
    help='Things to display aligned left')

parser.add_argument(
    '-c', '--center', default='',
    help='Things to display aligned in the center')

parser.add_argument(
    '-r', '--right',
    default='network bat light vol clock',
    help='Things to display aligned right')

parser.add_argument(
    '-s', '--soundcard', type=int, default=0,
    help='Soundcard number to use for volume')

parser.add_argument(
    '-b', '--bat', type=int, default=-1,
    help='Battery number to use for power info')

parser.add_argument(
    '-e', '--eth',default='eno1', help='Ethernet interface')

parser.add_argument(
    '-w', '--wl', default='wlp2s0', help='Wireless interface')

parser.add_argument(
    '-nw', '--network_width', type=int, default=21,
    help='Network info width')

parser.add_argument(
    '-sw', '--systray_width', type=int, default=13,
    help='Systray width')

parser.add_argument(
    '-cw', '--client_width', type=int, default=25,
    help='Client name width')

parser.add_argument(
    '--spacer', default='  ', help='Spacer separating things')

parser.add_argument(
    '--char_width', type=int, default=7,
    help='Width (in px) of a character in your font. ' \
    + 'For Monospace8, it is 7.')

args = parser.parse_args()

# color definitions
fullBlack = [0, 0, 0] # "#000000"
fullWhite = [1, 1, 1] # "#FFFFFF"
black = [.1, .1, .1] # "#1A1A1A"
white = [.87, .87, .87] # "#DDDDDD"
grey = [.47, .47, .47] # "#777777"
red = [.76, .11, .09] # "#C11B17"
green = [.21, .49, .09] # "#347C17"
blue = [0, .41, .55] # "#00688B"
yellow = [1., .73, 0] # "#FFBB00"

textcolor = white

color_maps = [m for m in cm.datad if not m.endswith("_r")]
def get_cmap(name):
  return cm.get_cmap(color_maps[color_maps.index(name)])

# color map test
def test_cmap(cmap):
  nums = ''
  for i in range(0, 101, 2):
    color = mp.colors.rgb2hex(cmap(i/100.))
    nums += '^fg(' + color + ')' + str(i) + ' '
    print (nums)
  sys.stdout.flush()
  time.sleep(500)
  exit(1)

# screen resolution
def res():
  return int(sp.check_output('xrandr').split()[7])

# format strings with colors for dzen and return string length
tail = '..'
def one_cstr(color,string,length):
  if type(color) is list or type(color) is tuple:
    color = mp.colors.rgb2hex(color)
  color_text = '^fg(%s)' %color if color != None else ''
  out_str = str(string).replace('\n','')
  if length >= 0:
    if len(out_str) > length:
      out_str = out_str[:length-len(tail)]+tail
    elif len(out_str) < length:
      out_str = ' '*(length-len(out_str))+out_str
  return color_text + out_str, len(out_str)

def cstr(colors,strings,length=-1):
  if not type(strings) is list:
    out_str, out_length = one_cstr(colors,strings,length)
  else:
    if length == -1:
      length = [-1]*len(strings)
    out_str = ''
    out_length = 0
    for i in range(len(strings)):
      addition = one_cstr(colors[i],strings[i],length[i])
      out_str += addition[0]
      out_length += addition[1]
      if i+1 < len(strings):
        out_str += ' '
        out_length += 1
  return out_str, out_length

# date and time
def clock():
  now = datetime.datetime.now()
  time = now.strftime('%a %Y-%m-%d %H:%M:%S')
  return cstr(textcolor,time)

# volume
cm_light = get_cmap('cool')
def volume():
  mixer = alsaaudio.Mixer(cardindex = args.soundcard)
  vol = mixer.getvolume()[0]
  color = cm_light(1-vol/100.) if mixer.getmute()[0] == 0 else red
  return cstr(color,vol,3)

# screen brightness
def light():
  value = int(float(sp.check_output('xbacklight'))+0.5)
  return cstr(yellow,value,3)

# battery
cm_bat = get_cmap('autumn')
def battery():
  acpi = (sp.check_output('acpi').decode('utf-8')).split()
  state = acpi[2][:-1]
  charge = acpi[3][:acpi[3].index('%')]

  if 'Charging' in state or 'Discharging' in state:
    time = acpi[4][:-3] + ('+' if 'Charging' in state else '-')
  elif 'Full' in state:
    time = ''
  else:
    time = '?????'

  charge_color = cm_bat(int(charge)/100.)
  time_color = textcolor
  return cstr([time_color, charge_color], [time, charge], [6,3])

# network
def network_up(interface):
  with open('/sys/class/net/'+interface+'/operstate','r') as f:
    state = f.read(1)
  return True if state == 'u' else False

def network():
  if network_up(args.eth):
    return cstr(textcolor,'ethernet',args.network_width)
  elif network_up(args.wl):
    f = open('/proc/net/wireless','r')
    for line in f:
      if args.wl in line:
        line = line.split()
        link_quality = int(float(line[2])*100/70)
        signal_strength = int(float(line[3]))
        network_name = (sp.check_output(['iwgetid','-r']).
                        decode('utf-8')).split()[0]
    f.close()
    return cstr(textcolor,str(link_quality)+'% '+str(signal_strength)+'dBm '
                +network_name,args.network_width)
  else:
    return cstr(textcolor,'',args.network_width)

# xmonad info
ws_color = {
  'c' : blue,
  'v' : yellow,
  'h' : green,
  'hnw' : white,
  'u' : red
}

def test(t):
    f = open('/home/perlinm/test','a')
    f.write(str(t)+'\n\n')
    f.close()

def xmonad(line):
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
  colors += [textcolor,None,yellow]
  strings += [layout,' '*args.systray_width,title]
  lengths += [len(layout),args.systray_width,args.client_width]
  test(strings)
  return cstr(colors,strings,lengths)

# function, value dictionaries
left = args.left.split()
center = args.center.split()
right = args.right.split()

all_funs = {
  'bat' : lambda: battery(),
  'clock' : lambda: clock(),
  'cpu' : lambda: cpu(),
  'light' : lambda: light(),
  'mem' : lambda: memory(),
  'network' : lambda: network(),
  'systray' : lambda: systray(),
  'temp' : lambda: core_temp(),
  'vol' : lambda: volume(),
  'weather' : lambda: weather(),
  'xmonad' : lambda line: xmonad(line),
}
arg_funs = ['xmonad']

all_timed_funs = [[1,['clock', 'cpu', 'light', 'mem', 'temp']],
                [5,['bat', 'network']], [600,['weather']]]
timed_funs = [[]]*len(all_timed_funs)
for i in range(len(timed_funs)):
  timed_funs[i] = [all_timed_funs[i][0],
                   [f for f in all_timed_funs[i][1] if f in left+center+right]]
funs = dict((f, all_funs[f]) for f in left+center+right)
vals = dict((f, funs[f]() if f not in arg_funs else ['',0]) for f in funs)

# bar info
def section_length(bar):
  if len(bar) == 0:
    return 0
  length = len(args.spacer)*(len(bar)-1)
  for i in range(len(bar)):
    length += vals[bar[i]][1]
  return length

def section_text(bar):
  text = ''
  for i in range(len(bar)):
    text += vals[bar[i]][0]
    if i+1 < len(bar):
      text += args.spacer
  return text

def bar_text(seconds):
  width = res()/args.char_width

  for i in range(len(timed_funs)):
    if seconds % timed_funs[i][0] == 0:
      for j in range(len(timed_funs[i][1])):
        vals[timed_funs[i][1][j]] = funs[timed_funs[i][1][j]]()

  l_pad = ' '*np.ceil(width/2 - section_length(left)
                      - section_length(center)/2)
  r_pad = ' '*np.floor(width/2 - section_length(right)
                       - section_length(center)/2)

  return (section_text(left) + l_pad + section_text(center)
          + r_pad + section_text(right))

# polling functions
seconds = 1
def second_poll():
  global seconds
  while True:
    print(bar_text(seconds))
    sys.stdout.flush()
    time.sleep(1)
    seconds += 1

def vol_poll():
  global vals
  while True:
    p = select.poll()
    mixer = alsaaudio.Mixer(cardindex = args.soundcard)
    fd, em = mixer.polldescriptors()[0]
    p.register(fd)
    if p.poll():
      vals['vol'] = funs['vol']()
      print(bar_text(seconds))
      sys.stdout.flush()
    p.unregister(fd)

def xmonad_poll(stdin):
  global vals
  for line in stdin:
    vals['xmonad'] = funs['xmonad'](line)
    print(bar_text(seconds))
    sys.stdout.flush()

second_thread = threading.Thread(target=second_poll)
second_thread.start()

if 'vol' in left+center+right:
  vol_thread = threading.Thread(target=vol_poll)
  vol_thread.start()

if 'xmonad' in left+center+right:
  xmonad_thread = threading.Thread(target=xmonad_poll(sys.stdin))
  xmonad_thread.start()

exit(1)




















# cpu info
cpucols = [(0.0, (0.2, 0.2, 0.2)),
           (0.1, (0.3, 0.3, 0.3)),
           (0.3, (.7, .8, .25)),
           (0.6, (.8, .6, 0)),
           (1.0, (1, 0, 0))]

cm_cpu = mp.colors.LinearSegmentedColormap.from_list(
  'cpu', cpucols, 100)

# get initial cpu values
f = open('/proc/stat', 'r')
stat = []
line = f.readline()
while 'cpu' in line:
  stat.append(line)
  line = f.readline()
f.close()
old_idles = np.array([int(s.split()[4]) for s in stat[1:]])
old_cpu_time = datetime.datetime.min
f = open('/proc/cpuinfo', 'r')
while True:
  line = f.readline()
  if 'cpu cores' in line:
    break
f.close()
ncores = int(line.split()[-1])
threads_per_core = int(len(old_idles)/ncores)

cpu_len = (threads_per_core*3 + 2)*ncores + 1
def cpu():
  global old_idles, old_cpu_time
  f = open('/proc/stat', 'r')
  stat = []
  line = f.readline()
  while 'cpu' in line:
    stat.append(line)
    line = f.readline()
  f.close()
  idles = np.array([int(s.split()[4]) for s in stat[1:]])
  cur_cpu_time = datetime.datetime.now()
  dt = (cur_cpu_time - old_cpu_time).total_seconds()
  vals = 100 - (idles - old_idles)/dt
  string = ['^fg(%s)|' %textcolor]
  for i in range(ncores):
    for j in range(threads_per_core):
      val = vals[i + ncores*j]
      color = mp.colors.rgb2hex(cm_cpu(val/100.))
      display = '^fg(%s)%2i' %(color,val) \
        if val < 100 else '^fg(%s)00' %color
      string.append(display)
    string.append('^fg(%s)|' %textcolor)

  old_idles = idles
  old_cpu_time = cur_cpu_time
  return ' '.join(string), cpu_len

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
  return '^fg(' + color + ')' + str(temp) + ' C', 4

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

# weather
def weather():
  weath = pywapi.get_weather_from_noaa('KCVO')
  w = '^fg(%s)' %textcolor + weath['temp_c'] + ' C' # + weath['weather']
  return w, 4
