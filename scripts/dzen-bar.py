#!/usr/bin/python3
import matplotlib as mp
import matplotlib.cm as cm
import numpy as np
import subprocess as sp
import time, sys, datetime, alsaaudio, argparse, select, pywapi, threading, re

# arguments
parser = argparse.ArgumentParser(
    description='Returns a string containing various system status ' \
    + 'for use in a dzen status bar.')

parser.add_argument(
    '-l', '--left', default='xmonad',
    help='Things to display aligned left')

parser.add_argument(
    '-c', '--center', default='cpu',
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
    '-clw', '--client_width', type=int, default=100,
    help='Client name width')

parser.add_argument(
    '-tw', '--tray_width', type=int, default=8,
    help='System tray width')

parser.add_argument(
    '-th', '--tray_height', type=int, default=16,
    help='System tray height (in px)')

parser.add_argument(
    '-bg', '--background', type=str, default='#111111',
    help='Background color for system tray')

parser.add_argument(
    '-ws', '--workspaces', type=int, default=11,
    help='Number of workspaces')

parser.add_argument(
    '--spacer', default='  ', help='Spacer separating things')

parser.add_argument(
    '-chw', '--char_width', type=int, default=7,
    help='Width (in px) of a character in your font. ' \
    + 'For Monospace-[8/9], it is 7.')

args = parser.parse_args()


# color definitions
fullBlack = [0, 0, 0] # "#000000"
fullWhite = [1, 1, 1] # "#ffffff"
black = [2/30., 2/30., .2/30.] # "#111111"
white = [.87, .87, .87] # "#dddddd"
grey = [.47, .47, .47] # "#777777"
red = [.76, .11, .09] # "#c11b17"
green = [.21, .49, .09] # "#347c17"
blue = [0, .41, .55] # "#00688b"
yellow = [1., .73, 0] # "#ffbb00"

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


# format strings with colors for dzen and return string length
tail = '..'
def one_cstr(align,color,string,length):
  if type(color) is list or type(color) is tuple:
    color = mp.colors.rgb2hex([color[0],color[1],color[2]])
  color_text = '^fg(%s)' %color if color != None else ''
  out_str = str(string).replace('\n','')
  if length >= 0:
    if len(out_str) > length:
      out_str = out_str[:length-len(tail)]+tail
    elif len(out_str) < length:
      spacer = ' '*(length-len(out_str))
      if align == 'l':
        out_str += spacer
      else:
        out_str = spacer + out_str
  return color_text + out_str, len(out_str)

def cstr(align,colors,strings,length=-1):
  if not type(strings) is list:
    out_str, out_length = one_cstr(align,colors,strings,length)
  else:
    if length == -1:
      length = [-1]*len(strings)
    out_str = ''
    out_length = 0
    for i in range(len(strings)):
      addition = one_cstr(align,colors[i],strings[i],length[i])
      out_str += addition[0]
      out_length += addition[1]
      if i+1 < len(strings):
        out_str += ' '
        out_length += 1
  return out_str, out_length


# date and time
def clock(align):
  now = datetime.datetime.now()
  time = now.strftime('%a %Y-%m-%d %H:%M:%S')
  return cstr(align,textcolor,time)


# volume
cm_light = get_cmap('cool')
p = re.compile('\w')
def volume(align):
  info = sp.check_output(['amixer','-M','get','Master'])
  vol = ''.join(p.findall(info.split()[-3].decode('utf-8')))
  state = ''.join(p.findall(info.split()[-1].decode('utf-8')))
  color = cm_light(1-int(vol)/100.) if state == 'on' else red
  return cstr(align,color,vol,3)

# screen brightness
def light(align):
  value = int(float(sp.check_output('xbacklight'))+0.5)
  return cstr(align,yellow,value,3)


# battery
cm_bat = get_cmap('autumn')
def battery(align):
  acpi = (sp.check_output('acpi').decode('utf-8')).split()
  state = acpi[2][:-1]
  charge = acpi[3][:acpi[3].index('%')]
  charge_color = cm_bat(int(charge)/100.)
  time_color = textcolor

  if 'Charging' in state or 'Discharging' in state:
    time = acpi[4][:-3]
    charge += '+' if 'Charging' in state else '-'
  elif 'Full' in state:
    time = ''
    charge += ' '
  else:
    time = '?????'
    charge += '?'

  return cstr(align,[time_color, charge_color], [time, charge], [6,4])


# network
def network_up(interface):
  with open('/sys/class/net/'+interface+'/operstate','r') as f:
    state = f.read(1)
  return True if state == 'u' else False

def network(align):
  if network_up(args.eth):
    return cstr(align,textcolor,'ethernet',args.network_width)
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
    return cstr(align,textcolor,str(link_quality)+'% '+str(signal_strength)+'dBm '
                +network_name,args.network_width)
  else:
    return cstr(align,textcolor,'',args.network_width)


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
  lengths += [layout_length,args.client_width]
  return cstr(align,colors,strings,lengths)


# system tray
def systray():
  pad = width() - max_size(right)
  sp.Popen(['killall','stalonetray'])
  sp.Popen(['stalonetray','--background',args.background,'--geometry',
            str(int(args.tray_width*args.char_width/args.tray_height))
            + 'x1+' + str(int((pad+0.5)*args.char_width)) + '+0',
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

old_idles = get_idles()
old_cpu_time = datetime.datetime.min

with open('/proc/cpuinfo', 'r') as f:
  for line in f:
    if 'cpu cores' in line:
      ncores = int(line.split()[-1])

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
    colors.append(cm_cpu(int(vals[i])/100.))
    vals[i] = int(vals[i]) if vals[i] < 100 else '00'
  lengths = [2]*len(vals)
  old_idles = idles
  old_cpu_time = cpu_time
  return cstr(align,colors,vals,lengths)


# function and value dictionaries
left = args.left.split()
center = args.center.split()
right = args.right.split()
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

all_timed_funs = [[1,['clock', 'cpu', 'light', 'mem', 'temp']],
                [5,['bat', 'network']], [600,['weather']]]
timed_funs = [[]]*len(all_timed_funs)
for i in range(len(timed_funs)):
  timed_funs[i] = [all_timed_funs[i][0],
                   [f for f in all_timed_funs[i][1] if f in used_funs]]
funs = dict((f, all_funs[f]) for f in used_funs)
aligns = dict((f, 'l' if f in left else 'r') for f in used_funs)
vals = dict((f, funs[f]() if f not in arg_funs else ['',0]) for f in funs)


# bar width in characters
def width():
  return int(int(sp.check_output('xrandr').split()[7])/args.char_width)

# maximum size of left/right bars
def max_size(bar):
  size = (width()-section_length(center)[0])/2
  return int(np.floor(size)) if bar == left else int(np.ceil(size))

# length of sections in characters
def section_length(bar):
  if len(bar) == 0:
    return 0
  length = len(args.spacer)*(len(bar)-1)
  for i in range(len(bar)):
    length += vals[bar[i]][1]

  truncation = 0
  # account for spacer to center section
  if bar == center:
    length += 2*len(args.spacer)
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
      text += args.spacer

  truncation = section_length(bar)[1]
  # add spacers to center section
  if bar == center:
    text = args.spacer + text + args.spacer
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
  global old_width, l_pad, r_pad

  # if width has changed, adjust padding between sections
  if width() != old_width:
    old_width = width()
    l_pad = max_size(left) - section_length(left)[0]
    r_pad = max_size(right) - section_length(right)[0]
    if 'xmonad' in used_funs:
      systray()

  return (section_text(left) + ' '*l_pad + section_text(center)
            + ' '*r_pad + section_text(right))


# polling functions
seconds = 1
def second_poll():
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
    time.sleep(1. - elapsed)

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

if 'vol' in used_funs:
  vol_thread = threading.Thread(target=vol_poll)
  vol_thread.start()

if 'xmonad' in used_funs:
  xmonad_thread = threading.Thread(target=xmonad_poll(sys.stdin))
  xmonad_thread.start()





# weather
def weather(align):
  weath = pywapi.get_weather_from_noaa('KCVO')
  temp = str(int(float(weath['temp_c']))) + ' C'
  return cstr(align, textcolor, temp, 5)


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
  return cstr(color,temp+' C',4)


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
