#!/usr/bin/python3
import matplotlib as mp
import numpy as np
import subprocess as sp
import time, sys, datetime, calendar, alsaaudio, select, argparse, \
  threading, glob, pywapi

# arguments
parser = argparse.ArgumentParser(
    description='Returns a string containing various system status ' \
    + 'for use in a dzen status bar.')

parser.add_argument(
    '-l', '--left', metavar='funs', default='vol',
    help='Things to display aligned left')

parser.add_argument(
    '-c', '--center', metavar='funs', default='temp cpu mem',
    help='Things to display aligned in the center')

parser.add_argument(
    '-r', '--right', metavar='funs', default='weather bat vol clock',
    help='Things to display aligned right')

parser.add_argument(
    '-s', '--soundcard', type=int, metavar='sc', default=0,
    help='number of soundcard to use for volume')

parser.add_argument(
    '-b', '--bat', type=int, metavar='bat', default=-1,
    help='battery number to use for power info')

parser.add_argument(
    '--char_width', type=int, metavar='w', default=7,
    help='width in pixels of a character in your font. ' \
    + 'For Monospace9, it is 7.')

parser.add_argument(
    '--lspace', type=int, metavar='w', default=21,
    help='Space, in pixels, to leave on the left side of the screen.')

parser.add_argument(
    '--rspace', type=int, metavar='w', default=0,
    help='Space, in pixels, to leave on the right side of the screen.')

args = parser.parse_args()

# color definitions
fullBlack = "#000000"
fullWhite = "#FFFFFF"
black = "#1A1A1A"
white = "#DDDDDD"
grey = "#777777"
red = "#C11B17"
green = "#347C17"
blue = "#00688B"
yellow = "#FFBB00"

textcolor = white

# screen resolution
def res():
  return int(sp.check_output('xrandr').split()[7])

# battery
dict_bat = {'red':   [(0.0, 1.0, 1.0),
                      (0.3, 1.0, 1.0),
                      (1.0, 0.0, 0.0)],

            'green': [(0.0, 0.0, 0.0),
                      (0.15, 0.0, 0.0),
                      (0.3, 1.0, 1.0),
                      (1.0, 1.0, 1.0)],

            'blue':  [(0.0, 0.0, 0.0),
                      (1.0, 0.0, 0.0)]}

cm_bat = mp.colors.LinearSegmentedColormap('bat', dict_bat, 100)

def battery():
  battdir = '/sys/class/power_supply/'
  if args.bat == -1:
    battdir = glob.glob(battdir + 'BAT?')[0]
  else:
    battdir += 'BAT' + str(args.bat)
  charge = int(np.loadtxt(battdir + '/capacity'))
  state = np.genfromtxt(battdir + '/status', dtype='str')
  bg = black
  if 'Discharging' in state:
    status = '-'
    statuscolor = red
  elif 'Charging' in state:
    status = '+'
  elif 'Full' in state:
    status = ''
  else:
    status = '*'
  color = mp.colors.rgb2hex(cm_bat(charge/100.))
  return '^fg(%s)' %color + str(charge) \
    + '^fg(%s)' %textcolor + status + '^fg()', \
    len(str(charge)) + len(status)

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


# date and time
def clock():
  now = datetime.datetime.now()
  date = now.strftime('%a %Y-%m-%d')
  day = now.day
  time = now.strftime('%H:%M:%S')
  clock = '^fg(' + textcolor + ')' + date + ' ' + time
  return clock, len(date)+len(time)+1

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

# volume
def volume(soundcard):
  mixer = alsaaudio.Mixer(cardindex = soundcard)
  p = select.poll()
  fd, em = mixer.polldescriptors()[0]
  p.register(fd)

  # while True:
  #   p.poll()
  #   mixer = alsaaudio.Mixer(cardindex = soundcard)
  #   p = select.poll()
  #   fd, em = mixer.polldescriptors()[0]
  #   p.register(fd)
  #   print(mixer.getvolume()[0])
  color = red if mixer.getmute()[0] == 1 else blue
  vol = '%2s' % mixer.getvolume()[0]
  return '^fg('+color+')' + vol, 2

def update_bar(v, lspace, rspace, char_width, resolution):
  width = (resolution - lspace - rspace)/char_width
  left = [(' ', 1)]
  mid = [v['temp'], v['cpu'], v['mem']]
  right = [v['vol'], v['bat'], v['weather'], v['clock']]

  leftlen = sum([b[1] for b in left])
  midlen =  sum([b[1] + 1 for b in mid]) - 1
  rightlen = sum([b[1] for b in right])

  side_room = int((width - midlen)/2)
  lgap = int((side_room - leftlen)/len(left))
  rgap = int((side_room - rightlen)/len(right))

  bar = ''
  for fun in left:
    bar += fun[0] + ' '*lgap
  if len(bar) < side_room:
    bar += ' '*(side_room - len(bar))
  for fun in mid[:-1]:
    bar += fun[0] + ' '
  bar += mid[-1][0]

  for fun in right:
    bar += ' '*rgap + fun[0]
  return bar

left = args.left.split()
center = args.center.split()
right = args.right.split()

all_funs = {
  'xmonad' : lambda: xmonad(),
  'cpu' : lambda: cpu(),
  'temp' : lambda: core_temp(),
  'mem' : lambda: memory(),
  'vol' : lambda: volume(args.soundcard),
  'bat' : lambda: battery(),
  'clock' : lambda: clock(),
  'weather' : lambda: weather(),
}
all_timed_funs = ['cpu', 'temp', 'mem', 'bat', 'clock', 'weather']
timed_funs = [f for f in all_timed_funs if f in left+center+right]
funs = dict((f, all_funs[f]) for f in left+center+right)
vals = dict((f, funs[f]()) for f in funs)

dt = 1 # second

i = 0
while True:
  for f in timed_funs:
    if f == 'weather':
      # update weather only every 10 minutes
      if i % 600 == 0:
        vals[f] = funs[f]()
      else:
        i += 1
    else:
      vals[f] = funs[f]()

  width = res()
  print(update_bar(vals, args.lspace, args.rspace,
                 args.char_width, width))
  sys.stdout.flush()
  time.sleep(dt)
