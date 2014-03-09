#!/usr/bin/python3
import matplotlib # used for colormaps to make font colors fade nicely
import numpy # used for arrays
import time, subprocess, sys, datetime, calendar, alsaaudio, select, argparse, threading

# each function should return a string formatted to be constant length with dzen
# color codes and the length of the string as displayed by dzen


# multipurpose things -----------------------------------------------------
red = '#ff0000'
green = '#00cc00'
blue = '#ff00ff'
black = '#000000'
textcolor = '#888888'
col = '#aabbcc'

def test_cmap(cmap):
  nums = ''
  for i in range(0, 101, 2):
    color = matplotlib.colors.rgb2hex(cmap(i/100.))
    nums += '^fg(' + color + ')' + str(i) + ' '
    sys.stdout.flush()
  print (nums)

# battery -----------------------------------------------------------------
dict_bat = {'red':   [(0.0, 1.0, 1.0),
                      (0.3, 1.0, 1.0),
                      (1.0, 0.0, 0.0)],

            'green': [(0.0, 0.0, 0.0),
                      (0.15, 0.0, 0.0),
                      (0.3, 1.0, 1.0),
                      (1.0, 1.0, 1.0)],

            'blue':  [(0.0, 0.0, 0.0),
                      (1.0, 0.0, 0.0)]}
cm_bat = matplotlib.colors.LinearSegmentedColormap('bat', dict_bat, 100)

def battery():
  batdir = '/sys/class/power_supply/BAT'+str(args.bat)+'/'
  charge = int(numpy.loadtxt(batdir + 'capacity'))
  f = open(batdir + 'status')
  state = f.readline()
  f.close()
  bg = black
  if 'Discharging' in state:
    status = '-'
    statuscolor = red
  elif 'Charging' in state:
    status = '+'
    statuscolor = green
  elif 'Full' in state:
    status = ''
    statuscolor = black
  else:
    status = '*'
    statuscolor = black
    bg = red
  color = matplotlib.colors.rgb2hex(cm_bat(charge/100.))
  return '^bg(' + bg + ')^fg(' + color + ')' + str(charge) + '^fg(' + statuscolor + ') ' + status + '^fg()', 4

# cpu ---------------------------------------------------------------------
cpucols = [(0.0, (0.2, 0.2, 0.2)),
           (0.1, (0.3, 0.3, 0.3)),
           (0.3, (.7, .8, .25)),
           (0.6, (.8, .6, 0)),
           (1.0, (1, 0, 0))]

cm_cpu = matplotlib.colors.LinearSegmentedColormap.from_list('cpu', cpucols, 100)

# get initial cpu values
f = open('/proc/stat', 'r')
stat = []
line = f.readline()
while 'cpu' in line:
  stat.append(line)
  line = f.readline()
f.close()
old_idles = numpy.array([int(s.split()[4]) for s in stat[1:]])
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
  idles = numpy.array([int(s.split()[4]) for s in stat[1:]])
  cur_cpu_time = datetime.datetime.now()
  dt = (cur_cpu_time - old_cpu_time).total_seconds()
  vals = 100 - (idles - old_idles)/dt
  string = ['^fg(%s)|' %textcolor]
  for i in range(ncores):
    for j in range(threads_per_core):
      val = vals[i + ncores*j]
      color = matplotlib.colors.rgb2hex(cm_cpu(val/100.))
      display = '^fg(%s)%2i' %(color,val) if val < 100 else '^fg(%s)00' %color
      string.append(display)
    string.append('^fg(%s)|' %textcolor)

  old_idles = idles
  old_cpu_time = cur_cpu_time
  return ' '.join(string), cpu_len


# date/time ---------------------------------------------------------------
dateC = '#3cb371'
timeC = '#40e0d0'

weekC = timeC
wendC = '#aaaaaa'
todayC = '#DC143C'

def cal(year, mon, today):
  cal = calendar.Calendar()
  cal.setfirstweekday(6)
  mycal = '\n^cs()\n'
  mycal += '  ^fg(%s)Sun ^fg(%s)Mon Tue Wed Thu Fri ^fg(%s)Sat\n' %(wendC, weekC, wendC)
  mycal += '  ---------------------------\n'
  days = cal.itermonthdays2(year, mon)
  week = '^fg(%s)  ' %wendC
  for day in days:
    if day[0] != 0:
      if day[0] == 1 and day[1] != 6:
        week += '    '*(day[1]+1)
        week += '^fg(%s)' %weekC
      if day[0] == today:
        week += '^fg(%s)' %todayC
      elif day[1] >= 5:
        week += '^fg(%s)' %wendC
      else:# day[1] == 0:
        week += '^fg(%s)' %weekC
      week += '%3i ' %day[0]
    if day[1] == 5:
      mycal += week + '\n'
      week = '^fg(%s)  ' %wendC
  return mycal

oldday = 0
def clock():
  global oldday
  now = datetime.datetime.now()
  date = now.strftime('%a %m-%d-%Y')
  day = now.day
  time = now.strftime('%I:%M:%S')
  clock = '^tw()^fg(' + dateC + ')' + date + '   ' + '^fg(' + timeC + ')' + time

  if day != oldday:
    oldday = day
    #clock += cal(now.year, now.month, day)
  return clock, 25

# core_temp ---------------------------------------------------------------
tempcols = [(0.0, (0, 1, 1)),
            (0.4, (0, 1, 1)),
            (0.8, (1, 1, 0)),
            (1.0, (1, 0, 0))]
cm_temp = matplotlib.colors.LinearSegmentedColormap.from_list('temp', tempcols, 100)

def core_temp():
  info = subprocess.check_output(['acpi','-t']).split()
  temp = int(float(info[3]))
  color = matplotlib.colors.rgb2hex(cm_temp(temp/100.))
  return '^fg(' + color + ')' + str(temp) + ' C', 4

# memory ------------------------------------------------------------------
memcols = [(0.0, (0, 1, 1)),
            (0.4, (0, 1, 1)),
            (0.8, (1, 1, 0)),
            (1.0, (1, 0, 0))]
cm_mem = matplotlib.colors.LinearSegmentedColormap.from_list('mem', memcols, 100)

mem_total = int(subprocess.check_output('free').split()[7])*2.**-20
mem_len = 5 if mem_total < 10 else 6

def memory():
  mem_free = int(subprocess.check_output('free').split()[16])*2.**-20
  used_frac = (mem_total - mem_free)/mem_total
  color = matplotlib.colors.rgb2hex(cm_mem(used_frac))
  if mem_free < 1:
    post = ' M'
    mem = '%3i' %(mem_free*1024)
  else:
    post = ' G'
    mem = '%.1f' %(mem_free)
  return '^fg(' + color + ')' + mem + post, mem_len

# weather -----------------------------------------------------------------

def weather():
  return 'blah', 4
  weath = pywapi.get_weather_from_yahoo('97330')['condition'] # fixme: location, text?, color
  w = '^fg(%s)' %col + weath['temp'] + ' ^fg()c' # + weath['text']
  return w, 4

# volume ------------------------------------------------------------------

dict_vol = {'red':   [(0.0, 1.0, 1.0),
                      (1.0, 0.0, 0.0)],

            'green': [(0.0, 0.0, 0.0),
                      (1.0, 1.0, 1.0)],

            'blue':  [(0.0, 1.0, 1.0),
                      (1.0, 1.0, 1.0)]}
cm_vol = matplotlib.colors.LinearSegmentedColormap('vol', dict_vol, 100)

muteC = '#ff00ff'

def volume(soundcard):
  # mixer = alsaaudio.Mixer(cardindex = soundcard)
  # p = select.poll()
  # fd, em = mixer.polldescriptors()[0]
  # p.register(fd)

  # while True:
  #   p.poll()
  #   mixer = alsaaudio.Mixer(cardindex = soundcard)
  #   p = select.poll()
  #   fd, em = mixer.polldescriptors()[0]
  #   p.register(fd)
  #   print(mixer.getvolume()[0])
  mute = True if mixer.getmute()[0] == 1 else False
  if mute:
    vol = 'MM'
    color = muteC
  else:
    vol = '%2s' % mixer.getvolume()[0]
    color = matplotlib.colors.rgb2hex(cm_vol(int(vol)/100.))
#  return '^ca(1,xdotool key XF86AudioLowerVolume)  ^ca()^ca(1,xdotool key XF86AudioMute)^fg('+color+')' + vol + '^ca()^ca(1,xdotool key XF86AudioRaiseVolume)  ^ca()', 6
  return 'vol', 3

# -------------------------------------------------------------------------

if 'test' in sys.argv:
  test_cmap(cm_temp)
  exit(0)

# -------------------------------------------------------------------------
def res():
  return int(subprocess.check_output('xrandr').split()[7])


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


def main():
  # arguments
  parser = argparse.ArgumentParser(
    description='Returns a string containing various system status for use in a dzen status bar with xmonad.')
  parser.add_argument(
    '-l', '--left', metavar='funs', default='vol',
    help='Things to display aligned left')

  parser.add_argument(
    '-c', '--center', metavar='funs', default='temp cpu mem',
    help='Things to display aligned in the center')

  parser.add_argument(
    '-r', '--right', metavar='funs', default='vol bat weather clock',
    help='Things to display aligned right')

  parser.add_argument(
    '-s', '--soundcard', type=int, metavar='sc', default=1,
    help='number of soundcard to use for volume')

  parser.add_argument(
    '-b', '--bat', type=int, metavar='bat', default=0,
    help='number of battery to use for power info')

  parser.add_argument(
    '--char_width', type=int, metavar='w', default=7,
    help='width in pixels of a character in your font. For Monospace9, it is 7.')

  parser.add_argument(
    '--lspace', type=int, metavar='w', default=21,
    help='Space, in pixels, to leave on the left side of the screen.')

  parser.add_argument(
    '--rspace', type=int, metavar='w', default=112,
    help='Space, in pixels, to leave on the right side of the screen.')

  args = parser.parse_args()

  left = args.leftf.split()
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

  # main loop ---------------------------------------------------------------
  # vol thread
  # if 'vol' in left+center+right:
  #   t = threading.Thread(target=vol, args=(args.soundcard))
  #   t.daemon = True
  #   t.start()

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
    print(update_bar(vals, args.lspace, args.rspace, args.char_width, width))
    sys.stdout.flush()
    time.sleep(dt)

    # fixme:
    # battery number
    # get rid of boobs


if __name__ == "__main__":
  main()
