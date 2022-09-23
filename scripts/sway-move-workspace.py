#!/usr/bin/env python3

import json
import os
import subprocess
import sys

move_script = os.path.join(os.path.dirname(__file__), "sway-swap-workspace-by-name.sh")
assert len(sys.argv) == 2

direction = sys.argv[1]
assert direction in ["prev", "next"]

workspaces = json.loads(subprocess.check_output(["swaymsg", "-t", "get_workspaces"]))
current_workspace = next(datum for datum in workspaces if datum["focused"])

diff = 1 if direction == "next" else -1
new_workspace_index = workspaces.index(current_workspace)
while True:
    new_workspace_index += diff
    new_workspace = workspaces[new_workspace_index % len(workspaces)]
    if new_workspace["output"] == current_workspace["output"]:
        subprocess.run([move_script, new_workspace["name"]])
        exit()
