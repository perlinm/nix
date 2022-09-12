#!/usr/bin/zsh

if ! [ $# -eq 1 ]; then
  echo "must provide exactly one argument"
  exit
fi

current_workspace=$(i3-msg -t get_workspaces \
                    | jq '.[] | select(.focused==true).name' \
                    | cut -d "\"" -f2)
i3-msg "rename workspace to TMP; \
        rename workspace $1 to $current_workspace; \
        rename workspace TMP to $1; \
        workspace $1"
