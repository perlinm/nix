{
  programs.helix = {
    enable = true;
    settings = {
      theme = "onedark_perlinm";

      editor = {
        auto-format = false;
        auto-pairs = false;
        bufferline = "multiple";
        color-modes = true;
        completion-trigger-len = 3;
        cursor-shape = { insert = "bar"; };
        file-picker = { hidden = false; };
        gutters = [ "diagnostics" "line-numbers" "diff" ];
        idle-timeout = 0;
        line-number = "absolute";
        lsp = { auto-signature-help = false; };
        middle-click-paste = false;
        mouse = false;
        soft-wrap = {
          enable = true;
          max-indent-retain = 100;
          wrap-indicator = "";
        };
      };

      keys = {
        insert = {
          A-e = "move_visual_line_down";
          A-i = "move_char_right";
          A-k = "align_view_center";
          A-l = "goto_first_nonwhitespace";
          A-n = "move_char_left";
          A-q = "normal_mode";
          A-u = "move_visual_line_up";
          A-y = "goto_line_end_newline";
          C-a = "goto_line_start";
          C-c = "yank_main_selection_to_clipboard";
          C-k = "kill_to_line_end";
          C-r = [ "redo" "normal_mode" ];
          C-s = ":write";
          C-v = "paste_clipboard_before";
          C-x =
            [ "yank_main_selection_to_clipboard" "delete_selection_noyank" ];
          C-z = [ "undo" "normal_mode" ];
        };

        normal = {
          ";" = "half_page_up";
          "A-/" = "search_selection";
          A-c = "no_op";
          A-e = "goto_next_paragraph";
          A-f = ":reload";
          A-h = "copy_selection_on_next_line";
          A-i = "move_next_word_end";
          A-j = "copy_selection_on_prev_line";
          A-l = "goto_line_start";
          A-n = "move_prev_word_start";
          A-q = [ "collapse_selection" "keep_primary_selection" ];
          A-r = "toggle_comments";
          A-ret = "append_mode";
          A-u = "goto_prev_paragraph";
          A-y = "goto_line_end_newline";
          B = "shrink_selection";
          C-a = "goto_line_start";
          C-c = "yank_main_selection_to_clipboard";
          C-k = "kill_to_line_end";
          C-r = "redo";
          C-s = ":write";
          C-u = "jumplist_picker";
          C-v = "paste_clipboard_before";
          C-w = ":buffer-close";
          C-x =
            [ "yank_main_selection_to_clipboard" "delete_selection_noyank" ];
          C-z = "undo";
          D = "extend_line_above";
          F = ":format";
          L = "jump_backward";
          P = "open_below";
          S = "select_regex";
          S-tab = ":buffer-previous";
          T = "append_mode";
          V = "paste_after";
          Y = "jump_forward";
          Z = "redo";
          a = "no_op";
          b = "expand_selection";
          backspace = "delete_selection_noyank";
          c = "yank";
          d = "extend_line_below";
          del = "delete_selection_noyank";
          e = "move_visual_line_down";
          f = "search_next";
          h = "hover";
          i = "move_char_right";
          j = "no_op";
          k = "align_view_center";
          l = "goto_first_nonwhitespace";
          n = "move_char_left";
          o = "half_page_down";
          p = "open_above";
          r = "redo";
          ret = "insert_mode";
          s = "select_mode";
          space = {
            F = "file_picker";
            c = "file_picker_in_current_buffer_directory";
            f = "file_picker_in_current_directory";
          };
          t = "insert_mode";
          tab = ":buffer-next";
          u = "move_visual_line_up";
          v = "paste_before";
          w = "search_prev";
          x = "delete_selection";
          y = "goto_line_end";
          z = "undo";
        };

        select = {
          ";" = "half_page_up";
          "A-/" = "search_selection";
          A-c = "no_op";
          A-e = "goto_next_paragraph";
          A-f = ":reload";
          A-h = "copy_selection_on_next_line";
          A-i = "extend_next_word_end";
          A-j = "copy_selection_on_prev_line";
          A-l = "goto_line_start";
          A-n = "extend_prev_word_start";
          A-q = "normal_mode";
          A-r = "toggle_comments";
          A-ret = "append_mode";
          A-u = "goto_prev_paragraph";
          A-y = "goto_line_end_newline";
          B = "shrink_selection";
          C-a = "goto_line_start";
          C-c = "yank_main_selection_to_clipboard";
          C-k = "kill_to_line_end";
          C-r = "redo";
          C-s = ":write";
          C-u = "jumplist_picker";
          C-v = "paste_clipboard_before";
          C-w = ":buffer-close";
          C-x =
            [ "yank_main_selection_to_clipboard" "delete_selection_noyank" ];
          C-z = "undo";
          D = "extend_line_above";
          F = ":format";
          L = "jump_backward";
          P = "open_below";
          S = "select_regex";
          S-tab = ":buffer-previous";
          T = "append_mode";
          V = "paste_after";
          Y = "jump_forward";
          Z = "redo";
          a = "no_op";
          b = "expand_selection";
          backspace = "delete_selection_noyank";
          c = "yank";
          d = "extend_line_below";
          del = "delete_selection_noyank";
          e = "extend_visual_line_down";
          f = "search_next";
          h = "hover";
          i = "extend_char_right";
          j = "no_op";
          k = "align_view_center";
          l = "goto_first_nonwhitespace";
          n = "extend_char_left";
          o = "half_page_down";
          p = "open_above";
          r = "redo";
          ret = "insert_mode";
          s = "collapse_selection";
          t = "insert_mode";
          tab = ":buffer-next";
          u = "extend_visual_line_up";
          v = "paste_before";
          w = "search_prev";
          x = "delete_selection";
          y = "goto_line_end";
          z = "undo";
        };
      };
    };

    languages = {
      language = [
        {
          name = "python";
          formatter = {
            args = [ "-" "--quiet" ];
            command = "black";
          };
          config = {
            pylsp = {
              configurationSources = [ "flake8" ];
              plugins = {
                flake8 = { enabled = true; };
                mccabe = { enabled = false; };
                pycodestyle = { enabled = false; };
                pyflakes = { enabled = false; };
              };
            };
          };
        }

        {
          name = "nix";
          formatter = { command = "nixfmt"; };
        }
      ];
    };
  };
}
