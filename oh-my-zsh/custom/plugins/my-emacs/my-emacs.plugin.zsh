# Emacs 23 daemon capability is a killing feature.
# One emacs process handles all your frames whether
# you use a frame opened in a terminal via a ssh connection or X frames
# opened on the same host.

# Benefits are multiple
# - You don't have the cost of starting Emacs all the time anymore
# - Opening a file is as fast as Emacs does not have anything else to do.
# - You can share opened buffered across opened frames.
# - Configuration changes made at runtime are applied to all frames.

if "$ZSH/tools/require_tool.sh" emacsclient 24 2>/dev/null ; then


    # set EDITOR if not already defined.
    export EDITOR='emacsclient -nw --alternate-editor="emacs -nw" --no-wait'

    function ediff {
      if [[ "$#" != 2 ]]; then
        echo "ediff needs exactly two files."
        return 1
      fi
      local file1="$1"
      local file2="$2"

      for file in "$file1" "$file2"; do
        if [[ ! -r "${file}" ]]; then
          echo "Cannot read ${file}."
          return 1
        fi
      done

      cmd="(ediff \"${file1}\" \"${file2}\")"
      emacsclient -nw --alternate-editor="emacs -nw" --eval "$cmd"
    }
fi

## Local Variables:
## mode: sh
## End:
