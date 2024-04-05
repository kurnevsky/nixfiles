[[ $TERM != 'dumb' ]] && {
  # Autocompletion with an arrow-key driven interface
  zstyle ':completion:*' menu select
  # Rehash automatically
  zstyle ':completion:*:commands' rehash true
  # Verbose completion results
  zstyle ':completion:*' verbose true
  # Enable corrections
  zstyle ':completion:*' completer _complete _correct
  # Case-insensitive completion, completion of dashed values
  zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[_-]=* r:|=*'
  # Don't insert a literal tab when trying to complete in an empty buffer
  zstyle ':completion:*' insert-tab false
  # Group results by category
  zstyle ':completion:*:matches' group true
  zstyle ':completion:*' group-name ''
  # Keep directories and files separated
  zstyle ':completion:*' list-dirs-first true
  # Use ls-colors for path completions
  zstyle ':completion:*:default' list-colors "${(s.:.)LS_COLORS}"
  # Advanced process completion
  zstyle ':completion:*:*:*:*:processes' command 'ps -A -o pid,user,%cpu,cmd'
  zstyle ':completion:*:*:*:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;32'
  # List all processes for killall
  zstyle ':completion:*:processes-names' command "ps -eo cmd= | sed 's:\([^ ]*\).*:\1:;s:\(/[^ ]*/\)::;/^\[/d'"
  # Display message when no matches are found
  zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
  # Ignore internal zsh functions
  zstyle ':completion:*:functions' ignored-patterns '_*'

  # Sets autocompletion
  autoload -Uz compinit && mkdir -p ~/.cache/zsh && compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
  # Enable colors in prompt
  autoload -Uz colors && colors
  # Massive rename
  autoload -Uz zmv
  # Calculator
  autoload -Uz zcalc
  # Search history by entered text
  autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
  zle -N up-line-or-beginning-search
  zle -N down-line-or-beginning-search
  # Edit current command in the editor (ctrl-alt-e)
  autoload -Uz edit-command-line
  zle -N edit-command-line
  bindkey '^[^E' edit-command-line

  # Many programs change the terminal state, and often do not restore terminal settings on exiting abnormally
  # This avoids the need to manually reset the terminal
  ttyctl -f

  # Find the key with: showkey -a
  bindkey "^[[1;5C" forward-word # Ctrl-Right
  bindkey "^[[1;5D" backward-word # Ctrl-Left
  bindkey '^ ' autosuggest-accept # Ctrl+Space

  # Terminal can send different control sequences depending on whether it has been put in keypad transmit mode or not.
  # The smkx and rmkx terminfo entries can be used to put a terminal in or out of that mode.
  [[ -n "${terminfo[kcuu1]}" ]] && bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search # Up
  [[ -n "${terminfo[kcuu1]/O/[}" ]] && bindkey "${terminfo[kcuu1]/O/[}" up-line-or-beginning-search # Up
  [[ -n "${terminfo[kcud1]}" ]] && bindkey "${terminfo[kcud1]}" down-line-or-beginning-search # Down
  [[ -n "${terminfo[kcud1]/O/[}" ]] && bindkey "${terminfo[kcud1]/O/[}" down-line-or-beginning-search # Down

  if command -v sk > /dev/null
  then
    skim-history-widget() {
      local num
      setopt localoptions pipefail
      echo -ne "\r"
      num=$(fc -rl 1 | sk --height 50% -n2..,.. --tiebreak=score,index --layout=reverse --inline-info -p "➜ " --query="$LBUFFER")
      local ret=$?
      zle reset-prompt
      if [ -n "$num" ]; then
        zle vi-fetch-history -n "$num"
      fi
      return $ret
    }
    zle -N skim-history-widget
    bindkey '^R' skim-history-widget
  fi

  # Set cursor type to steady bar
  echo -e -n "\x1b[\x36 q"
}
