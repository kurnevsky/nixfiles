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
  zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
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

  # Autocompletion for kubernetes
  command -v kubectl > /dev/null && source <(kubectl completion zsh)

  # Many programs change the terminal state, and often do not restore terminal settings on exiting abnormally
  # This avoids the need to manually reset the terminal
  ttyctl -f
}
