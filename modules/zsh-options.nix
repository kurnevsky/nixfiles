[
  # Remove all duplicates of current command from history, add current to end
  "hist_ignore_all_dups"
  # Don't save any commands beginning with space
  "hist_ignore_space"
  # Enable extended globs to interpret things like rm ^(file|file2)
  "extended_glob"
  # Don't beep even if zsh don't like something
  "no_beep"
  # Change directory even if user forgot to put 'cd' command in front, but entered path is valid
  "auto_cd"
  # If possible, correct commands
  "correct"
  # Append their history list to the history file, rather than replace it
  "inc_append_history"
  # If a pattern for filename generation has no matches, print an error, instead of leaving it unchanged in the argument list
  "nomatch"
  # Report the status of background jobs immediately, rather than waiting until just before printing a prompt
  "notify"
  # Allow parameter expansion, command substitution and arithmetic expansion for prompt string
  "prompt_subst"
  # Remove any right prompt from display when accepting a command line
  "transient_rprompt"
  # File completion after =
  "magic_equal_subst"
  # Apply globbing to hidden files
  "glob_dots"
  # Use OS file locking
  "hist_fcntl_lock"
]
