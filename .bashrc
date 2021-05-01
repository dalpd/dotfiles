export PATH=$PATH:/home/dad/.local/bin

# make bash history be forever
export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT="[%F %T] "
export HISTCONTROL=ignoredups
# make sure misconfigured bash instances don't bork the history
export HISTFILE=~/.bash_eternal_history
# force the history to save after each command
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
