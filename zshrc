# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# ZSH_THEME moved to ~/.zshrc_local
if [[ -r ~/.zshrc_local ]]; then
  source ~/.zshrc_local &>/dev/null
fi

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Options for `less`
less_options=("i" "R" "X" "F")
for flag in "${less_options[@]}"; do
  if ! grep -q "${flag}" <<<"${LESS}"; then
      LESS="${LESS} -${flag}"
  fi
done
export LESS

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  git
  my-emacs
  my-git
)

source $ZSH/oh-my-zsh.sh

# User configuration

# PATH changes
# export MANPATH="/usr/local/man:$MANPATH"
PATH=$PATH:$HOME/.local/bin

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Set personal aliases
if [ -f ~/.zsh_aliases ]; then
    source ~/.zsh_aliases
fi

# These lines set up the SSH agent, which is necessary to get into
# your SSH connections.
SSH_ENV="$HOME/.ssh/environment"

function start_agent {
    echo "Initialising new SSH agent..."
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add;
    if [[ -f ~/.ssh/brian_home ]]; then
      ssh-add ~/.ssh/brian_home
    fi
}

if [[ ! $TERM == "dumb" ]]; then
  if [ -f "${SSH_ENV}" ]
  then
    . "${SSH_ENV}" > /dev/null
    ps h ${SSH_AGENT_PID} &>/dev/null || {
      start_agent;
    }
  else
    start_agent;
  fi
fi

# Some setting
export IGNOREEOF=42

# Username on VM at work is "ir"
if [[ $(whoami) = "ir" ]]; then
    source ~/.zshrc_pure
fi

# Inside emacs, take autocorrection out
# (otherwise, this can trip tramp)
if [[ $TERM == "dumb" ]]; then
  unset RPROMPT
  unset RPS1
  PS1="$ "
  unsetopt correct_all
  unsetopt rcs
fi

# Use ripgrep.conf
export RIPGREP_CONFIG_PATH=${HOME}/.config/ripgrep.conf

# Local variables:
# mode: shell-script
# End:
