# De-alias things we want to overwrite.
which gbg >/dev/null && unalias gbg
which gcm >/dev/null && unalias gcm
which gg >/dev/null && unalias gg
which gl >/dev/null && unalias gl
which gsh >/dev/null && unalias gsh
which gst >/dev/null && unalias gst
which gsta >/dev/null && unalias gsta

# Aliases.
alias gcm="git commit -m"
alias gcom='git checkout master'
alias ggrep="git grep"
alias ghash='git log -n 1 --pretty="%H"'
alias gl="git log --decorate"
alias gloh='git log -n10 --oneline --decorate'
alias gmb="git merge-base"
alias gpl="git pull"
alias gplrb="git pull --rebase"
alias gs="git status"
alias gsh="git show --decorate --no-patch"
alias gst="git stash"

# Functions
function gbdrm {
  local branch=$1
  local remote_branch=$(git branch -r | grep $branch | tr -d "\*[:blank:]")
  local remote
  if [[ -n $remote_branch ]]; then
      remote=$(cut -d "/" -f 1 <<< $remote_branch)
      git push --delete $remote $branch
  fi
  git branch -d $branch
}

which grbo >/dev/null && unalias grbo

function grbo {
  local branch=$1
  local current_branch=$(git rev-parse --abbrev-ref HEAD)

  git rebase --onto ${branch} $(git merge-base ${branch} HEAD)
}

function gbg {
  git branch | grep "$@" | tr -d "\*[:blank:]"
}

function gcog {
  branch=$(gbg "$1")
  branch=$(echo $branch | sed 's/\(^\*\?\s*\)\|\(\s*$\)//')
  branch_lines=$(echo $branch | wc -l)
  if [[ "$branch_lines" -eq 0 ]]; then
      echo "No branch exists containing $1"
  elif [[ "$branch_lines" -gt 1 ]]; then
      i=0
      echo $branch | while read line; do
        echo "$i) $line"
        i=$(( $i + 1 ))
      done
      readint "Which branch? " 0 $branch_lines
      n=$(( $? + 1 ))
      script="$n p"
      branch=$(echo $branch | sed -n $script)
  fi
  git checkout $branch
}

function gbc {
  git branch "$@" | cat
}

function readint {
  if [[ $# -ne 3 ]]; then
      return
  fi
  if [[ ! $3 -ge $2 ]]; then
      return
  fi

  a=$2
  end_loop=""
  while [[ -z $end_loop ]]; do
    vared -p $1 -c a
    if [[ $a -ge $2 ]] && [[ $a -le $3 ]]; then
        end_loop="end"
        return $a
    else
      echo "Should be between $2 and $3!"
      a=$2
    fi
  done
}

function git-is-ancestor {
  if [[ $# -ne 2 ]]; then
    echo "usage: ${FUNCNAME[0]} <patch_sha> <other_sha>"
  fi

  local patch_sha="$1"
  local other_sha="$2"

  # Shorten if it's a hex sha.
  if grep -Pq "^[0-9a-fA-F]+$" <<< $patch_sha; then
    local short_patch=${patch_sha:0:8}
  else
    local short_patch=${patch_sha}
  fi
  if grep -Pq "^[0-9a-fA-F]+$" <<< $other_sha; then
    local short_other=${other_sha:0:8}
  else
    local short_other=${other_sha}
  fi

  local either_ancestor=0

  if git merge-base --is-ancestor $patch_sha $other_sha; then
    echo "${short_patch} is an ancestor of ${short_other}."
    either_ancestor=1
  fi
  if git merge-base --is-ancestor $other_sha $patch_sha; then
    echo "${short_other} is an ancestor of ${short_patch}."
    either_ancestor=1
  fi

  if [[ "${either_ancestor}" == 0 ]]; then
    echo "Neither is an ancestor of the other."
  fi
}
