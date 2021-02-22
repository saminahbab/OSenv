export PATH=$HOME/.cargo/bin:$PATH

export GOPATH=$HOME/go

export PATH=$GOPATH/bin:$PATH
export PATH=~/.bin:$PATH
export GOPRIVATE=github.com/saminahbab/*

#aliases cant live without
alias lt='ls --human-readable --size -1 -S --classify'
alias gh='history|grep'
alias die='systemctl poweroff'
alias nap='systemctl hibernate'
alias chx='chmod +x '
##python aliases
alias ve='python3 -m venv ./venv'
alias va='source ./venv/bin/activate'
alias pr='pip3 install -r requirements.txt'
alias py='python3'
alias pi='pip3 install'
## go aliases
alias gun='go run'
alias gup='go get -u ./...'

## docker
alias dup='docker-compose up'
alias docks='systemctl start docker.service'

## Kubernetes
alias kc='kubectl'

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi
