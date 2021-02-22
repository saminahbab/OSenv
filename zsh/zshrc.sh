plugins=(git archlinux history-substring-search zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

export PATH=$HOME/.cargo/bin:$PATH
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$PATH
export PATH=~/.bin:$PATH

export GOPRIVATE=github.com/saminahbab/*

alias lt='ls --human-readable --size -1 -S --classify'
alias gh='history|grep'
alias die='systemctl poweroff'
alias nap='systemctl hibernate'
alias wif='nmcli device wifi connect BTWholeHome-WGH'
alias eyes='redshift -l 51.5:-0.127 '

alias ve='python3 -m venv ./venv'
alias va='source ./venv/bin/activate'
alias pr='pip3 install -r requirements.txt'
alias py='python3'
alias pi='pip3 install'
alias pu='pip3 uninstall'

alias gun='go run'
alias gup='go get -u ./...'

alias dup='docker-compose up'
alias docks='systemctl start docker.service'
alias dune='docker system prune'
alias drop='docker stop $(docker ps -a -q) && docker rm $(docker ps -a -q)'

alias kc='kubectl'
alias kup='aws eks --region eu-west-2 update-kubeconfig --name '
alias kval='kubeval --strict'
alias kap='kubectl apply -f'
alias kdeb='kubectl exec --stdin --tty debug -- /bin/zsh'

[ -f ~/.kubealiases ] && source ~/.kubealiases

alias tap='terraform apply'
alias tpl='terraform plan'
alias ted='terraform destroy'

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh

vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
