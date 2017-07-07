#!/bin/zsh
## Editor/emacs

# Using the long route as some applications have difficulty resolving
# the alias (*cough* Bundle *cough*)
export EDITOR='emacsclient'
alias m='/usr/bin/env emacsclient'

## Shell

alias ls='ls -G'
alias fj=pj                           # Easier access.
# From [http://stackoverflow.com/questions/890620/unable-to-have-bash-like-c-x-e-in-zsh](here),
# an easy way to edit long commands.
autoload edit-command-line
zle -N edit-command-line
bindkey '^Xe' edit-command-line
alias eprof='$EDITOR $ZSH && reload!' # Edit and reload
alias reload!='source ~/.zshrc'       # reload config files
alias publickey='cat ~/.ssh/id_rsa.pub | pbcopy'
alias rr=". $ZSH/bin/pj_change_ruby"
# From @inferis, easy nocorrect
if [ -f ~/.zsh_nocorrect ]; then
    while read -r COMMAND; do
        alias $COMMAND="nocorrect $COMMAND"
    done < ~/.zsh_nocorrect
fi
# Additional Dutch voice on OSX, it's great.
alias zeg="say -v 'Xander'"
# By far the most uless alias in here
alias savethisscreen='j=0;a=1;x=1;y=1;xd=1;yd=1;while true;do for i in {1..2000} ; do if [[ $x == $LINES || $x == 0 ]]; then xd=$(( $xd *-1 )) ; fi ; if [[ $y == $COLUMNS || $y == 0 ]]; then yd=$(( $yd * -1 )) ; fi ; x=$(( $x + $xd )); y=$(( $y + $yd )); printf "\33[%s;%sH\33[48;5;%sm . \33[0m" $x $y $(( $a % 8 + 16 + $j % 223 )) ;a=$(( $a + 1 )) ; done ; x=$(( x%$COLUMNS + 1 )) ; j=$(( $j + 8 )) ;done'
alias ssh-config='$EDITOR ~/.ssh/config'

## Git

alias g=git
alias gti=git
alias gst='g status -sb'
if (( $+commands[gh] ))
then
    # https://github.com/jingweno/gh
    # Alias generated with `gh alias -s`
    alias git=gh
    if type compdef > /dev/null; then
        compdef gh=git
    fi
fi

## OSX Stuff

alias ql="qlmanage -p 2>/dev/null"
alias airport="/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport"
alias wa=pj-random-color

if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi

export PYENV_ROOT=/usr/local/var/pyenv

# Frasier's little brother - https://github.com/pjaspers/frasier
#
# Returns 4 random words separated by a '-', quick way to get a random identifier.
alias niles='WEBSCALE=1; ruby -e "print File.open('"'/usr/share/dict/words'"').read.lines.reject{|w| w.length < 3 || 10 < w.length}.sample(4).map{|w| w.strip! && w.downcase}.join('"'-'"')" | tee >(pbcopy)'

alias niles='WEBSCALE=1; ruby -e "print File.open('"'/usr/share/dict/words'"').read.lines.reject{|w| w.length < 3 || 10 < w.length}.sample(4).map{|w| w.strip! && w.downcase}.join('"'-'"')" | tee >(pbcopy)'
