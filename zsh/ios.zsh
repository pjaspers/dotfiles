# Oh the Xcodes
alias which_xcode='echo "Using `xcode-select -p`"'
alias xcode4='echo "Not installed"'
alias xcode5='echo "Not installed"'
alias xcode6b='sudo xcode-select -s /Applications/Xcode6-Beta3.app/Contents/Developer; which_xcode'
alias xcode61='sudo xcode-select -s /Applications/Xcode6.1.app/Contents/Developer; which_xcode'

# Returns a nicely formatted datestring instead of core data's weird datetype.
function cddate() {
    ruby -e "puts Time.at(Time.gm("2001", "01", "01").to_i + $1)"
}

function killsim () {
	if (type pru &>/dev/null); then
		kill `ps ax | pru '/[Ss]imulator/' | pru 'split(" ")[0]'`
	else
		kill `ps ax | grep '/[Ss]imulator/' | awk '{print $1}'`
	fi
}
