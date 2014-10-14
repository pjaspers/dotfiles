# Oh the Xcodes
alias xcode4='sudo xcode-select -s /Applications/Xcode_4.6.3.app/Contents/Developer; echo "Using `xcode-select -p`"'
alias xcode5='sudo xcode-select -s /Applications/Xcode.app/Contents/Developer; echo "Using `xcode-select -p`"'
alias xcode6='sudo xcode-select -s /Applications/Xcode6-Beta3.app/Contents/Developer; echo "Using `xcode-select -p`"'

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
