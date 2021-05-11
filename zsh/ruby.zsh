# From [here](http://ryan.mcgeary.org/2011/02/09/vendor-everything-still-applies/)
# VENDOR ALL THE THINGS
alias b="bundle"
alias bi="bundle install"
alias bil="bundle install --local"
alias bu="bundle update"
alias be="bundle exec"
alias binit="bundle install && bundle package && bundle config set path 'vendor' && echo 'vendor/ruby' >> .gitignore"

# From https://twitter.com/defv/status/185346025467281409
function last_migration_version() {
    ls -1t db/migrate | awk -F_ '{ print $1 }' | head -n 1
}

# $1: port number (64304)
# $2: text
function papertrail() {
    nc -w0 -u logs.papertrailapp.com $1 <<< $2
}

# tail log
alias tailtest='tail -f log/test.log'
alias devlog='tail -f log/development.log'

alias rbcop="git status --porcelain | cut -c4- | grep '\.rb' | xargs rubocop"
