function top50 {
  if (( $# < 1 ))
  then echo "Needs a number between 1 and 50"; return 1; fi

  curl -LsS https://gist.github.com/pjaspers/7706719/raw/2b042751d416c53aedf12603b7908bd1a2902fb8/gistfile1.md | grep "^$1\."
}

function stats() {
    fc -l 1 | awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | grep --color=auto -v "./" | column -c3 -s " " -t | sort -nr | nl | head -n20
}

# From [here](https://gist.github.com/lelandbatey/8677901)
# Takes two arguments, <input_file> <output_file>
function whiteboard () {
    convert "$1" -morphology Convolve DoG:15,100,0 -negate -normalize -blur 0x1 -channel RBG -level 60%,91%,0.1 "$2"
}

# Using it to load my passphrase into the gpg agent so that others don't have to ask for it.
# Because I absolutely, positively, dislike GPGTools
function cryptonic() {
  echo "Passphrase injected" | gpg --encrypt --armor -r "piet@pjaspers.com" | gpg -d --use-agent
}

function crypt() {
    echo "$1" | gpg --encrypt --armor -r "$2"
}

function dcrypt() {
    pbpaste | gpg -d --use-agent
}

# Ghetto OCR
#
# Requires:
#  - brew install imagemagick
#  - brew install tesseract --all-languages
#
# Takes pdfs and spits out a txt file with anything it could OCR
function ocr {
   if (( $# < 1 ))
   then echo "usage: ocr filename.pdf"; return 1; fi

   for i in $*
   do
   file_name="$( basename "$i" .pdf)"
   echo "Converting ${file_name} to tif"
   convert -monochrome -density 600 $i "${file_name}.tif"
   echo "Starting OCR"
   tesseract -l nld "${file_name}.tif" "${file_name}" 2>/dev/null
   rm "${file_name}.tif"
   done
   echo "Done."
}

extract () {
    if [ -f $1 ] ; then
      case $1 in
        *.tar.bz2)   tar xjf $1     ;;
        *.tar.gz)    tar xzf $1     ;;
        *.bz2)       bunzip2 $1     ;;
        *.rar)       unrar e $1     ;;
        *.gz)        gunzip $1      ;;
        *.tar)       tar xf $1      ;;
        *.tbz2)      tar xjf $1     ;;
        *.tgz)       tar xzf $1     ;;
        *.zip)       unzip $1       ;;
        *.Z)         uncompress $1  ;;
        *.7z)        7z x $1        ;;
        *)     echo "'$1' cannot be extracted via extract()" ;;
      esac
     else
         echo "'$1' is not a valid file"
     fi
}

# Prints current battery charge
function power {
    /usr/sbin/ioreg -l | awk 'BEGIN{a=0;b=0}
	$0 ~ "MaxCapacity" {a=$5;next}
	$0 ~ "CurrentCapacity" {b=$5;nextfile}
	END{printf("%.2f%%", b/a * 100)}'
}

function powerrr {
    local all_bluetooth=$(defaults read /Library/Preferences/com.apple.Bluetooth)
    local case=$(grep BatteryPercentCase <<< $all_bluetooth | awk -F "=" '{ gsub(";", ""); print $2}')
    local left=$(grep BatteryPercentLeft <<< $all_bluetooth | awk -F "=" '{print $2}')
    local right=$(grep BatteryPercentRight <<< $all_bluetooth | awk -F "=" '{print $2}')
    local battery=$(pmset -g batt | grep InternalBattery | awk '{printf "%s [%s %s]", $3,$5, $6}')
    echo "🔋 $battery"
    echo $case
    if [ ! -z "$case" ] && [ ! "$case" -eq "0" ]; then
       echo "🎧 L: $left R: $right Case: $case"
    else
        echo "Well this is awkward"
    fi
}

# Takes a Github Username and copies his/her public key
#
#       ghkey pjaspers
#
function ghkey {
   if (( $# < 1 ))
   then echo "usage: ghkey <username>"; return 1; fi

    curl -sL https://github.com/$1.keys | pbcopy
}

function f() {
  find * -name $1
}

function wiki() {
  dig +short txt $1.wp.dg.cx
}

alias deletesvn='find . -name ".svn" -exec rm -rf {} \;'

function browse() {
    open "http://$(basename $PWD).dev"
}

## Gifwit helpers
#
# Easy interaction with DB of [gifwit](http://gifwit.com)
function gifcount() {
    COUNT=$(gifme | wc -l)
    echo "Current number of gifs: ${COUNT}"
}

function gifsize() {
    files="${HOME}/Library/Containers/stevesmith.gifwit/Data/Library/Application Support/stevesmith.gifwitfiles"
    du -sh "$files" | awk '{print $1}' | xargs printf "Barely %s of gifs"
}

# Usage: `gifme cats`
function gifme() {
    db="${HOME}/Library/Containers/stevesmith.gifwit/Data/Library/Application Support/stevesmith.gifwitfiles/gifwit.storedata"
    query="select ZURL, ZKEYWORDS from ZIMAGE where ZKEYWORDS LIKE '%$1%';"
    /usr/bin/sqlite3 $db $query | awk '{split($0,a,"|"); printf "\033[1;31m%-20s\033[0m: %s\n",a[2],a[1]}'
}

function safari_top() {
    local number=50
    if (( $# > 0 ));then
        number=$1
    fi
    local db="${HOME}/Library/Safari/History.db"
    local query="select url, visit_count from history_items group by visit_count order by visit_count desc limit ${number}"
    /usr/bin/sqlite3 -column -header $db $query
}

function safari_search() {
    if (( $# < 1 ));then
        echo "search term needed"
        return
    fi
    local db="${HOME}/Library/Safari/History.db"
    local query="select url, visit_count from history_items where url like '%${1}%' order by visit_count DESC;"
    set -x
    /usr/bin/sqlite3 -column -header $db $query
}

function safari_stats() {
    local db="${HOME}/Library/Safari/History.db"

    echo "Hourly"
    /usr/bin/sqlite3 $db $hours <<EOF | spark
select
  count(*)from history_visits
where
  (redirect_source is not null) or (redirect_source is null and redirect_destination is null)
group by
  strftime('%H', datetime(history_visits.visit_time, 'unixepoch', '31 years'));
EOF

    echo "Weekdays"
    /usr/bin/sqlite3 $db $weekdays <<EOF | spark
select
  count(*)from history_visits
where
  (redirect_source is not null) or (redirect_source is null and redirect_destination is null)
group by
  strftime('%w', datetime(history_visits.visit_time, 'unixepoch', '31 years'))
EOF

    echo "Months"
    /usr/bin/sqlite3 $db $months <<EOF | spark
select
  count(*)from history_visits
where
  (redirect_source is not null) or (redirect_source is null and redirect_destination is null)
group by
  strftime('%m', datetime(history_visits.visit_time, 'unixepoch', '31 years'));
EOF

    echo "Days"
    /usr/bin/sqlite3 $db <<EOF | spark
select
  count(*) from history_visits
where
  (redirect_source is not null) or (redirect_source is null and redirect_destination is null)
group by
  strftime('%j', datetime(history_visits.visit_time, 'unixepoch', '31 years'))
EOF

    echo "Most links visited in a day"
    /usr/bin/sqlite3 -list -separator ' → ' $db <<EOF
select
  count(*) as count, strftime('%w %Y%m%d',datetime(history_visits.visit_time, 'unixepoch', '31 years')) as date
from
  history_visits
where
  (redirect_source is not null) or (redirect_source is null and redirect_destination is null)
group by
  strftime('%j%Y', datetime(history_visits.visit_time, 'unixepoch', '31 years'))
order by
  count desc
limit 1
EOF

    echo "Most links visited in a month"
    /usr/bin/sqlite3 -list -separator ' → ' $db <<EOF
select
  count(*) as count, strftime('%m %Y',datetime(history_visits.visit_time, 'unixepoch', '31 years')) as date
from
  history_visits
join history_items on history_visits.history_item = history_items.id
where (redirect_source is not null) or (redirect_source is null and redirect_destination is null)
group by
  strftime('%m%Y', datetime(history_visits.visit_time, 'unixepoch', '31 years'))
order by
  count desc
limit 1
EOF

    echo "Top domains"
    /usr/bin/sqlite3 -list -separator ' → ' $db <<EOF
select
  count(*) as thing, substr(replace(replace(replace(replace(url, 'https://', 'http://'), 'http://', ''), 'www.', ''), 'm.s', 's'), 0, instr(replace(replace(replace(replace(url, 'https://', 'http://'), 'http://', ''), 'www.', ''), 'm.s', 's'), '/')) as domain
from
  history_visits
join
  history_items on history_visits.history_item = history_items.id
where
  (redirect_source is not null) or (redirect_source is null and redirect_destination is null)
group by
  domain
order by
  thing desc
limit 20
EOF
    # "select datetime(history_visits.visit_time, 'unixepoch', '31 years'), url, redirect_source, redirect_destination from history_visits join history_items on history_visits.history_item = history_items.id where ((redirect_source is not null) or (redirect_source is null and redirect_destination is null)) and datetime(history_visits.visit_time, 'unixepoch', '31 years') between '2018-08-01' and '2018-09-01' order by history_visits.visit_time"
    #
    # select avg(count) from (select count(*) as count, datetime(history_visits.visit_time, 'unixepoch', '31 years') as date from history_visits where ((redirect_source is not null) or (redirect_source is null and redirect_destination is null)) and date between '2018-01-01' and '2019-01-01' group by strftime('%j', date))
    #
    # select count/avg(max_hour - min_hour) as average_things from (
    # select count(*) as count,
    # min(strftime('%H', datetime(history_visits.visit_time, 'unixepoch', '31 years'))) as min_hour,
    # max(strftime('%H', datetime(history_visits.visit_time, 'unixepoch', '31 years'))) as max_hour,
    # datetime(history_visits.visit_time, 'unixepoch', '31 years') as date
    # from
    # history_visits
    # where
    # ((redirect_source is not null) or (redirect_source is null and redirect_destination is null))
    # and
    # date between '2018-01-01' and '2019-01-01'
    # group by
    # strftime('%j', date));
}

# Takes a regex and show all words that match it
function wordme() {
  grep --colour -E "$1" /usr/share/dict/words
}
function pj_velo() {
    url=https://www.velo-antwerpen.be/CallWebService/StationBussinesStatus.php
    station_id=97
    curl -sSd "idStation=$station_id&s_id_idioma=nl" $url | awk '{split($0,a," "); printf "Nog %s 🚲 beschikbaar", a[5]}'
}

# Needs VELO_EMAIL to be set, will submit the form which will send a mail with the
# current waiting list number. (Not really sure why it doesn't send back the actual
# number)
function pj_velo_waiting_list() {
    velo_email=$VELO_EMAIL
    curl -sS -X POST -d "CustEmail=$velo_email&form_id=checkposition_form" "https://www.velo-antwerpen.be/nl/registreren/wachtlijst-positie"
    echo "An email will be arriving soon."
}

function pj_check_ssl() {
    if (( $# < 1 ))
   then echo "usage: pj_check_ssl <site>"; return 1; fi
    echo | openssl s_client -connect $1:443 2>/dev/null | openssl x509 -text
}

# Fetches the last 1000 tweets of a twitter username and scans them for gifs
#       requirements: - https://github.com/sferik/t
#
# Returns path to file with gifs.
function pj-fetch-twitter-gifs() {
    if (( $# < 1 ))
    then echo "usage: pj-fetch-twitter-gifs <username>"; return 1; fi
    rm /var/tmp/$1.*
    csv=/var/tmp/$1.csv
    urls_file=/var/tmp/$1.only_urls
    result=/var/tmp/$1.gifs
    echo "Fetching tweets using the 't' gem"
    t timeline $1 -d -n 1000 -c > $csv
    echo "Extracting URLs -> $urls_file"
    cat $csv | sed -ne 's/.*\(http[^"]*\).*/\1/p' | sed '/instagram/d' | sed -e 's/ .*$//' | sort -u | uniq -u > $urls_file
    echo "Checking for gifs -> $result"
    for i in $(cat $urls_file); do
        curl -sSIL $i | grep 'image/gif' && echo $i >> $result
    done
    echo "Exported the gifs to: $result"
}

## Set a random background color for this shell
function pj-random-color() {
    color=$(random_css_color)
    wasko -p $color
    echo "O HAI $color:u"
}

function cattish() {
    cat <<"EOT"
                   ;,_            ,
                  _uP~"b          d"u,
                 dP'   "b       ,d"  "o
                d"    , `b     d"'    "b
               l] [    " `l,  d"       lb
               Ol ?     "  "b`"=uoqo,_  "l
             ,dBb "b        "b,    `"~~TObup,_
           ,d" (db.`"         ""     "tbc,_ `~"Yuu,_
         .d" l`T'  '=                      ~     `""Yu,
       ,dO` gP,                           `u,   b,_  "b7
      d?' ,d" l,                           `"b,_ `~b  "1
    ,8i' dl   `l                 ,ggQOV",dbgq,._"  `l  lb
   .df' (O,    "             ,ggQY"~  , @@@@@d"bd~  `b "1
  .df'   `"           -=@QgpOY""     (b  @@@@P db    `Lp"b,
 .d(                  _               "ko "=d_,Q`  ,_  "  "b,
 Ql         .         `"qo,._          "tQo,_`""bo ;tb,    `"b,
(qQ         |L           ~"QQQgggc,_.,dObc,opooO  `"~~";.   __,7,
`qp         t\io,_           `~"TOOggQV""""        _,dg,_ =PIQHib.
 `qp        `Q["tQQQo,_                          ,pl{QOP"'   7AFR`
   `         `tb  '""tQQQg,_             p" "b   `       .;-.`Vl'
              "Yb      `"tQOOo,__    _,edb    ` .__   /`/'|  |b;=;.__
                            `"tQQQOOOOP""        `"\QV;qQObob"`-._`\_~~-._
                                 """"    ._        /   | |oP"\_   ~\ ~\_  ~\
                                         `~"\ic,qggddOOP"|  |  ~\   `\  ~-._
                                           ,qP`"""|"   | `\ `;   `\   `\
                                _        _,p"     |    |   `\`;    |    |
                                 "boo,._dP"       `\_  `\    `\|   `\   ;
                                  `"7tY~'            `\  `\    `|_   |
                                                           `~\  |
EOT
    cat "$@"
}

# Fuck tco links
#
# Returns the actual URL (and a copy is placed in the clipboard)
function ftco() {
    if (( $# < 1 ))
    then echo "usage: ftco <tco-link>"; return 1; fi

    curl -sSI $1 | grep "location:" | awk '{print $2}' | tee >(pbcopy)
}

# Takes current branch and tries to deploy it using capistrano
function dp() {
    if [[ $1 ]] && env=$1 || env="staging"
    branch=$(git rev-parse --abbrev-ref HEAD)
    echo "GATEWAY=1 BRANCH=$branch bundle exec cap $env deploy" | tee >(pbcopy)
}

# Fetches the branch name of a Pull Request
function pr() {
    if (( $# < 2 ))
    then echo "usage: pr <name/repo> <number>"; return 1; fi

    curl -sSH "Authorization: token $GITHUB_API_TOKEN" "https://api.github.com/repos/$1/pulls/$2" | grep -m 1 label | awk -F ': ' '{ print $2 }' | sed -e 's/["|,]//g'
}

# When on a wonky wifi, this will try to reclaim an IP-address
function renew_dhcp() {
    echo "add State:/Network/Interface/en0/RefreshConfiguration temporary" | sudo scutil
}

# That awkward moment you want a simple function to the scores from the 2016 final between Ding and Selby.
# Come on Ding!
function snooker() {
    curl -sS "http://livescores.worldsnookerdata.com/LiveScoring/Match/13868/444262/world-championship?pos=342" | nokogiri -e 'table = $_.at_css(".live-match-number").next();sa,sb = table.css("tr:last td").map(&:text);fa,fb = table.css("tr:first td").map(&:text);puts "%d [%d-%d] %d" % [sa.to_i, fa.to_i, fb.to_i, sb.to_i]'
}

# That awkward moment you come across a small JS include on a news site
# and you think you might need it someday so you keep it in a stupid
# litte function. Because then one day, you'll be able to say:
#
#           You want to know the total amount of congestion on Belgian's
#           road system, this very minute?
#
#           Stand back. I have a function for this.
function btraffic() {
    local url='https://services.vrt.be/traffic/teaser?accept=application%2Fvnd.traffic.vrt.be.traffic_jam_length_1.0%2Bjson'
    curl -sS $url |
        jq '.trafficJamLength' |
        xargs echo "0.001*" |
        bc |
        xargs echo "Aantal km file:"
}

function random_line() {
    if (( $# < 1 ))
    then echo "usage: random_line path/to/file"; return 1; fi

   head -$((${RANDOM} % `wc -l < $1` + 1)) $1 | tail -1
}

function wat() {
    random_line "${HOME}/.config/randoms/${1}"
}

function phantom_me() {
    random_line ~/.config/randoms/phantom_menace
}

function trooper_me() {
    random_line ~/.config/randoms/starship_trooper
}

function crypto_me() {
    random_line ~/.config/randoms/crypto_names
}
function cert_info() {
    if (( $# < 1 ))
    then echo "usage: cert_info server.tld"; return 1; fi

    echo | openssl s_client -showcerts -servername $1 -connect $1:443 2>/dev/null | openssl x509 -inform pem -noout -text
}

function pwcheck() {
    # https://www.troyhunt.com/ive-just-launched-pwned-passwords-version-2/
    echo -n "Password: "
    read -s password
    local head=$(echo -n $password | shasum | cut -b 1-5)
    local tail=$(echo -n $password | shasum | cut -b 6-40 | tr /a-f/ /A-F/)
    curl -sS https://api.pwnedpasswords.com/range/$head | grep "$tail"
}

# On Slack's free plan, you sometimes want to kill your own darlings/files. This helps doing
# that. For a Python version go ask @teufen.
function slackfilesbegone() {
    for page in {0..50}; do
        echo "Fetchez le page numero \033[33;5m$page\033[0m"
        curl -sS "https://slack.com/api/files.list?token=$SLACK_TOKEN&page=$page" |
            jq '.files[].id' |
            while read FILE_ID; do
                echo -n "  Deleting \033[0;36m${FILE_ID//\"}\033[0m\n";
                res=$(curl -sSX POST "https://slack.com/api/files.delete?token=$SLACK_TOKEN&file=${FILE_ID//\"}")
                echo "  =~> $res"
            done
    done
}


# Generate a Simpsons-ladida, thanks to the incomparable @lewisfidlers
function ladida () {
    url="https://bd8wz9ifl5.execute-api.eu-west-3.amazonaws.com/Production/%7Bladida+%7D"
    curl -G -s --data-urlencode "text=$*" $url | pbcopy
}

# Get a man page as a PDF in Preview.app
# Borrowed from oh-my-zsh, and seen [here](https://twitter.com/emilyst/status/1039540902773972997)
function man-preview() {
    man -t "$@" | open -f -a Preview
}
