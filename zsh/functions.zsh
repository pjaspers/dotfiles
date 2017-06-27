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

function mx { ruby $ZSH/bin/copy_maxgif.rb $argv}

# Syntax-highlight JSON strings or files
function json() {
	if [ -p /dev/stdin ]; then
		# piping, e.g. `echo '{"foo":42}' | json`
		python -mjson.tool | pygmentize -l javascript
	else
		# e.g. `json '{"foo":42}'`
		python -mjson.tool <<< "$*" | pygmentize -l javascript
	fi
}

# Takes a repo and sets the hook between pivotal and Github
#
# pivhub 10to1/report
#
function pivhub {
   if (( $# < 1 ))
   then echo "usage: pivhub <org/reponame>"; return 1; fi
   curl -L --user "pjaspers" -d "{'name': 'pivotaltracker', 'active' : true, 'config': {'token':'${PIVOTAL_API_TOKEN}'}}" https://api.github.com/repos/$1/hooks
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
