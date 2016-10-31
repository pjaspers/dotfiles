#! /usr/bin/env ruby
#
# Takes an URL from http://gifs.com and copies the url of the shown gif
#

unless ARGV[0]
  banner = <<BANNER
Takes a http://gifs.com url and copies the direct link to the GIF. If
supplied a second argument, it is used as the name in `boom`.

      mx <gifs.com url> <optional name>

BANNER
  abort banner
end

unless ARGV[0] =~ /gifs.com/
  puts "Not a gifs.com url, YMMV"
end
require 'open-uri'
require 'cgi'
require 'json'

url = ARGV[0]
name = ARGV[1]

def better_gif(url)
  random_letters = (0...8).map { (65 + rand(26)).chr }.join
  json = JSON.parse(open("http://upload.gfycat.com/transcode/#{random_letters}?fetchUrl=#{url}").read) rescue nil
  return json["gifUrl"] if json["gifUrl"]
  return nil unless json
  if res = JSON.parse(open("http://http://gfycat.com/cajax/get/#{json["gfyname"]}").read)["gifUrl"] rescue nil
    if res["error"]
      puts res["error"]
    else
      puts res["gifUrl"]
      res["gifUrl"]
    end
  end
end

source = `curl -sL #{url} | grep "og:image"`
gif = CGI.unescapeHTML(source.match(/content="(.*?)"/)[1])

# puts better_gif(gif.strip)
system("printf '%s' '#{gif.strip}' | pbcopy")
if name
  system("boom gifs #{name} #{gif}")
else
  puts "Copied #{gif}"
end
