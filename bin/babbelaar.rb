#! /usr/bin/env ruby
#
# Small script that spits some nonsense into a room on Campfire.
#
# Usage:
#
#      ruby ~/path/to/babbelaar.rb campfire_token
#
# Where campfire_token is your [token](https://10to1.campfirenow.com/member/edit).
# It will keep talking until you quit it with Control-C
require "broach"

token = ARGV[0]
campfire_room = "Butane"
words_file = File.join("/", 'usr', "share", "dict", "words")
indices = (0..File.open(words_file).lines.count).to_a

puts "Setting up campfire connection..."
Broach.settings = {
  'account' => '10to1',
  'token' => token,
  'use_ssl' => true
}
room = Broach::Room.find_by_name(campfire_room)

puts "Done. Talking in the #{campfire_room} Room"

while true
  random_word = File.readlines(words_file)[indices.sample].strip
  room.speak(random_word)
  puts "  and I was like \"#{random_word}\""
  sleep 10
end
