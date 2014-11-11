#! /Users/pjaspers/.rvm/rubies/ruby-1.9.3-p0/bin/ruby
# -*- coding: utf-8 -*-
# (The spotify gem requires ruby 1.9.3)
# -*- coding: utf-8 -*-
#
# The [Hallon-gem](https://github.com/Burgestrand/Hallon/) for all things Spotify
require "hallon"

# Get an application key from Spotify (the binary kind), from [here](https://developer.spotify.com/en/libspotify/application-key/)
# The binary kind.
session = Hallon::Session.initialize(IO.read(File.expand_path("~/bin/spotify_appkey.key")))

# User you login credentials
session.login!("junkiesxl", "scotch grand piano")

# Enter the URI of the playlist you want to add songs to
playlist_uri = "spotify:user:junkiesxl:playlist:1O7GyCPlJ2kVjknuqBzBRn"
playlist = Hallon::Playlist.new(playlist_uri).load

tracks = ["No Use For A Name - Secret",
"Pennywise - Greed",
"Swingin' Utters - Five Lessons Learned",
"The Gaslight Anthem - Mae",
"Social Distortion - Don't Drag Me Down",
"Millencolin - Bowmore",
"Fenix TX - All My Fault",
"The Loved Ones - Louisiana",
"Rancid - Maxwell Murder",
"NOFX - August 8th",
"Sum 41 - Makes No Difference",
"No Fun At All - Believers",
"Rise Against - Prayer of the Refugee",
"MxPx - Do Your Feet Hurt (Life In General Album Version)",
"Swingin' Utters - Stupid Lullabies"]

tracks.each do |track_title|
  results = Hallon::Search.new(track_title).load
  track = results.tracks.first
  if (!track)
    puts "Nothing found with #{track_title}, did you mean #{results.did_you_mean}"
    next
  end

  playlist.insert(playlist.size, [track]).upload
end
