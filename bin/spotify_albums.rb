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
playlist_uri = "spotify:user:junkiesxl:playlist:1qAm6eSmrvkzeyE2d9NsCt"
playlist = Hallon::Playlist.new(playlist_uri).load

nirvana_top_50 = ["The Stooges - Raw Power",
"Pixies - Surfer Rosa",
"The Breeders - POD",
"The Vaselines - Son Of A Gun",
"The Shaggs - Philosophy Of The World",
"Fang - Landshark",
"MDC - Millions Of Dead Cops",
"Scratch Acid - Scratch Acid",
"Saccharine Trust - Paganicons",
"Butthole Surfers - Pee Pee The Sailor",
"Black Flag - My War",
"Bad Brains - Rock For Light",
"Gang Of Four - Entertainment!",
"Sex Pistols - Never Mind The Bollocks",
"The Frogs - It's Only Right And Natural",
"PJ Harvey - Dry",
"Sonic Youth - Daydream Nation",
"The Knack - Get The Knack",
"The Saints - Know Your Product",
"Kleenex - Kleenex/Liliput",
"The Raincoats - The Raincoats",
"Young Marble Giants - Colossal Youth",
"Aerosmith - Rocks",
"Various - What is it?",
"R.E.M. - Green",
"Shonen Knife - Burning Farm",
"The Slits - Typical Girls",
"The Clash - Combat Rock",
"Void/Faith - Void/Faith/Split EP",
"Rites Of Spring - Rites Of Spring",
"Beat Happening - Jamboree",
"Tales Of Terror - Tales Of Terror",
"Leadbelly - Last Sessions Volume 1",
"Mudhoney - Superfuzz Bigmuff",
"Daniel Johnston - Yim Jump Music",
"Flipper - Generic Flipper",
"The Beatles - Meet The Beatles",
"Half Japanese - We Are They Who Ache With Amorous Love",
"Butthole Surfers - Locust Abortion Technician",
"Black Flag - Damaged",
"Fear - The Record",
"Pil - Flowers Of Romance",
"Public Enemy - It Takes A Nation Of Millions To Hold Us Back",
"The Marine Girls Beach Party",
"David Bowie - The Man Who Sold The World",
"The Wipers - Is This Real?",
"The Wipers - Youth Of America",
"The Wipers - Over The Edge",
"Mazzy Star - She Hangs Brightly",
"Swans - Young God EP"]

nirvana_top_50.each do |album_title|
  results = Hallon::Search.new(album_title).load
  album = results.albums.first
  if (!album)
    puts "Nothing found with #{album_title}, did you mean #{results.did_you_mean}"
    next
  end

  playlist.insert(playlist.size, album.browse.load.tracks).upload
end
