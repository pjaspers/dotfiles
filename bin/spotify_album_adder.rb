# The [Hallon-gem](https://github.com/Burgestrand/Hallon/) for all things Spotify
require "hallon"

# Get an application key from Spotify (the binary kind), from [here](https://developer.spotify.com/en/libspotify/application-key/)
# The binary kind.
session = Hallon::Session.initialize(IO.read(File.expand_path("~/development/shell/dotfiles/bin/spotify_appkey.key")))

# User you login credentials
session.login!("junkiesxl", ENV["SPOTIFY_PASS"])
playlist_uri = "spotify:user:junkiesxl:playlist:1td6hPLdjO8Eij9S94iHeK"
playlist = Hallon::Playlist.new(playlist_uri).load

# From the incomparable #288
# https://www.theincomparable.com/theincomparable/288/index.php
albums = ["Draconian Times / Paradise Lost",
          "Ziggy Stardust and the Spiders of Mars / David Bowie",
          "Abbey Road / The Beatles",
          "Anything Goes",
          "The Beatles / The Beatles",
          "The Joshua Tree / U2",
          "The Reptile House / Sisters of Mercy",
          "Court and Spark / Joni Mitchell",
          "Together Again / Tony Bennett and Bill Evans",
          "Concert in Central Park / Simon & Garfunkel",
          "Loveless / My Bloody Valentine",
          "Welcome Interstate Managers / Fountains of Wayne",
          "Scoundrel Days / a-ha",
          "Harvest / Neil Young",
          "The Spine / They Might Be Giants",
          "Something Fierce / Marian Call",
          "Meddle / Pink Floyd",
          "The Bends / Radiohead"]

albums.each do |album_title|
  results = Hallon::Search.new(album_title).load
  album = results.albums.first
  if !album
    puts "Nothing found with #{album_title}, did you mean #{results.did_you_mean}"
    next
  end

  browser = album.browse
  browser.load
  puts "Found #{album.name} - adding #{browser.tracks.count} tracks"
  playlist.insert(playlist.size, browser.tracks).upload
end
