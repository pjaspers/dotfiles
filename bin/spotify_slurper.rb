#! /Users/pjaspers/.rvm/rubies/ruby-1.9.3-p0/bin/ruby
# -*- coding: utf-8 -*-
# (The spotify gem requires ruby 1.9.3)
# -*- coding: utf-8 -*-
#
# The [Hallon-gem](https://github.com/Burgestrand/Hallon/) for all things Spotify
require "hallon"

# Get an application key from Spotify (the binary kind), from [here](https://developer.spotify.com/en/libspotify/application-key/)
# The binary kind.
session = Hallon::Session.initialize(IO.read("./spotify_appkey.key"))

# User you login credentials
session.login!("junkiesxl", "scotch grand piano")

# Enter the URI of the playlist you want to add songs to
playlist = Hallon::Playlist.new("spotify:user:junkiesxl:playlist:2YF0zXj84fJvyBOCwEZtDf").load

# thursday = ["NETSKY LIVE", "BJÖRK", "SNOOP DOGG", "BLOC PARTY", "SANTIGOLD", "BUSH", "ZORNIK", "MARK LANEGAN BAND", "FEIST", "THE GASLIGHT ANTHEM", "HOT CHIP", "THE HORRORS", "FRANK OCEAN TINIE TEMPAH", "OF MONSTERS AND MEN", "THE JEZABELS", "THE BONY KING OF NOWHERE & FRIENDS", "NERO DJ SET", "SUB FOCUS (LIVE)", "MODESELEKTOR", "EXAMPLE", "MODESTEP", "DIRTYPHONICS", "LABRINTH", "SOCIAL DISTORTION", "APOCALYPTICA", "ME FIRST AND THE GIMME GIMMES", "STEAK NUMBER EIGHT YOUNG GUNS", "THE BOTS", "BLEED FROM WITHIN", "TOUCHÉ AMORÉ", "THE COMPUTERS", "LAURENT GARNIER LBS FEATURING SCAN X", "DADA LIFE", "FLUX PAVILION", "BORGORE HIGH CONTRAST FEAT. JESSY ALLEN & DYNAMITE MC", "KOAN SOUND", "THE ZOMBIE KIDS", "SCNTST MUMBAI SCIENCE", "TLP", "NICOLAS JAAR LIVE", "RUSTIE LIVE", "DORIAN CONCEPT", "GHOSTPOET", "CHROMATICS", "ALT-J", "STAY+ NO CEREMONY", "tUnE-yArDs", "THE BIG PINK", "LIANNE LA HAVAS", "DJANGO DJANGO", "MINUS THE BEAR", "ALBERTA CROSS BOWERBIRDS", "CLOUD NOTHINGS", "WHITE RABBITS", "VIVE LA FÊTE", "ISBELLS", "DE MENS", "CREATURE WITH THE ATOM BRAIN", "BED RUGS MAD ABOUT MOUNTAINS", "GREAT MOUNTAIN FIRE", "CLOUDS ON ELEKTRICITY", "GLENN CLAES"]

# friday = ["CHASE & STATUS DJ SET AND RAGE","THE STONE ROSES","LYKKE LI","KEANE EAGLES OF DEATH METAL","TWO DOOR CINEMA CLUB","MAXÏMO PARK","BLOOD RED SHOES","FREAKY AGE","THE AFGHAN WHIGS","JAMIE WOON","GOOSE","GRANDADDY","BAND OF SKULLS","SAM SPARRO THE WALKMEN","OBERHOFER","DOG IS DEAD","DIGITALISM LIVE","ANDY C: ALIVE","CARL CRAIG PRESENTS 69 LIVE","CAMO & KROOKED LIVE SEBASTIAN LIVE","JAKWOB LIVE","BASSNECTAR","SOUND OF STEREO LIVE","GOOD RIDDANCE","EVERY TIME I DIE","BILLY TALENT","BARONESS","LETLIVE.","CANCER BATS","SKINDRED DEAF HAVANA","FIDLAR","YASHIN","CROOKERS","KNIFE PARTY","LEN FAKI (FIGURE, OSTGUT TON, BERLIN, GERMANY)","MARTIN SOLVEIG BRODINSKI","NINA KRAVIZ","DOORLY","RAVING GEORGE","EPTIC","MISS POLSKA","TRASH RADIO","CASTELLO","BEN UFO","PANGAEA","PEARSON SOUND","JOY ORBISON","APPARAT BAND","KAP BAMBINO","COM TRUISE" ,"VONDELPARK","BRETON","POLLYN","CHARLES BRADLEY & HIS EXTRAORDINAIRES","THE TALLEST MAN ON EARTH","FINK","WE ARE AUGUSTINES FRIENDS","WILLY MOON","ZULU WINTER","WILLIS EARL BEAL","HONG KONG DONG","CUSTOMS","GORKI","WALLACE VANBORN","MERDAN TAPLAK","DRIVE LIKE MARIA","REIZIGER","GEPPETTO & THE WHALES","THE ME IN YOU"]

# saturday = ["FOO FIGHTERS", "THE BLACK KEYS", "THE HIVES", "THE SHINS", "ALL TIME LOW", "THE CRIBS THE JOY FORMIDABLE", "BALTHAZAR", "DIZZEE RASCAL", "WILCO", "MIIKE SNOW", "OFWGKTA", "BOB MOULD PERFORMS 'COPPER BLUE' STEPHEN MALKMUS AND THE JICKS", "THE VAN JETS", "HOWLER", "DRY THE RIVER", "MAGNETIC MAN", "BURAKA SOM SISTEMA", "GESAFFELSTEIN", "BENGA LIVE", "MAJOR LAZER MS. DYNAMITE", "LOADSTAR LIVE", "DISCLOSURE LIVE", "REFUSED", "ENTER SHIKARI", "GHOST", "GRAVEYARD", "PULLED APART BY HORSES", "TRASH TALK THE JIM JONES REVUE", "DEVIL SOLD HIS SOUL", "CEREMONY", "A-TRAK", "DIPLO", "TIGA", "JACQUES LU CONT", "FEED ME", "GEMINI", "DILLON FRANCIS", "P MONEY EGO TROOPERS", "ED & KIM", "BLAWAN", "HUDSON MOHAWKE", "C2C", "SX", "LOWER DENS", "LIGHT ASYLUM", "MAN WITHOUT COUNTRY TROUMACA", "SLEIGH BELLS", "THE ANTLERS", "PATRICK WATSON", "JESSIE WARE", "DAUGHTER", "JAMIE N COMMONS OSCAR & THE WOLF"]

# Takes a artist name (string), and a `Hallon::Playlist` will try to
# add the top songs of the artists to the playlist.
def add_top_hits_of_artist(artist_name, playlist)
  results = Hallon::Search.new(artist_name).load
  artist = results.artists.first
  if (!artist)
    puts "Nothing found with #{artist_name}, did you mean #{results.did_you_mean}"
    return
  end

  puts "Found #{artist.name}"

  tracks = artist.browse.load.top_hits[0..15]
  puts "  - has #{tracks.size} top hits"

  playlist.insert(playlist.size, tracks).upload
  puts "  - added to the playlist"
end

thursday.each do |artist_name|
  add_top_hits_of_artist(artist_name, playlist)
end
