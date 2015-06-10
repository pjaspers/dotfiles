#! /Users/pjaspers/.rvm/rubies/ruby-1.9.3-p0/bin/ruby
# -*- coding: utf-8 -*-
# (The spotify gem requires ruby 1.9.3)
# -*- coding: utf-8 -*-
#
# The [Hallon-gem](https://github.com/Burgestrand/Hallon/) for all things Spotify
require "hallon"
require "timeout"

raise "Set a Spotify password" unless ENV["SPOTIFY_PASS"]

# Get an application key from Spotify (the binary kind), from [here](https://developer.spotify.com/en/libspotify/application-key/)
# The binary kind.
def reload_session
  session = Hallon::Session.initialize(IO.read("./spotify_appkey.key"))

  # User you login credentials
  session.login!("junkiesxl", ENV["SPOTIFY_PASS"])
  puts "Reloaded session"
end

reload_session
# Enter the URI of the playlist you want to add songs to
# "spotify:user:junkiesxl:playlist:2YF0zXj84fJvyBOCwEZtDf"
spotify_uri = "spotify:user:junkiesxl:playlist:22QUx82lTszpiXE85MxchP"
playlist = Hallon::Playlist.new(spotify_uri).load



# "A-Trak", "Above & Beyond", "Acid Arab DJ Set", "Adam Beyer", "Alice On The Roof", "All Time Low", "Allah-Las", "Alt-J", "Amenra", "Architects", "Atreyu", "Baauer", "Bad Breeding", "Baroness", "Bastille", "Bear's Den", "Beatsteaks", "Beauhause", "Benjamin Booker", "Billie", ""Black Box Revelation", "Black Sun Empire & MC LowQui", "Bleachers", "Bony King", "Boys Noize", "Brodinski presents Brava", "Brutus", "Cardiknox", "Charles Bradley & his Extraordinaires", "Charli XCX", "Christine And The Queens", "Chunk! No, Captain Chunk!", "Chvrches", "Coheed And Cambria", "Condor Gruppe", "Courtney Barnett", "Craze", "Critical Soundsystem feat Mefjus x Kasra x Enei", "Curtis Harding", "Cymbals Eat Guitars", "Daniel Avery b2b Erol Alkan", "De Sluwe Vos live (Kontra Album Show)", "Dead Souls", "The Dear Hunter", "Dez Mona", "The Dillinger Escape Plan", "Diplo", "The Districts", "Dj Mustard","Django Django", "Dolomite Minor", "Dorian Concept Liveband feat. Cid Rim & The Clonious", "Douglas Firs", "Dropkick Murphys", "Duke Dumont Live", "Dusky", "Echosmith", "Ellie Goulding", "Elliphant", "Enter Shikari", "Evil Superstars", "Faisal", "Fakear", "Fat White Family", "Father John Misty", "FFS (Franz Ferdinand & Sparks)", "Flako", "Flosstradamus", "Four Tet", "Future Islands", "Ganz", "Garden City Movement", "The Gaslight Anthem", "Gazelle Twin", "Gengahr",

artists = IO.readlines("pp_artists_2015")
# thursday = ["NETSKY LIVE", "BJÖRK", "SNOOP DOGG", "BLOC PARTY", "SANTIGOLD", "BUSH", "ZORNIK", "MARK LANEGAN BAND", "FEIST", "THE GASLIGHT ANTHEM", "HOT CHIP", "THE HORRORS", "FRANK OCEAN TINIE TEMPAH", "OF MONSTERS AND MEN", "THE JEZABELS", "THE BONY KING OF NOWHERE & FRIENDS", "NERO DJ SET", "SUB FOCUS (LIVE)", "MODESELEKTOR", "EXAMPLE", "MODESTEP", "DIRTYPHONICS", "LABRINTH", "SOCIAL DISTORTION", "APOCALYPTICA", "ME FIRST AND THE GIMME GIMMES", "STEAK NUMBER EIGHT YOUNG GUNS", "THE BOTS", "BLEED FROM WITHIN", "TOUCHÉ AMORÉ", "THE COMPUTERS", "LAURENT GARNIER LBS FEATURING SCAN X", "DADA LIFE", "FLUX PAVILION", "BORGORE HIGH CONTRAST FEAT. JESSY ALLEN & DYNAMITE MC", "KOAN SOUND", "THE ZOMBIE KIDS", "SCNTST MUMBAI SCIENCE", "TLP", "NICOLAS JAAR LIVE", "RUSTIE LIVE", "DORIAN CONCEPT", "GHOSTPOET", "CHROMATICS", "ALT-J", "STAY+ NO CEREMONY", "tUnE-yArDs", "THE BIG PINK", "LIANNE LA HAVAS", "DJANGO DJANGO", "MINUS THE BEAR", "ALBERTA CROSS BOWERBIRDS", "CLOUD NOTHINGS", "WHITE RABBITS", "VIVE LA FÊTE", "ISBELLS", "DE MENS", "CREATURE WITH THE ATOM BRAIN", "BED RUGS MAD ABOUT MOUNTAINS", "GREAT MOUNTAIN FIRE", "CLOUDS ON ELEKTRICITY", "GLENN CLAES"]

# friday = ["CHASE & STATUS DJ SET AND RAGE","THE STONE ROSES","LYKKE LI","KEANE EAGLES OF DEATH METAL","TWO DOOR CINEMA CLUB","MAXÏMO PARK","BLOOD RED SHOES","FREAKY AGE","THE AFGHAN WHIGS","JAMIE WOON","GOOSE","GRANDADDY","BAND OF SKULLS","SAM SPARRO THE WALKMEN","OBERHOFER","DOG IS DEAD","DIGITALISM LIVE","ANDY C: ALIVE","CARL CRAIG PRESENTS 69 LIVE","CAMO & KROOKED LIVE SEBASTIAN LIVE","JAKWOB LIVE","BASSNECTAR","SOUND OF STEREO LIVE","GOOD RIDDANCE","EVERY TIME I DIE","BILLY TALENT","BARONESS","LETLIVE.","CANCER BATS","SKINDRED DEAF HAVANA","FIDLAR","YASHIN","CROOKERS","KNIFE PARTY","LEN FAKI (FIGURE, OSTGUT TON, BERLIN, GERMANY)","MARTIN SOLVEIG BRODINSKI","NINA KRAVIZ","DOORLY","RAVING GEORGE","EPTIC","MISS POLSKA","TRASH RADIO","CASTELLO","BEN UFO","PANGAEA","PEARSON SOUND","JOY ORBISON","APPARAT BAND","KAP BAMBINO","COM TRUISE" ,"VONDELPARK","BRETON","POLLYN","CHARLES BRADLEY & HIS EXTRAORDINAIRES","THE TALLEST MAN ON EARTH","FINK","WE ARE AUGUSTINES FRIENDS","WILLY MOON","ZULU WINTER","WILLIS EARL BEAL","HONG KONG DONG","CUSTOMS","GORKI","WALLACE VANBORN","MERDAN TAPLAK","DRIVE LIKE MARIA","REIZIGER","GEPPETTO & THE WHALES","THE ME IN YOU"]

# saturday = ["FOO FIGHTERS", "THE BLACK KEYS", "THE HIVES", "THE SHINS", "ALL TIME LOW", "THE CRIBS THE JOY FORMIDABLE", "BALTHAZAR", "DIZZEE RASCAL", "WILCO", "MIIKE SNOW", "OFWGKTA", "BOB MOULD PERFORMS 'COPPER BLUE' STEPHEN MALKMUS AND THE JICKS", "THE VAN JETS", "HOWLER", "DRY THE RIVER", "MAGNETIC MAN", "BURAKA SOM SISTEMA", "GESAFFELSTEIN", "BENGA LIVE", "MAJOR LAZER MS. DYNAMITE", "LOADSTAR LIVE", "DISCLOSURE LIVE", "REFUSED", "ENTER SHIKARI", "GHOST", "GRAVEYARD", "PULLED APART BY HORSES", "TRASH TALK THE JIM JONES REVUE", "DEVIL SOLD HIS SOUL", "CEREMONY", "A-TRAK", "DIPLO", "TIGA", "JACQUES LU CONT", "FEED ME", "GEMINI", "DILLON FRANCIS", "P MONEY EGO TROOPERS", "ED & KIM", "BLAWAN", "HUDSON MOHAWKE", "C2C", "SX", "LOWER DENS", "LIGHT ASYLUM", "MAN WITHOUT COUNTRY TROUMACA", "SLEIGH BELLS", "THE ANTLERS", "PATRICK WATSON", "JESSIE WARE", "DAUGHTER", "JAMIE N COMMONS OSCAR & THE WOLF"]

# Takes a artist name (string), and a `Hallon::Playlist` will try to
# add the top songs of the artists to the playlist.
def add_top_hits_of_artist(artist_name, playlist)
  begin
    Timeout::timeout(10) do
      puts "Searching for #{artist_name}"
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
  rescue Timeout::Error
    puts '#{artist_name} timed out. Ever so sorry'
  end
end

artists.dup.each do |artist_name|
  add_top_hits_of_artist(artist_name, playlist)
  artists.shift
  File.open("pp_artists_2015", "w") { |f| f.puts artists }
end
