


photo_id = "9CD3C1BA-043F-4228-B373-A3B4ECC3593A"
app_id = "F39688C1-6F7A-4FDA-A049-4C375B7B8839"
path = "/Users/pjaspers/Library/Application Support/iPhone Simulator/6.1/Applications/#{app_id}/Documents/photos/originals/"

command = <<CO
curl --user "admin@example.com:123456" -X POST -F "image=@#{path}#{photo_id}.jpg" http://0.0.0.0:3000/sync/upload/photo.json\?name\=#{photo_id}
CO

puts command
100.times do
  `#{command}`
end
