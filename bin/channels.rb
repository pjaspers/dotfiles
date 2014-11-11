require 'open-uri'
require 'json'

url = "http://services.vmma.be/feed/epg"
humo = "http://www.humo.be/api/epg/humosite/schedule/main/2013-12-26/full"

path = File.join("/tmp" , "piek_%s.json" % Time.now.strftime("%m-%d"))
unless File.exists?(path)
  File.open(path, 'w') do |f|
    f.write open(url).read
  end
end

channels = JSON.load(File.open(path))["channels"]

def current(c)
  c["items"].detect do |i|
    Time.at(i["start_time"].to_i) < Time.now && Time.now < Time.at(i["stop_time"].to_i)
  end
end

puts channels.collect{|c| "%s - %s" % [c["machinename"], current(c)["title"]]}
