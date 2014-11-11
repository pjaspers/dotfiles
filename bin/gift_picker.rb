partners = {"mama" => "papa", "koen" => "sien", "bart" => "kelly", "dries" => "stephanie"}
people = [partners.keys + partners.values].flatten.uniq

results = partners.inject({}) do |gifts, (a,b)|
  gifts[a] = (people - [a,b]).sample
  gifts[b] = (people - [a,b,gifts[a]]).sample
  people -= [gifts[a], gifts[b]]
  gifts
end

results.each {|a,b| puts("%s => %s" % [a,b].map(&:capitalize))}
