def title_for(hash)
  word = hash.fetch("Name")
  letters = word.split("")
  letters[0] = letters[0].upcase
  letters.join("")
end

def key_for(hash)
  hash.fetch("Name").downcase.gsub(/ +/, '_')
end

def system_file(system:)
  File.open("/Users/jfriesen/git/org/rpgs/thel-sector/systems/#{key_for(system)}.org", "w+") do |file|

    file.puts "#+roam_key: rpgs-thel-sector-systems:#{key_for(system)}"
    file.puts "* #{title_for(system)}"
    file.puts ""
    file.puts "  - Tags :: [[file:../systems.org][Systems]]"
    file.puts "  - Hex ::  (#{system.fetch("Row")},#{system.fetch("Col")})"
    links = system.fetch("Worlds").map do |world|
      world_file(system: system, world: world)
      "[[file:../worlds/#{key_for(world)}.org][#{title_for(world)}]]"
    end
    file.puts "  - Worlds :: #{links.join(', ')}"
    file.puts ""
    file.puts "** Points of Interest"
    if pois = system.fetch("POIs")
      file.puts ""
      pois.each do |poi|
        poi.keys.each do |key|
          file.puts "    - #{key} :: #{poi.fetch(key)}"
        end
      end
    end
  end
end

def world_file(system:, world:)
  File.open("/Users/jfriesen/git/org/rpgs/thel-sector/worlds/#{key_for(world)}.org", "w+") do |file|
    file.puts "#+roam_key: rpgs-thel-sector-worlds:#{key_for(world)}"
    file.puts "* #{title_for(world)}"
    file.puts ""
    file.puts "  - System :: [[file:../systems/#{key_for(system)}.org][#{title_for(system)}]]"
    tags = world.fetch("Tags").map do |tag|
      tag_file(tag: tag)
      "[[file:../../swn/tags/worlds/#{key_for(tag)}.org][#{title_for(tag)}]]"
    end
    file.puts "  - Tags :: #{tags.join(', ')}"

    [
      "Atmosphere",
      "Biosphere",
      "Culture",
      "Population",
      "TechLevel",
      "Temperature",
      "Origin",
      "Relationship",
      "Contact"
    ].each do |key|
      file.puts "  - #{key} :: #{world.fetch(key)}" unless world.fetch(key).empty?
    end
    file.puts ""
    file.puts "** Points of Interest"
  end
end

def tag_file(tag:)
  name = key_for(tag)
  File.open("/Users/jfriesen/git/org/rpgs/swn/tags/worlds/#{name}.org", "w+") do |file|
    file.puts "#+roam_key: rpgs-swn-tags-worlds:#{name}"
    file.puts "* #{title_for(tag)}"
    file.puts ""
    file.puts "  - Tags :: [[file:../worlds.org][World Tags]]"
    file.puts ""
    file.puts "  #{tag.fetch('Desc')}"
    ["Complications", "Enemies", "Friends", "Places", "Things"].each do |key|
      file.puts ""
      file.puts "** #{key}"
      file.puts ""
      tag.fetch(key).fetch("Items").each do |item|
        file.puts "   - #{item}"
      end
    end
  end
end

require 'json'
content = File.read(File.expand_path("../../Thel-Sector.json", __FILE__))
json = JSON.parse(content)

json.fetch("Systems").each do |system|
  system_file(system: system)
end
