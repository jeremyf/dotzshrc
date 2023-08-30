require 'json'
content = File.read(File.expand_path("../Thel-Sector.json", __FILE__))
json = JSON.parse(content)

json.fetch("Systems").each do |system|
  lines = []
  lines << "<html>"
  lines << "<head>"
  lines << "<title>#{system.fetch("Name")} (#{system.fetch("Row")},#{system.fetch("Col")})</title>"
  lines << "</head>"
  lines << "<h1>#{system.fetch("Name")}</h1>"
  lines << ""
  lines << "<dl>"
  lines << "<dt>Hex</dt>"
  lines << "<dd>(#{system.fetch("Row")},#{system.fetch("Col")})</dd>"
  lines << "</dl>"
  lines << ""
  lines << "<h2>Worlds</h2>"
  lines << ""
  worlds = system.fetch("Worlds")
  lines << "<ul>"
  worlds.each do |world|
    lines << "<li><a href='#dom-id-#{world.fetch('Name').downcase}'>Skip to #{world.fetch('Name')}</a></li>"
  end
  lines << "</ul>"
  lines << ""
  worlds.each do |world|
    world_name = world.fetch("Name")
    world_name += " <small>(Primary)</small>" if world.fetch("Primary")

    lines << "<h3 id='dom-id-#{world.fetch("Name").downcase}'>#{world_name}</h3>"
    lines << ""
    lines << "<dl>"
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
      lines << "<dt>#{key}</dt><dd>#{world.fetch(key)}</dd>" unless world.fetch(key).empty?
    end
    lines << "</dl>"
    lines << ""
    world.fetch("Tags").each do |tag|
      lines << "<h4>Tag: #{tag.fetch("Name")}</h4>"
      lines << ""
      lines << "<p>#{tag.fetch("Desc")}</p>"
      lines << "<dl>"
      ["Complications", "Enemies", "Friends", "Places", "Things"].each do |key|
        lines << "<dt>#{key}</dt><dd><ul>"
        tag.fetch(key).fetch("Items").each do |item|
          lines << "<li>#{item}</li>"
        end
        lines << "</ul></dd>"
      end
      lines << "</dl>"
    end
  end

  if pois = system.fetch("POIs")
    lines << ""
    lines << "<h2>Points of Interest</h2>"
    lines << ""
    pois.each do |poi|
      lines << "<dl>"
      poi.keys.each do |key|
        lines << "<dt>#{key}</dt><dd>#{poi.fetch(key)}</dd>"
      end
      lines << "</dl>"
    end
  end
  lines << "</html>"

  File.open(File.expand_path("../html/#{system.fetch("Name").downcase}.html", __FILE__), "w+") do |f|
    f.puts lines.join("\n")
  end
end
