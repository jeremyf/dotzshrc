require 'json'

lines = [
  "#+title: Report: Overview of the Thel Sector",
  "#+author: New Vistas Survey Team",
  "#+date:",
  "#+roam_tags:",
  "* Report: Overview of the Thel Sector",
  "",
  "  The following report, generated 3200.10.05, contains a high-level overview of the Thel Sector.  We've summarized our asseessment of each system based on operative intelligence.",
  "",
  "  #+CAPTION: Players Map of the Thel Sector",
  "  #+ATTR_LATEX: :caption \bicaption{Players Map of the Thel Sector}",
  "  #+ATTR_LATEX: :width 7in",
  "  [[./assets/thel-sector-player-map.png]]",
]

class System
  SYSTEMS_TO_SKIP = ["Askira"]
  WARNINGS = ["Umbrete", "Hubar", "Yuki", "Uman", "Omsk"]
  SYSTEMS_TO_ONLY_LIST_PLANETS = ["Suwa", "Ramlah", "Saratov", "Tinpahar", "Zigong", "Itako", "Gansu"]
  def initialize(json:)
    @json = json
  end
  attr_reader :json

  def as_lines
    return [] if SYSTEMS_TO_SKIP.include?(name)
    [
      %(*** #{name} (#{coordinates}))
    ] + warnings + worlds_as_lines
  end
  def warnings
    return [] unless WARNINGS.include?(name)
    [
      "",
      "    *New Vistas recommends caution traveling to this system.*",
      "",
    ]
  end
  def worlds_as_lines
    lines = []
    json.fetch("Worlds").each do |world|
      lines << "**** #{world.fetch('Name')}"
      if ! SYSTEMS_TO_ONLY_LIST_PLANETS.include?(name)
        lines += tags(world: world)
        ["Atmosphere", "Temperature", "Biosphere", "Population"].each do |key|
          lines << "     - #{key}: #{world.fetch(key)}"
        end
        lines << "     - Tech Level: #{world.fetch('TechLevel')}"
      end
    end
    lines
  end

  SKIP_TAGS = ["Pretech Cultists", "Battleground", "Friendly Foe", "Immortals", "Taboo Treasure"]
  def tags(world:)
    tags = []
    world.fetch("Tags").each do |tag|
      tags << tag.fetch("Name") unless SKIP_TAGS.include?(tag.fetch("Name"))
    end
    tags
  end

  def name
    json.fetch("Name")
  end

  def coordinates(traveller: true)
    if traveller
      return sprintf("%02d%02d", json.fetch('Col')+1, json.fetch('Row')+1)
    else
      # For swnt format
      return "(#{json.fetch('Row')}, #{json.fetch('Col')})"
    end
  end
end

lines << ""
lines << "** Systems"
lines << ""

json = JSON.parse(File.read(File.expand_path("../data/thel-sector.json", __dir__)))
json.fetch("Systems").sort {|a,b| a["Name"] <=> b["Name"] }.each do |system_as_json|
  lines += System.new(json: system_as_json).as_lines
end

puts lines.join("\n")
