# This file is for moving a previous mixed bag of RPG files into a
# centralized location.

require 'fileutils'
ORIGINAL_BASE_PATH = "/Users/jfriesen/git/org/rpgs/"
class RpgFile
  TARGET_BASE_PATH = "/Users/jfriesen/git/org/projects/thel-sector"
  TIMESTAMP_FILENAME_PREFIX_REGEXP = %r{^\d+-}
  def initialize(filename:)
    @filename = filename
    @content = File.read(filename)
    basename_suffix = File.basename(filename).sub(TIMESTAMP_FILENAME_PREFIX_REGEXP, "")
    basename_prefix = File.ctime(filename).strftime("%Y%m%d")
    @target_basename = "#{basename_prefix}-#{basename_suffix}"
  end

  attr_reader :filename, :content, :target_basename

  def process!(filename_map:)
    move_file!
    update_content!(filename_map: filename_map)
  end

  def move_file!
    FileUtils.mv(filename, target_filename)
  end

  private

  def update_content!(filename_map:)
    lines = [
      "#+title: #{title}".strip,
      "#+roam_tags: #{roam_tags.join(' ')}".strip,
      "* #{title}".strip
    ]
    content.split("\n").each do |line|
      next if line.start_with?("#")
      next if line.start_with?("* ")
      lines << convert_line(line: line, filename_map: filename_map)
    end
    File.open(target_filename, "w+") do |file|
      lines.each do |line|
        file.puts line
      end
    end
  end

  private

  ORG_LINK_FRAGMENT_REGEXP = %r{(file:)([^\]]*)(.*)}
  def convert_line(line:, filename_map:)
    line.split("[[").map do |fragment|
      match = ORG_LINK_FRAGMENT_REGEXP.match(fragment)
      if match
        original_file_path = match[2]
        link_to_filename = File.basename(original_file_path)
        target_filename = filename_map.fetch(link_to_filename, original_file_path)
        "#{match[1]}#{target_filename}#{match[3]}"
      else
        fragment
      end
    end.join("[[")
  end

  def roam_tags
    normalize_tags!(tags: (header_based_tags + path_based_tags))
  end

  HEADER_BASED_ROAM_TITLE_REGEXP = %r{#\+title:([^\n]+)}
  ORG_TITLE_REGEXP = %r{\n\* +([^\n]+)}
  def title
    header_title_match = HEADER_BASED_TAGS_REGEXP.match(content)
    if header_title_match
      return header_title_match[1]
    else
      return ORG_TITLE_REGEXP.match(content)[1]
    end
  end

  def target_filename
    File.join(TARGET_BASE_PATH, target_basename)
  end

  TAG_MAP = {
    "thel_sector" => ["thel", "sectors"],
    "faction_assets" => ["factions", "assets"]
  }
  def normalize_tags!(tags:)
    tags.sort.uniq.map do |tag|
      TAG_MAP.fetch(tag, tag)
    end.flatten.compact
  end

  def path_based_tags
    filename.sub(ORIGINAL_BASE_PATH, "").split("/")[0..-2]
  end

  HEADER_BASED_TAGS_REGEXP = %r{#\+roam_tags:([^\n]+)}

  def header_based_tags
    match = HEADER_BASED_TAGS_REGEXP.match(content)
    return [] unless match

    match[1].strip.split(/ +/)
  end
end




rpg_files = Dir.glob(File.join(ORIGINAL_BASE_PATH, "**/*.org")).map do |filename|
  RpgFile.new(filename: filename)
end
filename_map = {}
rpg_files.each do |rpg|
  original_basename = File.basename(rpg.filename)
  filename_map[original_basename] = rpg.target_basename
end


rpg_files.each do |rpg|
  rpg.process!(filename_map: filename_map)
end
