# A quick script to fix misplaced attachments

require 'pathname'
ORG_DIRECTORY = Pathname.new(File.join(ENV['HOME'], "git", "org"))
class Attachment
  AG_RESULT_REGEXP = %r{^(?<source_filename>[^:]+):(?<line_number>\d+)::ID: +(?<id_prefix>[\w\d-]{2})(?<id_suffix>[\w\d-]+)}
  def initialize(line:)
    @line = line
    match = AG_RESULT_REGEXP.match(line)
    @source_path = Pathname.new(match[:source_filename])
    @line_number = match[:line_number]
    @id_prefix = match[:id_prefix]
    @id_suffix = match[:id_suffix]
  end
  attr_reader :line, :source_path, :line_number, :id_prefix, :id_suffix
end

require 'fileutils'
mover = FileUtils

list_org_ids_command = [
  "ag",
  "--only-matching",
  "--nocolor",
  "--noheading",
  '":ID: +[\w\d-]+"',
  ORG_DIRECTORY
]
`#{list_org_ids_command.join(' ')}`.split("\n") do |line|
  line.strip!
  next if line.empty?
  attachment = Attachment.new(line: line)
  glob =  ORG_DIRECTORY.glob(File.join("**", attachment.id_prefix, attachment.id_suffix, "**","*"))
  glob.each do |attached_pathname|
    next if attached_pathname.to_s.start_with?(attachment.source_path.dirname.join("data").to_s)
    target_dir_pathname = attachment.source_path.dirname.join("data", attachment.id_prefix, attachment.id_suffix)
    mover.mkdir_p(target_dir_pathname)
    mover.mv(attached_pathname, target_dir_pathname)
  end
end
