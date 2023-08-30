#!/usr/bin/env ruby -w
require 'pathname'
org_directory = Pathname.new(File.join(ENV["HOME"], "git/org"))

DESIRED_SEPARATOR = "---"
NO_DESIRED_SEPARATOR_REGEXP = %r{^(?<prefix>[^-]+)-(?<slug>[^-]+\.org)$}
org_directory.glob("**/*.org").each do |filename|
  file_path = Pathname.new(filename)
  match = NO_DESIRED_SEPARATOR_REGEXP.match(file_path.basename.to_s)
  next if match.nil?

  command = [
    "org-file-mover",
    "-f", file_path.to_s,
    "-t", file_path.dirname.to_s,
    "-r", "#{match[:prefix]}#{DESIRED_SEPARATOR}#{match[:slug]}",
    "-d"
  ]
  system(command.join(" "))
end
