# coding: utf-8
# This script installs the various files from the dotzshrc "ecosystem".
require 'fileutils'
$stdout.puts "Installing zshrc aliases…"
home_dirname = ENV.fetch('HOME')
symlink_sources = File.expand_path("../symlinks/.*", __FILE__)
Dir.glob(symlink_sources).each do |source_filename|
  basename = File.basename(source_filename)
  next if basename == '.'
  next if basename == '..'
  # Create a symlink in HOME directory to source_filename
  target_name = File.join(home_dirname, "#{basename}")
  $stdout.puts "\t#{target_name} ->\n\t\t#{source_filename}"
  FileUtils.ln_sf(source_filename, target_name)
end
$stdout.puts "Finished installing zshrc aliases…"

$stdout.puts "Installing bin aliases…"
bin_sources = File.expand_path("../bin/*", __FILE__)
FileUtils.mkdir_p(File.join(home_dirname, "bin"))
Dir.glob(bin_sources).each do |source_filename|
  # Create a symlink in HOME directory to source_filename
  target_name = File.join(home_dirname, "bin", File.basename(source_filename))
  $stdout.puts "\t#{target_name} ->\n\t\t#{source_filename}"
  FileUtils.ln_sf(source_filename, target_name)
end
$stdout.puts "Finished installing bin aliases…"
