require 'fileutils'
$stdout.puts "Installing zshrc aliases…"

home_dirname = ENV.fetch('HOME')
symlink_sources = File.expand_path("../symlinks/*.symlink", __FILE__)
Dir.glob(symlink_sources).each do |source_filename|
  # Create a symlink in HOME directory to source_filename
  target_name = File.join(home_dirname, ".#{File.basename(source_filename, '.symlink')}")
  $stdout.puts "\t#{target_name} ->\n\t\t#{source_filename}"
  FileUtils.ln_sf(source_filename, target_name)
end
$stdout.puts "Finished installing zshrc aliases…"
