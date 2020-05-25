require 'fileutils'
$stdout.puts "Installing zshrc aliases…"
home_dirname = ENV.fetch('HOME')
symlink_sources = File.expand_path("../symlinks/.*.symlink", __FILE__)
Dir.glob(symlink_sources).each do |source_filename|
  # Create a symlink in HOME directory to source_filename
  target_name = File.join(home_dirname, "#{File.basename(source_filename, '.symlink')}")
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

$stdout.puts "Installing VS Code symlinks"
vscode = File.expand_path("../vscode/*", __FILE__)
target_directory = File.join(home_dirname, "Library/Application\ Support/Code/User")
Dir.glob(vscode).each do |source_filename|
  target_name = File.join(target_directory, File.basename(source_filename))
  if File.directory?(source_filename)
    # Without this line, the source directory will be linked as a sub-directory
    # of the target. Which really isn't what we want.
    FileUtils.rm_rf(target_name)
  end
  $stdout.puts "\t#{target_name} ->\n\t\t#{source_filename}"
  FileUtils.ln_sf(source_filename, target_name)
end
$stdout.puts "Finished installing VS Code symlinks…"
