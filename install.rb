# frozen_string_literal: true

# This script installs the various files from the dotzshrc "ecosystem".
require 'fileutils'
$stdout.puts 'Installing zshrc aliases…'
home_dirname = ENV.fetch('HOME')
symlink_sources = File.expand_path('symlinks/dot.*', __dir__)
Dir.glob(symlink_sources).each do |source_filename|
  basename = File.basename(source_filename)
  target_basename = basename.to_s.sub(/^dot/,'')
  # Create a symlink in HOME directory to source_filename
  target_name = File.join(home_dirname, target_basename.to_s)
  $stdout.puts "\t#{target_name} ->\n\t\t#{source_filename}"
  FileUtils.ln_sf(source_filename, target_name)
end

$stdout.puts 'Finished installing zshrc aliases…'

$stdout.puts 'Installing bin aliases…'
bin_sources = File.expand_path('bin/*', __dir__)
FileUtils.mkdir_p(File.join(home_dirname, 'bin'))
Dir.glob(bin_sources).each do |source_filename|
  # Create a symlink in HOME directory to source_filename
  target_name = File.join(home_dirname, 'bin', File.basename(source_filename))
  $stdout.puts "\t#{target_name} ->\n\t\t#{source_filename}"
  FileUtils.ln_sf(source_filename, target_name)
end
$stdout.puts 'Finished installing bin aliases…'

if RUBY_PLATFORM.include?('darwin')
  $stdout.puts "Installing global git config for darwin"
  system("git config --system --add credential.helper osxkeychain")
  system("git config --system --add interactive.diffFilter `brew --prefix git`/share/git-core/contrib/diff-highlight/diff-highlight")
  system("git config --system --add core.pager `brew --prefix git`/share/git-core/contrib/diff-highlight/diff-highlight | less -F -X")
elsif RUBY_PLATFORM.include?('linux')
  $stdout.puts "Installing global git config for gnu-linux"
  system("git config --system --add credential.helper /usr/lib/git-core/git-credential-libsecret")
  system("git config --system --add interactive.diffFilter /usr/share/git/diff-highlight/diff-highlight")
  system("git config --system --add core.pager /usr/share/git/diff-highlight/diff-highlight | less -F -X")
end