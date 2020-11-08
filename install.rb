# coding: utf-8
# frozen_string_literal: true

# This script installs the various files from the dotzshrc "ecosystem".
require 'fileutils'
$stdout.puts 'Installing zshrc aliases…'
home_dirname = ENV.fetch('HOME')
symlink_sources = File.expand_path('symlinks/.*', __dir__)
Dir.glob(symlink_sources).each do |source_filename|
  basename = File.basename(source_filename)
  next if basename == '.'
  next if basename == '..'
  target_basename = basename.to_s
  # Create a symlink in HOME directory to source_filename
  target_name = File.join(home_dirname, target_basename)
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

$stdout.puts 'Installing emacs.d symlinks…'
FileUtils.mkdir_p(File.join(home_dirname, '.emacs.d'))

[
  File.expand_path('emacs.d/.*', __dir__), # Hidden files
  File.expand_path('emacs.d/*', __dir__)  # Non-hidden files
].each do |glob|
  Dir.glob(glob).each do |source_filename|
    basename = File.basename(source_filename)
    next if basename == '.'
    next if basename == '..'

    target_basename = basename.to_s
    # Create a symlink in HOME directory to source_filename
    target_name = File.join(home_dirname, '.emacs.d', target_basename)
    $stdout.puts "\t#{target_name} ->\n\t\t#{source_filename}"
    FileUtils.ln_sf(source_filename, target_name)
  end
end
$stdout.puts 'Finished installing .emacs.d aliases…'

platform = `uname`.strip.downcase

if platform =~ /darwin/
  $stdout.puts "Installing global git config for darwin"
  unless system("git config --system --get credential.helper")
    system("git config --system --add credential.helper osxkeychain")
  end
  unless system("git config --system --get interactive.diffFilter")
    system("git config --system --add interactive.diffFilter \"`brew --prefix git`/share/git-core/contrib/diff-highlight/diff-highlight\"")
  end
  unless system("git config --system --get core.pager")
    system("git config --system --add core.pager \"`brew --prefix git`/share/git-core/contrib/diff-highlight/diff-highlight | less -F -X\"")
  end
elsif platform =~ /linux/
  $stdout.puts "Installing global git config for gnu-linux"
  $stdout.puts(%(Run "sudo git config --system --add credential.helper /usr/lib/git-core/git-credential-libsecret"))
  $stdout.puts(%(Run "sudo git config --system --add interactive.diffFilter /usr/share/git/diff-highlight/diff-highlight"))
  $stdout.puts(%(Run "sudo git config --system --add core.pager \"/usr/share/git/diff-highlight/diff-highlight | less -F -X\""))
end
