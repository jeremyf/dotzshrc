# coding: utf-8
# frozen_string_literal: true


# This script installs the various files from the dotzshrc "ecosystem".
require 'fileutils'
$stdout.puts 'Installing zshrc aliases…'
home_dirname = ENV.fetch('HOME')

symlink_sources = File.expand_path('symlinks/dot.*', __dir__)
Dir.glob(symlink_sources).each do |source_filename|
  basename = File.basename(source_filename)
  next if basename == '.'
  next if basename == '..'

  target_basename = basename.to_s.sub("dot.", ".")
  # Create a symlink in HOME directory to source_filename
  target_name = File.join(home_dirname, target_basename)
  $stdout.puts "\t#{target_name} ->\n\t\t#{source_filename}"
  FileUtils.ln_sf(source_filename, target_name)
end

## Zed's kind of nice but not enough

# zed_sources = File.expand_path("dot.config/zed/*.*", __dir__)

# Dir.glob(zed_sources).each do |source_filename|
#   basename = File.basename(source_filename)
#   next if basename == '.'
#   next if basename == '..'

#   FileUtils.mkdir_p(File.join(home_dirname, ".config/zed/"))
#   target_basename = basename.to_s.sub("dot.config", ".config")
#   target_name = File.join(home_dirname, ".config/zed/", target_basename)
#   $stdout.puts "\t#{target_name} ->\n\t\t#{source_filename}"
#   FileUtils.ln_sf(source_filename, target_name)
# end

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

{
  "emacs" => "/opt/homebrew/opt/emacs-plus@29/bin/emacs",
  "emacsclient" => "/opt/homebrew/opt/emacs-plus@29/bin/emacsclient"
}.each do |basename, source_filename|
  target_name = File.join(home_dirname, 'bin', basename)
  $stdout.puts "\t#{target_name} ->\n\t\t#{source_filename}"
  FileUtils.ln_sf(source_filename, target_name)
end

$stdout.puts 'Installing emacs.d symlinks…'
FileUtils.mkdir_p(File.join(home_dirname, '.emacs.d'))

[
  File.expand_path(File.join(home_dirname, 'git/dotemacs/emacs.d/.*')), # Hidden files
  File.expand_path(File.join(home_dirname, 'git/dotemacs/emacs.d/*')), # Non-hidden files
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

denote_dir = File.join(home_dirname, "git/org/denote")
FileUtils.mkdir_p(denote_dir)
denote_domains = {
  "blog-posts" => "local",
  "epigraphs" => "shared",
  "glossary" => "shared",
  "indices" => "local",
  "melange" => "local",
  "people" => "local",
  "private" => "private",
  "references" => "shared",
}

denote_domains.each do |domain, type|
  target_name = File.join(denote_dir, domain)
  source_name = case type
                when "local"
                  File.join(home_dirname, "git/pkm-local/#{domain}")
                when "shared"
                  File.join(home_dirname, "git/pkm-shared/#{domain}")
                when "private"
                  File.join(home_dirname, "Documents/denote-private")
                end



  if File.exist?(source_name)
    if File.exist?(target_name)
      $stdout.puts "Skipping #{source_name}; #{target_name} already exists"
    else
      $stdout.puts "\t#{target_name} ->\n\t\t#{source_name}"
      if domain == "epigraphs"
        require 'debug'; binding.break
      end
      FileUtils.ln_sf(source_name, target_name)
    end
  else
    $stdout.puts "Skipping #{source_name}; it does not exist"
  end
end

platform = `uname`.strip.downcase

if platform =~ /darwin/
  $stdout.puts "Installing karabiner elements (you'll need to enable them)"
  target_filename = File.join(home_dirname, ".config/karabiner/assets/complex_modifications/modifications.json")
  FileUtils.mkdir_p(File.basename(target_filename))
  source_filename = File.join(home_dirname, "git/dotzshrc/karabiner/modifications.json")
  FileUtils.cp(source_filename, target_filename)

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

  system("source $HOME/git/dotzshrc/configs/darwin-defaults.zsh")
elsif platform =~ /linux/
  $stdout.puts "Installing global git config for gnu-linux"
  $stdout.puts(%(Run "sudo git config --system --add credential.helper /usr/lib/git-core/git-credential-libsecret"))
  $stdout.puts(%(Run "sudo git config --system --add interactive.diffFilter /usr/share/git/diff-highlight/diff-highlight"))
  $stdout.puts(%(Run "sudo git config --system --add core.pager \"/usr/share/git/diff-highlight/diff-highlight | less -F -X\""))
end
