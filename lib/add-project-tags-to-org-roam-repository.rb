# This script handles tagging org file based on their projects.
#
# This is a necessary script as the updated org-roam (v2) does not
# allow for directories to auto-tag files.  This leaves me thinking
# about my file structure.
Dir.glob(File.join(ENV["HOME"], "git/org/projects/**/*.org")).each do |filename|
  tags = filename.split("projects/").last.split("/")[0..-2]
  command = %($EDITOR --eval "(with-current-buffer (find-file \\"#{filename}\\") (org-roam-tag-add '(\\":#{ tags.join(':\\", \\":') }:\\")) (save-buffer) (kill-this-buffer))")
  $stdout.puts "Tagging #{filename}"
  `#{command}`
end
