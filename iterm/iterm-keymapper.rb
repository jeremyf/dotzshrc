# This script creates an iTerm keymap, which allows me to map the
# Hyper(H) and Super(s) keys. Thus freeing up a massive number of
# keys.
#
CTRL_OPT_CMD = '0x1c0000'.freeze
CTRL_OPT = '0xc0000'.freeze
keymap_entries = []
[
  %w[61 a],
  %w[62 b],
  %w[63 c],
  %w[64 d],
  %w[65 e],
  %w[66 f],
  %w[67 g],
  %w[68 h],
  %w[69 i],
  %w[6a j],
  %w[6b k],
  %w[6c l],
  %w[6d m],
  %w[6e n],
  %w[6f o],
  %w[70 p],
  %w[71 q],
  %w[72 r],
  %w[73 s],
  %w[74 t],
  %w[75 u],
  %w[76 v],
  %w[77 w],
  %w[78 x],
  %w[79 y],
  %w[7a z]
].each_with_index do |(hex, _key), index|
  keymap_entries << %(  "0x#{hex}-#{CTRL_OPT_CMD}":{"Text":"[1;P#{index + 9}", "Action": 10 })
  keymap_entries << %(  "0x#{hex}-#{CTRL_OPT}":{"Text":"[1;P#{index + 35}", "Action": 10 })
end

# CTRL+OPT+/
keymap_entries << %(  "0x2f-#{CTRL_OPT}":{"Text":"[1;P70", "Action": 10})
# CTRL+OPT+.
keymap_entries << %(  "0x2e-#{CTRL_OPT}":{"Text":"[1;P71", "Action": 10})
# CTRL+OPT+,
keymap_entries << %(  "0x2c-#{CTRL_OPT}":{"Text":"[1;P72", "Action": 10})
# CTRL+OPT+SPC
keymap_entries << %(  "0x20-#{CTRL_OPT}": {"Text":"[1;P74", "Action": 10})

filename = File.expand_path('emacs.itermkeymap', __dir__)

File.open(filename, 'w+') do |file|
  file.puts %({"Key Mappings": {)
  file.puts keymap_entries.sort.join(",\n")
  file.puts '}}'
end
