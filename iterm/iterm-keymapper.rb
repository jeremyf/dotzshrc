CTRL_OPT_CMD = "0x1c0000"
CTRL_OPT = "0xc0000"
keymap_entries = []
[
  ["61", "a"],
  ["62", "b"],
  ["63", "c"],
  ["64", "d"],
  ["65", "e"],
  ["66", "f"],
  ["67", "g"],
  ["68", "h"],
  ["69", "i"],
  ["6a", "j"],
  ["6b", "k"],
  ["6c", "l"],
  ["6d", "m"],
  ["6e", "n"],
  ["6f", "o"],
  ["70", "p"],
  ["71", "q"],
  ["72", "r"],
  ["73", "s"],
  ["74", "t"],
  ["75", "u"],
  ["76", "v"],
  ["77", "w"],
  ["78", "x"],
  ["79", "y"],
  ["7a", "z"]
].each_with_index do |(hex, key), index|
  keymap_entries << %(  "0x#{hex}-#{CTRL_OPT_CMD}":{"Text":"[1;P#{index+9}", "Action": 10 })
  keymap_entries << %(  "0x#{hex}-#{CTRL_OPT}":{"Text":"[1;P#{index+35}", "Action": 10 })
end

# CTRL+OPT+/
keymap_entries << %(  "0x2f-#{CTRL_OPT}":{"Text":"[1;P70", "Action": 10})
# CTRL+OPT+.
keymap_entries << %(  "0x2e-#{CTRL_OPT}":{"Text":"[1;P71", "Action": 10})
# CTRL+OPT+,
keymap_entries << %(  "0x2c-#{CTRL_OPT}":{"Text":"[1;P72", "Action": 10})

File.open(File.join(ENV["HOME"], "git/dotzshrc/emacs.itermkeymap"), "w+") do |file|
  file.puts %({"Key Mappings": {)
  file.puts keymap_entries.sort.join(",\n")
  file.puts "}}"
end
