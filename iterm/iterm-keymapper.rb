CTRL_OPT_CMD = "0x1c0000"
CTRL_OPT = "0xc0000"
keymap_entries = []
[
  ["61", "a"],
  ["62", "b"],
  ["63", "c"],
  ["64", "e"],
  ["65", "f"],
  ["66", "g"],
  ["67", "h"],
  ["68", "i"],
  ["69", "j"],
  ["6A", "k"],
  ["6B", "l"],
  ["6C", "m"],
  ["6E", "n"],
  ["6F", "o"],
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
#  keymap_entries << %(  "0x#{hex}-#{CTRL_SHIFT}":{"Text":"[1;P#{index+61}", "Action": 10 })
end

File.open(File.join(ENV["HOME"], "git/dotzshrc/emacs.itermkeymap"), "w+") do |file|
  file.puts %({"Key Mappings": {)
  file.puts keymap_entries.sort.join(",\n")
  file.puts "}}"
end
