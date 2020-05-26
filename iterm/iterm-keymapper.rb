# Starting with CTRL+CMD+a
# From '61' to '7a'

#   And a to z
entries = []
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
  entries << %(  "0x#{hex}-0x1c0000":{"Text":"[1;P#{index+9}", "Action": 10 })
  entries << %(  "0x#{hex}-0xc0000":{"Text":"[1;P#{index+35}", "Action": 10 })
end
puts %({"Key Mappings": {)
puts entries.sort.join(",\n")
puts "}}"
