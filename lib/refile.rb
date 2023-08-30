path = File.expand_path( "../rpgs" , __dir__)
lookup = {}
Dir.glob(File.join(path, "**/*.org")).each do |filename|
  basename = File.basename(filename)
  lookup[basename] = filename
end

FILE_REGEXP = %r{\[\[file:([^\]]*)\]\[}

Dir.glob(File.join(path, "**/*.org")).each do |filename|
  lines = []
  File.read(filename).split("\n").each do |line|
    if match = FILE_REGEXP.match(line)
      linked_filename = match[1]
      require 'byebug'; debugger; true
      :debugger
    end
  end
end
