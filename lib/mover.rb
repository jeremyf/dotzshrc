require 'fileutils'
BASE_URL = "/Users/jfriesen/git/org/"
LAST_SLUG = "rpgs"
BASE_SLUGS = ""
STRUCTURE = File.join(BASE_SLUGS, LAST_SLUG)
slug_filename = File.join(BASE_URL, BASE_SLUGS, "#{LAST_SLUG}.org")
if !File.exist?(slug_filename)
  File.open(slug_filename, "w+") do |file|
    file.puts "#+roam_key: #{BASE_SLUGS.gsub('/', '-').sub(/^-/, '')}:#{LAST_SLUG}"
    file.puts "* #{LAST_SLUG}"
  end
end

key = STRUCTURE.gsub("/","-").sub(/^-/, '')
[

  "notes/20200805114752-witchburner.org",
  "notes/20200812194709-generating_thel_sector_map.org",
  "notes/20200813154630-sine_nomine.org",
  "notes/20200813160723-soft_horizon.org",
  "notes/20200813160841-conflict_risks.org",
  "notes/20200813164420-watch.org",
  "notes/20200813165526-heat.org",
  "notes/20200813165601-blades_in_the_dark.org",
].each do |filename|
  basename = File.basename(filename)
  name = basename.sub(".org", "")
  source = File.join(BASE_URL, filename)
  target = File.join(BASE_URL, STRUCTURE, basename)
  FileUtils.mkdir_p(File.join(BASE_URL, STRUCTURE))
  FileUtils.mv(source, target)
  content = File.read(target)
  lines = content.split("\n")
  while lines[0] =~ /^#/
    lines.shift
  end
  lines.unshift("#+roam_key: #{key}:#{name}")
  File.open(target, "w+") do |file|
    file.puts lines.join("\n")
  end
end
