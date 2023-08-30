require 'set'
require 'psych'
require 'byebug'

# The goal of this module is to convert author and work metadata to bibliographic citations (Bibtex
# format)
module ConvertGlossaryToBibtex
  Entry = Struct.new(:author, :title, keyword_init: true) do
    def to_bib
      lines = [%(@Book{#{key},)]
      lines << %(  title = {#{title.strip}},) if title?
      lines << %(  author = {#{author.strip}}) if author?
      lines << "}"
      lines.join("\n")
      end

    def key
      akey = author? ? author.to_s.split(/\ +/)[-1] : "null"
      tkey = title? ? title.to_s.split(/\ +/)[0..3].join("-") : "null"
      (akey + "_" + tkey).downcase.gsub(%r{[^\w|\-|_]}, '')
    rescue => e
      byebug
    end

    def author?
      !author.to_s.strip.size.zero?
    end

    def title?
      !title.to_s.strip.size.zero?
    end
  end

  class Glossary
    def initialize(filename: File.join(ENV['HOME'], "/git/takeonrules.source/data/glossary.yml"))
      @filename = filename
      @data = Psych.load_file(filename)
    end

    def title_for(key:)
      fetch(key).fetch("title")
    rescue NoMethodError => e
      byebug
      raise e
    end

    private

    def fetch(key)
      @data.find { |datum| datum['key'].upcase == key.upcase }
    end
  end

  class Bibliography
    def initialize(&block)
      @entries = Set.new
      @glossary = Glossary.new
      instance_exec(&block)
      @entries.freeze
    end

    attr_reader :entries

    def to_bib
      @entries.map(&:to_bib).join("\n")
    end

    protected

    AUTHOR_KEY_REGEXP = %r{^#\+AUTHOR_KEY: +(.*)$}i.freeze
    WORK_KEY_REGEXP = %r{^#\+WORK_KEY: +(.*)$}i.freeze
    AUTHOR_NAME_REGEXP = %r{^#\+AUTHOR_NAME: +(.*)$}i.freeze
    WORK_TITLE_REGEXP = %r{^#\+WORK_TITLE: +(.*)$}i.freeze

    def add(filename:)
      content = File.read(filename)
      author = author_from(content)
      title = title_from(content)
      @entries << Entry.new(author: author, title: title)
    end

    private

    def author_from(content)
      if author_key_match = AUTHOR_KEY_REGEXP.match(content)
        @glossary.title_for(key: author_key_match[1])
      else
        AUTHOR_NAME_REGEXP.match(content)&.[](1)
      end
    end

    def title_from(content)
      if work_key_match = WORK_KEY_REGEXP.match(content)
        @glossary.title_for(key: work_key_match[1])
      else
        WORK_TITLE_REGEXP.match(content)&.[](1)
      end
    end
  end

  def self.convert(dir: File.join(ENV['HOME'], "/git/org"))
    bibliography = Bibliography.new do
      `rg "^#\\+WORK_(TITLE|KEY): +(.*)$" #{dir} -i --files-with-matches`.split("\n").each do |filename|
        add(filename: filename)
      end
    end.to_bib
  end
end

puts ConvertGlossaryToBibtex.convert