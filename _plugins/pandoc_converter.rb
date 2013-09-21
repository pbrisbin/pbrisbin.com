module Jekyll
  class PandocConverter < Converter
    safe true
    priority :high

    def matches(ext)
      ext =~ /^\.md$/i
    end

    def output_ext(*)
      ".html"
    end

    def convert(content)
      IO.popen('pandoc --from=markdown --to=html', 'r+') do |h|
        h.puts(content)
        h.close_write

        h.read
      end
    end
  end
end
