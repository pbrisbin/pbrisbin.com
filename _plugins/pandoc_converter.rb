module Jekyll
  class PandocConverter < Converter
    FORMATS = {
      ".md"  => "markdown",
      ".lhs" => "markdown+lhs"
    }

    safe true
    priority :high

    def matches(ext)
      @format = FORMATS[ext]
    end

    def output_ext(*)
      ".html"
    end

    def convert(content)
      IO.popen("pandoc --from=#{@format} --to=html", 'r+') do |h|
        h.puts(content)
        h.close_write

        h.read
      end
    end
  end
end
