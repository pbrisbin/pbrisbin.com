module Jekyll
  class Feed < Page
    def initialize(site, base, dir)
      @site = site
      @base = base
      @dir = dir
      @name = 'index.xml'
      self.process(@name)
      self.read_yaml(File.join(base, '_layouts'), 'feed.xml')
    end
  end

  class FeedGenerator < Generator
    safe true

    def generate(site)
      if site.layouts.key? 'feed'
        feed = Feed.new(site, site.source, 'feed')
        feed.render(site.layouts, site.site_payload)
        feed.write(site.dest)
        site.pages << feed
      end
    end

    def write_feed(site, dir)j
    end
  end
end
