module Jekyll
  class TagGenerator < Generator
    safe true

    def generate(site)
      site.tags.each do |tag, posts|
        site.pages << TagPage.new(site, layout(site), tag, posts)
      end
    end

    private

    def layout(site)
      site.config['tag_page_layout'] || 'tag_index'
    end
  end

  class TagPage < Page
    def initialize(site, layout, tag, posts)
      self.data = {
        'tag' => tag,
        'title' => tag,
        'posts' => posts.sort.reverse,
        'layout' => layout
      }

      dir, name = dir_name(site, tag)

      super(site, site.source, dir, name)
    end

    def read_yaml(*)
    end

    private

    def title(site, tag)
      (site.config['tag_page_title'] || ':tag').sub(':tag', tag)
    end

    def dir_name(site, tag)
      permalink = site.config['tag_page_permalink'] || '/tags/:tag/'

      if permalink.end_with?('/')
        [permalink.sub(':tag', tag), 'index.html']
      else
        File.split("#{permalink.sub(':tag', tag)}.html")
      end
    end
  end
end
