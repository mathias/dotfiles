Pry::Commands.create_command "html5tidy" do
  description "Print indented, colorized HTML from the input: html5tidy [HTML & CSS-SELECTORS]"

  command_options requires_gem: ['nokogiri']

  def process
    @object_to_interrogate = args.empty? ? target_self : target.eval(args.first)
    cleaned_html = Nokogiri::XML(@object_to_interrogate,&:noblanks)

    if args.count > 1
      selector = args[1]
      cleaned_html = cleaned_html.css(selector)
    end

    colorized_text = Pry.config.color ? CodeRay.scan(cleaned_html, :html).term : cleaned_html
    output.puts colorized_text
  end
end
