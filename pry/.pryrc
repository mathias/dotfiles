Pry::Commands.create_command "html5tidy" do
  description "Print indented, colorized HTML from the input: html5tidy [ARGS]"

  command_options requires_gem: ['nokogiri']

  def process
    @object_to_interrogate = args.empty? ? target_self : target.eval(args.join(" "))
    cleaned_html = Nokogiri::XML(@object_to_interrogate,&:noblanks)

    colorized_text = Pry.config.color ? CodeRay.scan(cleaned_html, :html).term : cleaned_html
    output.puts colorized_text
  end
end
