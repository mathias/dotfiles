#!/usr/bin/env ruby

require 'optparse'

OptionParser.new do |opts|
  opts.banner = 'Usage: vol [options]'

  opts.on('-u', '--up', 'Turn volume up') do |up|
    `osascript -e 'set volume output volume (output volume of (get volume settings) + 10)'`
  end

  opts.on('-d', '--down', 'Turn volume down') do |down|
    `osascript -e 'set volume output volume (output volume of (get volume settings) - 10)'`
  end

  # No argument, shows at tail.  This will print an options summary.
  # Try it and see!
  opts.on_tail("-h", "--help", "Show this message") do
    puts opts
    exit
  end

  # Another typical switch to print the version.
  opts.on_tail("--version", "Show version") do
    puts "0.0.1"
    exit
  end

  if ARGV.empty?
    puts opts
  end
end.parse!

