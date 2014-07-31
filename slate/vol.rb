#!/usr/bin/env ruby

require 'optparse'
#require 'anyplayer'

OptionParser.new do |opts|
  opts.banner = 'Usage: vol [options]'

  opts.on('-u', '--up', 'Turn volume up') do
    `osascript -e 'set volume output volume (output volume of (get volume settings) + 10)'`
  end

  opts.on('-d', '--down', 'Turn volume down') do
    `osascript -e 'set volume output volume (output volume of (get volume settings) - 10)'`
  end

  opts.on('-n', '--next', 'Next song') do
    `curl --data "" http://localhost:9292/player/next`
  end

  opts.on('-p', '--prev', 'Prev song') do
    `curl --data "" http://localhost:9292/player/prev`
  end

  opts.on('', '--play', 'Play music') do
    `curl --data "" http://localhost:9292/player/play`
  end

  opts.on('', '--pause', 'Play music') do
    `curl --data "" http://localhost:9292/player/pause`
  end

  opts.on('', '--darker', 'Darken display') do
    `osascript -e 'tell application "System Events"' -e 'key code 107' -e 'end tell'`
  end

  opts.on('', '--brighter', 'Brighten display') do
    `osascript -e 'tell application "System Events"' -e 'key code 113' -e 'end tell'`
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

