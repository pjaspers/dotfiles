#!/usr/bin/ruby

=begin

  This is a simple script for printing out the methods in your Objective-C .h or .m file. It reads the passed file and stores just the method names in a text file

=end

# Default filename and type for the file output
DEFAULT_FILENAME = "outfile"
DEFAULT_EXTENSION = "txt"

# Dependiencies
require 'rubygems'

# This requires the 'colorize' gem. Install with '[sudo] gem install colorize'
require 'colorize'

# This requires the 'launchy' gem. Install with '[sudo] gem install launchy'
require 'launchy'

def printhelp
  puts "Usage: methods [filename]\nThis script will then create a file named #{ $defaultfilename } with a list of methods in the passed file".yellow
end

if ARGV.count == 0
  puts "You must enter a filename to read. Use -help for instructions".red
  exit
end

if ARGV[0] == "-h" or ARGV[0] == "-help"
  printhelp
  exit
end

filename = ARGV[0]
unless File.exists?(filename)
  puts "File named: #{ filename } doesn't exist".red
  exit
end

unless File.readable?(filename)
  puts "File named: #{ filename } doesn't exist".red
  exit
end

lines = File.open(filename, 'r').readlines
methods = lines.select{|line| line =~ /^(-|\+)/}

outfilename = "#{ DEFAULT_FILENAME }.#{ DEFAULT_EXTENSION }"

filenamecount = 0
while File.exists?(outfilename)
  filenamecount += 1
  outfilename = "%s_%d.%s" % [DEFAULT_FILENAME, filenamecount, DEFAULT_EXTENSION]
end

outfile = File.open(outfilename, "w") do |file|
  file << methods
end

puts "Wrote #{ methods.size } methods/functions to #{ outfilename }".green
Launchy.open(outfilename)
