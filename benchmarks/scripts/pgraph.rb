#!/usr/bin/env ruby
# used to produce benchmark graphs
# usage : ./pgraph.rb [-a] [-c=collector-name[, ...]] [-b=bench-name[,...]]

# default materials
collectors = ['batch', 'incremental']
benchs = `ls mutators/*.rkt`.split(/\n/).map { |b| b.split('.')[0] }

# argv parse
#
# collectors : [listof name w/o ext]
# benchs : [listof path w/o ext]
ARGV.each do |arg|
  case arg
  when '-a'
  when /^-c=/
    collectors = arg.split('=')[1].split(',')
  when /^-b=/
    benchs = arg.split('=')[1].split(',')
    benchs = benchs.map { |b| "mutators/%s" % b }
  else
    puts "Unknown Arguments #{arg}"
  end
end

# generate graphs
#
# if collectors are more than 1 produce both solo & comparison graphs
#
# else only produce solo graphs
(3..10).each do |i|
  benchs.each do |b|
    fs = collectors.map { |c| "#{b}/#{c}-#{i}.rktd" }
    fs.each do |f|
      puts f
      `racket plot-graphs.rkt #{f}`
    end
    
    if collectors.size > 1
      fs.combination(2).each do |a, b|
        puts "#{a} & #{b}"
        `racket plot-graphs.rkt #{a} #{b}`
      end
    end
  end
end
