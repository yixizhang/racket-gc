#!/usr/bin/env ruby
# used for racket GC benchmark
# usage : ./rbench.rb [-a] [-c=collector-name[, ...]] [-b=bench-name[,...]]

# default materials
collectors = ['batch', 'incremental', 'incremental2']
benchs = `ls basic/*.rkt`.split(/\n/).map { |b| b.split('.')[0] }

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
    benchs = benchs.map { |b| "basic/%s" % b }
  else
    puts "Unknown Arguments #{arg}"
  end
end

# run benchmarks
benchs.each { |b| `mkdir -p #{b} 2> /dev/null` }
(3..10).each do |i|
  collectors.each do |c|
    `cp collectors/#{c}-#{i}.rkt collector.rkt 2> /dev/null`
    benchs.each do |b|
      puts "#{c} : #{i} -> #{b}"
      `racket runner.rkt #{c} #{b} #{i}`
    end
  end
end