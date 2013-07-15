#!/usr/bin/env ruby

if ARGV.empty?
  collectors = ['batch', 'incremental', 'incremental2']
else
  collectors = ARGV
end

collectors.each do |c|
  `ls ../benchmarks/collectors/*.rkt | grep "#{c}-"`.each_line do |cl|
    cl = cl.chop
    `cp #{cl} collector.rkt 2> /dev/null`
    puts "### #{cl} ###"
    puts ""

    `ls *.rkt | egrep -v "cache|collector|util\.rkt"`.each_line do |f|
      f = f.chop
      puts f
      `racket #{f} > /dev/null`
    end
    puts ""
  end
end
