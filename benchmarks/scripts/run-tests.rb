#!/usr/bin/env ruby

collectors = ['batch', 'incremental', 'incremental2']
benchs = []
`ls mutators/*.rkt`.each_line do |f|
  benchs.push f.chop
end

ARGV.each do |arg|
  case arg
  when /^-c=/
    collectors = arg.split('=')[1].split(',')
  when /^-b=/
    benchs = arg.split('=')[1].split(',')
    benchs = benchs.map { |b| "mutators/#{b}.rkt" }
  else
    puts "Unknown Argument #{arg}"
  end
end

collectors.each do |c|
  `ls collectors/* | grep "#{c}-"`.each_line do |cl|
    cl = cl.chop
    `cp #{cl} collector.rkt 2> /dev/null`
    puts "### #{cl} ###"
    puts ""
    
    benchs.each do |b|
      puts b
      `racket #{b} > /dev/null`
    end
    puts ""
  end
end
