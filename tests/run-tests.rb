#!/usr/bin/env ruby

if ARGV.empty?
  collectors = ['../collectors/batch-profile.rkt', '../collectors/incremental-profile.rkt']
else
  collectors = ARGV
end

collectors.each do |c|
  `cp #{c} collector.rkt 2> /dev/null`

  `ls test-*.rkt`.each_line do |f|
    f = f.chop
    p f
    `racket #{f} > /dev/null`
  end
end
