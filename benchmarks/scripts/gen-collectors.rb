#!/usr/bin/env ruby

if ARGV.empty?
  collectors = ['../collectors/batch-profile.rkt', '../collectors/incremental-profile.rkt', '../collectors/incremental2-profile.rkt']
else
  collectors = ARGV
end

`rm collectors/* 2> /dev/null`
(3..10).each do |i|
  collectors.each do |c|
    ratio = i+1
    name = "%s-%d.rkt" % [c.split('/')[-1].split('-')[0], i]
    `sed "s/1\\/4/1\\/#{ratio}/g" #{c} > collectors/#{name}`
  end
end
