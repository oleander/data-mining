#!/usr/bin/env ruby

Dir["*.dot"].each do |dot|
  f = File.basename(dot, ".dot")
  puts `dot -T png #{dot} > #{f}.png`
  puts `open #{f}.png`
end
