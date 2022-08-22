#! /usr/bin/ruby

require 'set'

# Convert a set of integers into a histogram 

hist = Hash.new(0)
set = Set.new
count = 0

$stdin.each_line{|line|
  a = line.split()
  a.each{|s|
    x = s.to_i
    set << x
    count += 1
    hist[x] = hist[x] + 1
  }
}

arr = set.to_a
arr.sort!

count_f = count.to_f
arr.each{|x|
  freq = hist[x].to_f / count_f
  puts "#{x} #{freq}"
}

