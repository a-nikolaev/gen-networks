#! /usr/bin/ruby

=begin

reverse-CMF: 
  P(k >= x) = 1 - CMF(x)

=end

vs = Array.new
ps = Array.new

$stdin.each_line{|line|
  arr = line.split()
  val = arr[0].to_f
  prob = arr[1].to_f

  vs << val
  ps << prob
}

ps.reverse!
vs.reverse!

sum = 0.0

vs.each_index{|i|
  prob = ps[i]
  val = vs[i]

  sum += prob

  puts "#{val} #{sum}"
}


