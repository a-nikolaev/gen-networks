#! /usr/bin/ruby

# Expands a histogram data to a linear "vector" data for R

arr = Array.new
min_prob = 1.0e1000

$stdin.each_line{|line|
  a = line.split()

  val = a[0]
  prob = a[1].to_f

  if (prob < min_prob) 
    min_prob = prob
  end

  arr << [val, prob]

}

arr.reverse!

factor = (1.0 / min_prob).round.to_i

arr.each{|a|
  v = a[0]
  p = a[1]
  count = (p * factor).round.to_i

  count.times{
    puts v
  }
}
