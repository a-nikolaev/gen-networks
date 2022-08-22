#! /usr/bin/ruby

=begin

./log-binning-xy.rb < raw-xy-data > xy-log-binned-averages

Reading X-Y dataset: 

Value Probability

X1     Y1
X2     Y2
X3     Y3
....


Put the (X,Y)-values into bins that grow exponentially in size:
Each bin i groups all values in the range, and contain points 
based on their X coordinate:  2^{i-1} <= X < 2^{i}

The program computes the average (X,Y) in each bin.

bin (1):  0.5   7.2
bin (2):  1.4   3.1  
bin (3):  3.1   2.7
bin (4):  5.7   2.5
...

=end

bins = Hash.new
bin_size = Hash.new

bin_x = Hash.new
bin_y = Hash.new
bin_num = Hash.new


$stdin.each_line{|line|
  arr = line.split()
  x = arr[0].to_f
  y = arr[1].to_f
 
  i = (Math.log2(x*(1 + 1e-14)) + 1).floor

  if bins[i] == nil
    bins[i] = []
    bin_size[i] = 2.0**(i-1)
    
    bin_num[i] = 0
    bin_x[i] = 0.0
    bin_y[i] = 0.0
  end
  bins[i] << [x, y]
  
  bin_x[i] += x
  bin_y[i] += y
  bin_num[i] += 1
}

bins.keys.sort.each{| i |
  avg_x = bin_x[i] / bin_num[i] 
  avg_y = bin_y[i] / bin_num[i]

  puts "#{avg_x} #{avg_y}"
}
