#! /usr/bin/ruby

=begin

Reading PDF of some distribution: 

Value Probability

1      0.7
2      0.1
3      0.05
4      0.01 
5      0.001
.
.
.
234    0.0000001


Put them in bins:
bin (1): 1
bin (2): 2 3
bin (3): 4 5 6 7
bin (4): 8 9 10 11 12 13 14 15
...

Each bin i groups all values in the range:  2^{i-1} <= value < 2^{i}

=end

bins = Hash.new
bin_size = Hash.new

bin_wgt_sum = Hash.new
bin_prob = Hash.new


$stdin.each_line{|line|
  arr = line.split()
  val = arr[0].to_f
  prob = arr[1].to_f
 
  i = (Math.log2(val*(1 + 1e-14)) + 1).floor

  if bins[i] == nil
    bins[i] = []
    bin_size[i] = 2.0**(i-1)

    bin_wgt_sum[i] = 0
    bin_prob[i] = 0
  end
  bins[i] << [val, prob]
  
  bin_wgt_sum[i] += val * prob
  bin_prob[i] += prob

}

bins.keys.sort.each{| i |
  avg_val = bin_wgt_sum[i] / bin_prob[i] 
  prob = bin_prob[i] / bin_size[i]

  puts "#{avg_val} #{prob}"
}
