#! /usr/bin/ruby

if ARGV.size < 4
  puts "Usage:\n\t./sample_pic.rb [1|2] [prop|unif] {-}metric [n|i] count seed\n\n"
  exit 1
end

puts ARGV.inspect

growing_type = ARGV[0]
sampling = ARGV[1]
metric = ARGV[2]
stopping = ARGV[3]
count = ARGV[4]
seed = ARGV[5]


control = "best #{metric}"
if metric == '0' then
  control = 'any 0'
end
 
system("./growth #{growing_type} #{sampling} #{control} #{stopping} #{count} #{seed} | tee _tmp_1.sc; ./stats < _tmp_1.sc; ~/prog/visualsc/visualsc < _tmp_1.sc > _tmp.pdf")
