#! /bin/ruby

[0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9].each{|aa|
  [1, 2, 4, 8].each{|ll|
    cc = 1
    base = "./virt/hg-2-#{aa}-#{cc}-#{ll}-1M"
    
    cmd = "./growth --type=hg --model=acl:2:#{aa},#{cc},#{ll} --any --n=1000000 --virt='#{base}.bin' > #{base}.sc"
    puts "#{cmd}"
    `#{cmd}`
    
    cmd = "./virt_graph_degrees #{base}.bin #{base}.in #{base}.out"
    puts "#{cmd}"
    `#{cmd}`
    
    cmd = "cat #{base}.in | ./hist.rb > #{base}.in.hist"
    puts "#{cmd}"
    `#{cmd}`
    
    cmd = "cat #{base}.out | ./hist.rb > #{base}.out.hist"
    puts "#{cmd}"
    `#{cmd}`
    
    cmd = "cat #{base}.sc | ./fs_dist > #{base}.fs"
    puts "#{cmd}"
    `#{cmd}`
    
    cmd = "cat #{base}.sc | ./fd_dist > #{base}.fd"
    puts "#{cmd}"
    `#{cmd}`
    
  }
}
