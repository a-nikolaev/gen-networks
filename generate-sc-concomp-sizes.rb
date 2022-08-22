#! /bin/ruby

def compute_concomp(aa, cc, ll)
    
    arr = []
    sum = 0
    num = 20

    iters = 1000000

    num.times{
      max_cc = `./growth --type=sc --model=acl:2:#{aa},#{cc},#{ll} --any --n=#{iters}`
      v = max_cc.to_f / iters.to_f
      arr << v
      sum += v
    }
    
    avg = sum.to_f / num.to_f
    arr.map!{|v| v.round(5)}
    #puts "#{aa} #{cc} #{ll}  #{sprintf("%9g", avg)}  #{arr.join(' ')}"
    puts "#{aa} #{cc} #{ll}  #{sprintf("%9g", avg)}"
end

8.times{
  cc = 1
  [1,2,4].each{|ll|
    [0.55, 0.6, 0.65, 0.7].each{|aa|
      compute_concomp(aa, cc, ll)
    }
    puts ''
  }
}

=begin
8.times{
  cc = 1
  [1,2,4].each{|ll|
    [0.05, 0.1, 0.15, 0.20, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5].each{|aa|
      compute_concomp(aa, cc, ll)
    }
    puts ''
  }
}
=end

=begin
8.times{
  cc = 1
  [1].each{|ll|
    [0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.32, 0.34, 0.35, 0.36, 0.38, 0.4, 0.42, 0.44, 0.45, 0.46, 0.48, 0.5, 0.52, 0.55, 0.6, 0.65, 0.7, 0.8, 0.9].each{|aa|
      compute_concomp(aa, cc, ll)
    }
  }
}
=end


=begin
[2, 4].each{|cc|
  [1, 2, 4].each{|ll|
    [0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.6, 0.7, 0.8, 0.9].each{|aa|
      compute_concomp(aa, cc, ll)
    }
  }
}
=end

=begin
ll = 1
[0.58, 0.6].each{|aa|
  compute_concomp(aa, ll)
}
=end

=begin
ll = 1
[0.1, 0.2, 0.3].each{|aa|
  compute_concomp(aa, ll)
}
[0.36, 0.38, 0.4, 0.42, 0.44, 0.46, 0.48, 0.5, 0.52, 0.54, 0.56, 0.58].each{|aa|
  compute_concomp(aa, ll)
}
[0.6, 0.7, 0.8, 0.9].each{|aa|
  compute_concomp(aa, ll)
}
=end

=begin
ll = 2
[0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28, 0.29].each{|aa|
  compute_concomp(aa, ll)
}

ll = 4
[0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28, 0.29].each{|aa|
  compute_concomp(aa, ll)
}
=end
