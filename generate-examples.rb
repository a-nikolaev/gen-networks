#! /bin/ruby

[0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9].each{|aa|
  [1, 2, 4, 8].each{|ll|
    cc = 1
    cmd = "./growth --type=sc --model=acl:2:#{aa},#{cc},#{ll} --any --n=1000000 > ./new-examples/sc-2-#{aa}-#{cc}-#{ll}-1M.sc"
    puts "#{cmd}"

    starting = Process.clock_gettime(Process::CLOCK_MONOTONIC)

    `#{cmd}`
    
    ending = Process.clock_gettime(Process::CLOCK_MONOTONIC)
    elapsed = ending - starting
    puts "Elapsed time: #{elapsed}\n"
  }
}
