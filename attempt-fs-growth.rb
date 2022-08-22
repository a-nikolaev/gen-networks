#! /bin/ruby

require 'tempfile'

def avg_fs(file)
  s = `cat #{file} | ./stats`
  arr = s.split("\n")
  arr.each{|line|
    entry = line.split
    if entry[1] == 'AFS' 
      return entry[0].to_f
    end
  }
  return nil
end

def sample_size(fs_arr)
  x = rand()
  fs_arr.each{|entry|
    p = entry[1]
    if x <= p then
      return entry[0]
    else
      x -= p
    end
  }
  # if somehow reached the end of the array
  fs_arr[0][0] # return the first size entry
end

def grow(aa, cc, ll, iters, fs_arr) 
  tmp_file1 = Tempfile.new('attempt-grow')
  tmp_file1.write("%sc\n{0}")
  tmp_file1.close
  path1 = tmp_file1.path
  
  tmp_file2 = Tempfile.new('attempt-grow')
  path2 = tmp_file2.path
 
  iters.times{|i|
    afs = avg_fs(path1)
    size = sample_size(fs_arr)
    # expectation of binomial distribution: n*p
    expected_sampled = ((afs * ll) * (aa / ll)).to_i
    alt_cc = [1, size - expected_sampled].max
    `./growth --type=sc --model=acl:2:#{aa},#{alt_cc},#{ll} --any --i=1 #{path1} > #{path2}`
    sleep(0.001)
    `cp #{path2} #{path1}`
    sleep(0.001)
  }

  system("cat #{path1}")

  tmp_file1.unlink
  tmp_file2.unlink
end

def run()

  if ARGV.size < 5 
    puts "Usage:\n\t ./atempt-fs-growth.rb aa cc ll i fsdist\n\n"
    exit(1)
  end

  aa = ARGV[0].to_f
  cc = ARGV[1].to_i
  ll = ARGV[2].to_i
  iters = ARGV[3].to_i
  fsdist_file = ARGV[4]

  fs_arr = Array.new
  File.open(fsdist_file).each_line{|line|
    arr = line.split
    if arr.size > 1 then
      size = arr[0].to_i
      p = arr[1].to_f
      fs_arr << [size, p]
    end
  }

  grow(aa, cc, ll, iters, fs_arr)
end

run()
