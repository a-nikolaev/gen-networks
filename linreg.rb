#! /usr/bin/env ruby 


require 'linefit'

lineFit = LineFit.new

x_arr = Array.new
y_arr = Array.new

STDIN.each_line{|line|

    arr = line.split()
    x = arr[0].to_f
    y = arr[1].to_f

    if x >= 25 && x < 2000 then
      x_arr << Math.log(x)
      y_arr << Math.log(y)
    end
}

lineFit.setData(x_arr, y_arr)

intercept, slope = lineFit.coefficients

r_squared = lineFit.rSquared

mean_squared_error = lineFit.meanSqError

puts "#{slope}   #{intercept}   #{r_squared}      # slope   intercept   R2"
