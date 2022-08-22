#! /usr/bin/env ruby 


require 'linefit'

lineFit = LineFit.new

x_arr = Array.new
y_arr = Array.new

xmin = ARGV[0]
if xmin != nil
  xmin = xmin.to_i
end
xmax = ARGV[1]
if xmax != nil
  xmax = xmax.to_i
end

STDIN.each_line{|line|

    arr = line.split()
    x = arr[0].to_f
    y = arr[1].to_f

    if (xmin == nil || x >= xmin) && (xmax == nil || x <= xmax) then
      x_arr << Math.log(x)
      y_arr << Math.log(y)
    end
}

lineFit.setData(x_arr, y_arr)

intercept, slope = lineFit.coefficients

r_squared = lineFit.rSquared

mean_squared_error = lineFit.meanSqError

puts "#{slope}   #{intercept}   #{r_squared}      # slope   intercept   R2"
