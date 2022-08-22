#! /usr/bin/ruby

dir = "data-power-law"

ftd_file = "./#{dir}/ftd.dat"

# Get binned graph for plotting
`./log-binning-prob.rb < #{ftd_file} > ./#{dir}/log-binned-prob.dat`

# Expand the FTD histogram for the Kolmogorov-Smirnov power-law statistics
`cat #{ftd_file}  | ./expand-hist.rb > ./#{dir}/expanded-ftd.dat`

# puts `cat ./#{dir}/expanded-ftd.dat`

sleep(0.5)

# Run PL Fit
arr = `$SC/plfit-mod/plfit/build/src/plfit -b -p exact ./#{dir}/expanded-ftd.dat`.strip().split()

# Get the answer like:
#-: D 2.54284 1 -14970.9 0.0161236 0
# arr[1] == 'D' means discrete distribution

# exponent alpha
alpha = arr[2].to_f
# xmin
xmin = arr[3].to_i
# Likelihood L
ll = arr[4].to_f
# D
dd = arr[5].to_f
# p-value
pval = arr[6].to_f

msg = "alpha = #{alpha}\nxmin = #{xmin}\npval = #{pval}\n"
puts msg

#`echo "#{msg}" > ./plots/model-params.plt`

#`(cd ./plots; gnuplot plot-better.plt)`
