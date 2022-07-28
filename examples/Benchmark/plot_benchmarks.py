import csv
import matplotlib.pyplot as plt
import numpy as np

fixed_groupSize = 4

def groupBenchmarks(file):
  def chunksOf(xs, n):
      for i in range(0, len(xs), n):
          yield xs[i:i + n]

  csv_reader = csv.reader(file, delimiter=',')
  raw_data = [row for row in csv_reader]
  grouped_data = list(chunksOf(raw_data, fixed_groupSize))
  return grouped_data

def plotPage(hs_data):
  fig_a, axis_a = plt.subplots(3, 3)
  fig_a.tight_layout()
  for row_idx, group in enumerate(hs_data):
    # Get header
    header_row  = group[0]
    x_parameter = header_row[0]
    x_values    = list(map(int, header_row[1:]))
    # Iterate over programs
    for col_idx, prog in enumerate((group[1:])):
      # Set up plot labels
      axis_a[row_idx][col_idx].set_xlabel(x_parameter, fontsize=8)
      axis_a[row_idx][col_idx].set_xticks(x_values)
      axis_a[row_idx][col_idx].set_ylabel("time", fontsize=8)

      prog_name       = prog[0]
      axis_a[row_idx][col_idx].set_title(prog_name, fontsize=10)

      # Plot benchmarks
      prog_values  = list(map(float, prog[1:]))
      axis_a[row_idx][col_idx].plot(x_values, prog_values, label='ProbFX')

with open('benchmarks.csv') as benchmarks:
  groups  = groupBenchmarks(benchmarks)

  # benchmarks for varying over dataset size
  vary_data  = groups[0:3]
  plotPage(vary_data)

  # benchmarks for varying over inference parameters
  vary_inf = groups[3:5]
  plotPage(vary_inf)
  plt.show()