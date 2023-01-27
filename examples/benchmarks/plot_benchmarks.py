import csv
import matplotlib.pyplot as plt
import numpy as np

fixed_groupSize = 3

def groupBenchmarks(raw_data, n_groups, n_rows):
  def chunksOf(xs, n):
      for i in range(0, len(xs), n):
          yield xs[i:i + n]

  rows_fst  = [row for row in raw_data[0:n_groups * (1 + n_rows)]]
  rows_rest = [row for row in raw_data[n_groups * (1 + n_rows):]]
  grouped_data = list(chunksOf(rows_fst, 1 + n_rows))
  # Expect each benchmark group to be in the form
      # > Header
      # > Row: 1
      # > ...
      # > Row: fixed_groupSize
  return (grouped_data, rows_rest)

def plotPage(hs_data, n_groups, n_rows):
  fig_a, axis_a = plt.subplots(n_groups, n_rows)
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

with open('benchmarks.csv') as benchmarks_pfx:
  csv_reader = csv.reader(benchmarks_pfx, delimiter=',')
  raw_data_pfx   = [row for row in csv_reader]
  (models_pfx, raw_data_pfx)  = groupBenchmarks(raw_data_pfx, n_groups=3, n_rows=4)
  (inf_pfx, _)                = groupBenchmarks(raw_data_pfx, n_groups=4, n_rows=3)
  print(models_pfx, inf_pfx)

  # benchmarks for varying over dataset size
  # groups_pfx_model  = groups_pfx[0:3]
  plotPage(models_pfx, n_groups=3, n_rows=4)

  # # benchmarks for varying over inference parameters
  # vary_inf = groups[3:6]
  plotPage(inf_pfx, n_groups=4, n_rows=3)
  plt.show()