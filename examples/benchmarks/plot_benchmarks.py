import csv
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import FormatStrFormatter

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

def plotPage(data_dicts, n_groups, n_rows):
  fig_a, axis_a = plt.subplots(n_rows, n_groups)
  # fig_a.tight_layout()
  for data_dict in data_dicts:
    for col_idx, group in enumerate(data_dict["data"]):
      # Get header
      header_row  = group[0]
      x_parameter = header_row[0]
      axis_a[n_rows - 1][col_idx].set_xlabel(x_parameter, fontsize=10)
      x_values    = list(map(int, header_row[1:]))
      print(x_values)
      # Iterate over programs
      for row_idx, prog in enumerate((group[1:])):
        # Set bottom x-label
        prog_name       = prog[0]
        prefix_label    = prog_name.split("-[ ]-", 1)[0]
        suffix_label    = prog_name.split("-[ ]-", 1)[1]

        # Set top x-title
        axis_a[0][col_idx].set_title(prefix_label, fontsize=10)
        # Set left y-title
        axis_a[row_idx][0].set_ylabel(suffix_label, fontsize=10, rotation='horizontal', va='center', ha='right', labelpad=10)

        # Plot benchmarks
        prog_values  = list(map(float, prog[1:]))

        # Don't plot dummy results
        if (not (0 in prog_values)):
          axis_a[row_idx][col_idx].plot(x_values, prog_values, color=data_dict["color"], label=data_dict["language"])

        xmin, xmax, ymin, ymax = axis_a[row_idx][col_idx].axis()
        axis_a[row_idx][col_idx].set_xticks([xmin, xmax])
        axis_a[row_idx][col_idx].xaxis.set_major_locator(plt.MaxNLocator(5))
        axis_a[row_idx][col_idx].set_yticks([0, ymax])
        axis_a[row_idx][col_idx].yaxis.set_major_locator(plt.MaxNLocator(4))
        axis_a[row_idx][col_idx].yaxis.set_major_formatter(FormatStrFormatter('%.1f'))

  fig_a.legend([data_dict["language"] for data_dict in data_dicts], bbox_to_anchor=[1, 1], loc="upper right")


with open('benchmarks-prob-fx.csv') as benchmarks_pfx, open('benchmarks-monad-bayes.csv') as benchmarks_mb, open('benchmarks-gen.csv') as benchmarks_gen:

  raw_data_pfx                = [row for row in csv.reader(benchmarks_pfx, delimiter=',')]
  (models_pfx, raw_data_pfx)  = groupBenchmarks(raw_data_pfx, n_groups=3, n_rows=4)
  (infs_pfx, _)                = groupBenchmarks(raw_data_pfx, n_groups=4, n_rows=3)
  models_pfx                  = { "language": "ProbFX", "color": '#7570b3', "data": models_pfx }
  infs_pfx                     = { "language": "ProbFX", "color": '#7570b3', "data": infs_pfx }

  raw_data_mb                 = [row for row in csv.reader(benchmarks_mb, delimiter=',')]
  (models_mb, raw_data_mb)    = groupBenchmarks(raw_data_mb, n_groups=3, n_rows=4)
  (infs_mb, _)                 = groupBenchmarks(raw_data_mb, n_groups=4, n_rows=3)
  models_mb                   = { "language": "MonadBayes", "color": '#d95f02', "data": models_mb }
  infs_mb                      = { "language": "MonadBayes", "color": '#d95f02', "data": infs_mb }

  raw_data_gen                 = [row for row in csv.reader(benchmarks_gen, delimiter=',')]
  (models_gen, raw_data_gen)    = groupBenchmarks(raw_data_gen, n_groups=3, n_rows=4)
  (infs_gen, _)                 = groupBenchmarks(raw_data_gen, n_groups=4, n_rows=3)
  models_gen                   = { "language": "Gen", "color": '#1b9e77', "data": models_gen }
  infs_gen                      = { "language": "Gen", "color": '#1b9e77', "data": infs_gen }

  print(models_pfx, infs_pfx)

  models = [models_pfx, models_mb, models_gen]
  # benchmarks for varying over dataset size
  # groups_pfx_model  = groups_pfx[0:3]
  plotPage(models, n_groups=3, n_rows=4)

  infs   = [infs_pfx, infs_mb, infs_gen]
  # # # benchmarks for varying over inference parameters
  # # vary_inf = groups[3:6]
  plotPage(infs, n_groups=4, n_rows=3)
  plt.show()

  print(infs_mb)