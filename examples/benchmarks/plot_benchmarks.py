import csv
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import FormatStrFormatter

def zero_to_nan(values):
    """Replace every 0 with 'nan' and return a copy."""
    return [float('nan') if x==0 else x for x in values]

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

def plotPage(output_file, data_dicts, n_groups, n_rows):
  fig_a, axis_a = plt.subplots(n_rows, n_groups)
  for dict_idx, data_dict in enumerate(data_dicts):
    for col_idx, group in enumerate(data_dict["data"]):
      # Get header
      header_row  = group[0]
      # Get header parameter's and range
      x_parameter = header_row[0]
      x_values    = list(map(int, header_row[1:]))

      # Iterate over programs
      for row_idx, prog in enumerate((group[1:])):
        prog_name       = prog[0]
        prefix_label    = prog_name.split("-[ ]-", 1)[0]
        suffix_label    = prog_name.split("-[ ]-", 1)[1]
        print(prog)
        if dict_idx == 0:
          if row_idx == 0:
            ### Set top x-title
            axis_a[0][col_idx].set_title(prefix_label, fontsize=9)
            ### Set bottom x-label
            axis_a[n_rows - 1][col_idx].set_xlabel(x_parameter, fontsize=6.5)
          if col_idx == 0:
            ### Set left y-label
            axis_a[row_idx][0].set_ylabel("Exec time (s)", fontsize=6.5)
            ### Set right y-label
            twin_axis = axis_a[row_idx][n_groups - 1].twinx()
            twin_axis.set_yticks([])
            twin_axis.set_ylabel(suffix_label, fontsize=9, rotation='horizontal', va='center', ha='left')

        ### Get benchmarks, and use 'zero_to_nan' to avoid plotting dummy results
        prog_values  = zero_to_nan(list(map(float, prog[1:])))

        if (not (0 in prog_values)):
          axis_a[row_idx][col_idx].plot(x_values, prog_values, color=data_dict["color"], label=data_dict["language"])

        xmin, xmax, ymin, ymax = axis_a[row_idx][col_idx].axis()

        ### Set x ticks
        axis_a[row_idx][col_idx].set_xticks([xmin, xmax])
        axis_a[row_idx][col_idx].xaxis.set_major_locator(plt.MaxNLocator(5))
        axis_a[row_idx][col_idx].tick_params(axis='x', which='major', pad=0)
        plt.setp(axis_a[row_idx][col_idx].get_xticklabels(), rotation=20,  horizontalalignment='center', fontsize=5)

        ### Set y ticks
        axis_a[row_idx][col_idx].set_yticks([0, ymax])
        axis_a[row_idx][col_idx].yaxis.set_major_locator(plt.MaxNLocator(4))
        axis_a[row_idx][col_idx].tick_params(axis='y', which='major', pad=0)
        plt.setp(axis_a[row_idx][col_idx].get_yticklabels(), fontsize=5)

  ### Adjust padding between subplots
  plt.subplots_adjust(hspace=0.39, wspace=0.25)
  # fig_a.tight_layout()

  ###
  lgd = fig_a.legend([data_dict["language"] for data_dict in data_dicts], loc="upper center", bbox_to_anchor=(0.5, 1), fontsize=8, ncol=len(data_dicts))
  ### Adjust bounding box for legend in saved figure
  fig_a.savefig(output_file, bbox_extra_artists=(lgd,), bbox_inches='tight')

with open('benchmarks-prob-fx.csv') as benchmarks_pfx, open('benchmarks-monad-bayes.csv') as benchmarks_mb, open('benchmarks-gen.csv') as benchmarks_gen:

  raw_data_pfx                = [row for row in csv.reader(benchmarks_pfx, delimiter=',')]
  (models_pfx, raw_data_pfx)  = groupBenchmarks(raw_data_pfx, n_groups=3, n_rows=5)
  (infs_pfx, _)               = groupBenchmarks(raw_data_pfx, n_groups=5, n_rows=3)
  models_pfx                  = { "language": "InferFX", "color": '#7570b3', "data": models_pfx }
  infs_pfx                    = { "language": "InferFX", "color": '#7570b3', "data": infs_pfx }

  raw_data_mb                 = [row for row in csv.reader(benchmarks_mb, delimiter=',')]
  (models_mb, raw_data_mb)    = groupBenchmarks(raw_data_mb, n_groups=3, n_rows=5)
  (infs_mb, _)                = groupBenchmarks(raw_data_mb, n_groups=5, n_rows=3)
  models_mb                   = { "language": "MonadBayes", "color": '#d95f02', "data": models_mb }
  infs_mb                     = { "language": "MonadBayes", "color": '#d95f02', "data": infs_mb }

  raw_data_gen                = [row for row in csv.reader(benchmarks_gen, delimiter=',')]
  (models_gen, raw_data_gen)  = groupBenchmarks(raw_data_gen, n_groups=3, n_rows=5)
  (infs_gen, _)               = groupBenchmarks(raw_data_gen, n_groups=5, n_rows=3)
  models_gen                  = { "language": "Gen", "color": '#1b9e77', "data": models_gen }
  infs_gen                    = { "language": "Gen", "color": '#1b9e77', "data": infs_gen }

  # benchmarks for varying over dataset size
  # groups_pfx_model  = groups_pfx[0:3]
  models = [models_pfx, models_mb, models_gen]
  plotPage("plot-model-benchmarks.pdf", models, n_groups=3, n_rows=5)

  # # # benchmarks for varying over inference parameters
  # # vary_inf = groups[3:6]
  infs   = [infs_pfx, infs_mb, infs_gen]
  plotPage("plot-inference-benchmarks.pdf", infs, n_groups=5, n_rows=3)
  plt.show()
