# importing the required module
import sys
import matplotlib.pyplot as plt
import ast
import pandas as pd

def main():
  arg  = sys.argv[1]
  f    = open("model-output.txt", "r")
  data = ast.literal_eval(f.read())
  if arg == "basic":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    print(xys)
    # x axis values
    xs = xys[0]
    # y axis values
    ys = xys[1]
    plt.scatter(xs, ys)
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Linear regression')
    plt.show()
  if arg == "lw":
    xys        = [ d[0] for d in data]
    # sample maps
    sampleMaps = [ d[1] for d in data]
    # probabilities
    ps         = [ d[2] for d in data]
    # x axis values
    xs = [x[0] for x in xys]
    # y axis values
    ys = [y[1] for y in xys]

    mu_samples    = [ d[0][1] for d in sampleMaps ]
    c_samples     = [ d[1][1] for d in sampleMaps ]
    std_samples   = [ d[2][1] for d in sampleMaps ]

    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("x axis")
    axs1.set_ylabel("y axis")
    axs1.scatter(xs, ys, c=ps, cmap='gray')
    axs1.set_title('Linear regression')

    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel('mu value')
    axs2.set_ylabel('probability')
    axs2.scatter(mu_samples, ps)
    axs2.set_title('Linear regression')

    plt.show()


if __name__ == "__main__":
  main()