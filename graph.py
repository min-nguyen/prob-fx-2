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
    # corresponding y axis values
    ys = xys[1]
    # plotting the points
    plt.scatter(xs, ys)
    # naming the x axis
    plt.xlabel('x - axis')
    # naming the y axis
    plt.ylabel('y - axis')
    # giving a title to my graph
    plt.title('Linear regression')
    # function to show the plot
    plt.show()
  if arg == "lw":
    xys        = [ d[0] for d in data]
    # sample maps
    sampleMaps = [ d[1] for d in data]
    # probabilities
    ps         = [ d[2] for d in data]
    # x axis values
    xs = [x[0] for x in xys]
    # corresponding y axis values
    ys = [y[1] for y in xys]
    # plotting the points
    plt.scatter(xs, ys, c=ps, cmap='gray')
    # naming the x axis
    plt.xlabel('x - axis')
    # naming the y axis
    plt.ylabel('y - axis')
    # giving a title to my graph
    plt.title('Linear regression')
    # function to show the plot
    plt.show()


if __name__ == "__main__":
  main()