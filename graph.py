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
    xyps = [[ i for i, j in data ],
            [ j for i, j in data ]]
    xys = [[ i for i, j in xyps[0] ],
           [ j for i, j in xyps[0] ]]
    # x axis values
    xs = xys[0]
    # corresponding y axis values
    ys = xys[1]
    # probabilities
    ps = xyps[1]
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