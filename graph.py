# importing the required module
import matplotlib.pyplot as plt
import ast

# --| Linear Regression

f = open("model-output.txt", "r")
# Parse file to data structure :: [(Double, Double)]
xys = ast.literal_eval(f.read())
# Reify [(Double, Double)] to ([Double], [Double])
xsys = [[ i for i, j in xys ],
       [ j for i, j in xys ]]
print(xsys)
# x axis values
xs = xsys[0]
# corresponding y axis values
ys = xsys[1]
# plotting the points
plt.plot(xs, ys)
# naming the x axis
plt.xlabel('x - axis')
# naming the y axis
plt.ylabel('y - axis')
# giving a title to my graph
plt.title('Linear regression')
# function to show the plot
plt.show()