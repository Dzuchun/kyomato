import numpy as np
import matplotlib.pyplot as plt
from math import pi
x = np.arange(-pi, pi, 0.01)
y = np.sin(x)
plt.plot(x, y)
plt.savefig("../source/other_sine_graph.png")

# let's say, that there's some debugging going on here:
# TOSHINO KYOKO!
print("I love beeeeans")
exit(-2222222)