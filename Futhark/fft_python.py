#!/usr/bin/env python3

import fft
import random
import numpy as np
import pyopencl

print("Hello")

o = fft.fft()

n = 4
ys_r = o.main_n(n)
ys_i = o.mainy_n(n)

xs = o.test2(n)
ys = ys_r + ys_i*1j
print(ys)
# print(np.max((np.abs(np.fft.fft(xs) - ys))))

n = 4
ys_r = o.main2d_n(n)
ys_i = o.main2dy_n(n)

xs = o.test2(n).get()
ys = ys_r + ys_i*1j

print(np.max((np.abs(np.fft.fft2(xs) - ys.get()))))
print(ys)
print(np.fft.fft2(xs))
