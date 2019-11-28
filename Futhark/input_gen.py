#!/usr/bin/env python3

import sys
import random

random.seed(42)

n = int(sys.argv[1])
m = int(sys.argv[2])

rands = [[random.randint(0, 2**31 - 1) for i in range(n)] for j in range(m)]

with open('list_{}_{}.in'.format(str(n),str(m)), 'w') as f:
    print(rands, file=f)

out = list(map(sorted, rands))

with open('list_{}_{}.out'.format(str(n),str(m)), 'w') as f:
    print(out, file=f)