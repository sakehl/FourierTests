#!/usr/bin/env python3

import sys
import random

random.seed(42)

n = int(sys.argv[1])
m = int(sys.argv[2])

if len(sys.argv) <= 3:
    rands = [[random.randint(0, 2**31 - 1) for i in range(n)] for j in range(m)]

    with open('data/list_{}_{}.in'.format(str(n),str(m)), 'w') as f:
        print(rands, file=f)

    out = list(map(sorted, rands))

    with open('data/list_{}_{}.out'.format(str(n),str(m)), 'w') as f:
        print(out, file=f)
else:
    k = int(sys.argv[3])
    rands = [[[random.randint(0, 2**31 - 1) for i in range(n)] for j in range(m)] for k in range(k)]

    with open('data/list_{}_{}_{}.in'.format(str(n),str(m),str(k)), 'w') as f:
        print(rands, file=f)

    out = list(map(sorted, rands))

    with open('data/list_{}_{}_{}.out'.format(str(n),str(m),str(k)), 'w') as f:
        print(out, file=f)