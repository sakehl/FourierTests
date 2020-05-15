#!/usr/bin/env python3

import sys
import random

random.seed(42)


if len(sys.argv) <= 3:
    m = int(sys.argv[1])
    n = int(sys.argv[2])
    rands = [[random.randint(0, 2**31 - 1) for i in range(m)] for j in range(n)]

    with open('data/list_{}_{}.in'.format(str(m),str(n)), 'w') as f:
        print(rands, file=f)

    # out = list(map(sorted, rands))

    # with open('data/list_{}_{}.out'.format(str(m),str(n)), 'w') as f:
    #     print(out, file=f)
else:
    l = int(sys.argv[1])
    m = int(sys.argv[2])
    n = int(sys.argv[3])
    rands = [[[random.randint(0, 2**31 - 1) for i in range(l)] for j in range(m)] for k in range(n)]

    with open('data/list_{}_{}_{}.in'.format(str(l),str(m),str(n)), 'w') as f:
        print(rands, file=f)

    # out = list(map(sorted, rands))

    # with open('data/list_{}_{}_{}.out'.format(str(l),str(m),str(n)), 'w') as f:
    #     print(out, file=f)