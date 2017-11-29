#! usr/bin/env python

from math import sqrt

for run in range(6):
    file1 = open("../data/times_just_C_run_{}.txt".format(run))
    file2 = open("../data/times_poy_processor_{}.txt".format(run))

    C_times_str   = file1.readlines()
    POY_times_str = file2.readlines()

    if len(C_times_str) < 11 or len(POY_times_str) < 11:
        continue

    file1.close()
    file2.close()

    variance    = 0
    mean        = 0
    expectation = 0
    POY_count   = 0
    C_count     = 0

    C_times   = list(map(lambda x: int(float(x.split()[-1])), C_times_str))
    POY_times = list(map(lambda x: int(float(x.split()[-1])), POY_times_str))

    print("Run {}:".format(run))
    for c, poy in zip(C_times[:-1], POY_times[:-1]): # remember, last line is average
        print("{:>7}{:>7}{:>7}".format(c, poy, c - poy))
        # print((c - poy) * (c - poy))
        mean += abs(c - poy)
        if c < poy:
            C_count   += 1
        else:
            POY_count += 1

    mean /= 10

    for c, poy in zip(C_times[:-1], POY_times[:-1]):
        difference = abs(c - poy)
        rel_diff = c - poy
        variance  += (difference - mean) * (difference - mean)
        expectation += rel_diff

    expectation /= 10
    variance    /= 10

    print("\nC is faster:    {:>8}".format(C_count))
    print("Poy is faster:  {:>8}".format(POY_count))
    print("Mean time diff: {:>8}".format(int(mean)))
    print("Expectation:    {:>8}".format(int(expectation)))
    print("Variance:       {:>8}".format(int(variance)))
    print("Std. Dev:       {:>8}".format(int(sqrt(variance))))
    print()