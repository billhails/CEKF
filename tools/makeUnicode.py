import csv
import sys

bitvalue = {
    "Cc" : 1,
    "Cf" : 1 << 1,
    "Cn" : 1 << 2,
    "L&" : 1 << 3,
    "Ll" : 1 << 4,
    "Lm" : 1 << 5,
    "Lo" : 1 << 6,
    "Lt" : 1 << 7,
    "Lu" : 1 << 8,
    "Mc" : 1 << 9,
    "Me" : 1 << 10,
    "Mn" : 1 << 11,
    "Nd" : 1 << 12,
    "Nl" : 1 << 13,
    "No" : 1 << 14,
    "Pc" : 1 << 15,
    "Pd" : 1 << 16,
    "Pe" : 1 << 17,
    "Pf" : 1 << 18,
    "Pi" : 1 << 19,
    "Po" : 1 << 20,
    "Ps" : 1 << 21,
    "Sc" : 1 << 22,
    "Sk" : 1 << 23,
    "Sm" : 1 << 24,
    "So" : 1 << 25,
    "Zl" : 1 << 27,
    "Zp" : 1 << 28,
    "Zs" : 1 << 29,
}

data = [0 for i in range(1114112)]
with open('hs/UnicodePropList.csv', newline='') as csvfile:
    reader = csv.reader(csvfile)
    first = True
    biggest = 0
    for row in reader:
        if first:
            first = False
        else:
            n0 = int('0x' + row[0], 16);
            n1 = int('0x' + row[1], 16);
            for i in range(n0, n1+1):
                if data[i] is None:
                    data[i] = 0
                data[i] = data[i] | bitvalue[row[2]]

for i in range(len(data)):
    print(i, data[i])
