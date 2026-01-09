# generate a full category table for all Unicode code points
import csv

with open('unicode/UnicodeData.txt', newline='') as csvin:
    reader = csv.reader(csvin, delimiter=';')
    i = 0
    prev = 'GC_Co'
    print('/* 000000 */ ', end='')
    for row in reader:
        j = int(row[0], 16)
        while i < j:
            if i > 0 and i % 16 == 0:
                print('')
                print('/* {:06X} */ '.format(i), end='')
            print(f"{prev}, ", end='')
            i += 1
        i += 1
        if j > 0 and j % 16 == 0:
            print('')
            print('/* {:06X} */ '.format(j), end='')
        prev = f"GC_{row[2]}"
        print(f"{prev}, ", end='')
    print('')