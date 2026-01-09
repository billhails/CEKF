import csv

# Alternative to makeUnicodeData.py that generates a sparse lookup table
# for binary search (currently unused).
def allChars():
    with open('unicode/UnicodeData.txt', newline='') as csvin:
        reader = csv.reader(csvin, delimiter=';')
        i = 0
        prev = 'GC_Co'
        for row in reader:
            j = int(row[0], 16)
            while i < j:
                yield prev
                i += 1
            i += 1
            prev = f"GC_{row[2]}"
            yield prev
        print('')

all = allChars()
i = 0
count = 0
prev = ''
for code in all:
    if code != prev:
        print(f'{{ .c = {i}, .dec = {code} }},')
        count += 1
    i += 1
    prev = code

print(f'#define UNICODE_LOOKUP_TOTAL {count}')