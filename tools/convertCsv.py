import csv

with open('unicode/UnicodeData.csv', 'w', newline='') as csvout:
    writer = csv.writer(csvout, delimiter=',', quotechar='"')
    with open('unicode/UnicodeData.txt', newline='') as csvin:
        reader = csv.reader(csvin, delimiter=';')
        for row in reader:
            # these -1 values wil get transformed to null by the sql import script
            if row[3] == '':
                row[3] = '-1'
            if row[6] == '':
                row[6] = '-1'
            if row[7] == '':
                row[7] = '-1'
            if row[8] == '':
                row[8] = '-1'
            if row[12] == '':
                row[12] = '-1'
            else:
                row[12] = int(row[12], 16)
            if row[13] == '':
                row[13] = '-1'
            else:
                row[13] = int(row[13], 16)
            if row[14] == '':
                row[14] = '-1'
            else:
                row[14] = int(row[14], 16)
            row = [int(row[0], 16)] + row[0:11] + row[12:]
            writer.writerow(row)
