import csv

with open('UnicodeData.csv', 'w', newline='') as csvout:
    writer = csv.writer(csvout, delimiter=',', quotechar='"')
    with open('UnicodeData.txt', newline='') as csvin:
        reader = csv.reader(csvin, delimiter=';')
        for row in reader:
            row = [int(row[0], 16)] + row[0:11] + row[12:]
            writer.writerow(row)
