# generate the body of a case statement to map unicode code points to their digit values
import csv

count = 0
print("#define UNICODE_DIGITS_CASES \\")

with open('unicode/UnicodeData.txt', newline='') as csvin:
    reader = csv.reader(csvin, delimiter=';')
    for row in reader:
        if row[6]:
            print(f"    case 0x{row[0]}: return {row[6]}; \\")
            count += 1
print("    default: return -1;")
print(f"# define NUM_UNICODE_DIGITS {count}")