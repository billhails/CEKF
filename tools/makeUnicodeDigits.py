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

# 2: {'Zp', 'Pf', 'Co', 'Zl', 'Lt', 'Nl', 'Pc', 'Pe', 'Pi', 'Pd', 'Nd', 'Mn', 'Cc', 'Lu', 'Cs', 'Sk', 'Me', 'Cf', 'Ps', 'So', 'Lm', 'Po', 'Sc', 'Mc', 'Sm', 'Ll', 'Zs', 'No', 'Lo'}
#
# 3: {'1', '103', '216', '17', '33', '6', '226', '240', '91', '18', '10', '118', '23', '130', '28', '228', '222', '36', '20', '218', '234', '9', '22', '11', '21', '84', '107', '202', '0', '16', '12', '129', '25', '14', '214', '230', '24', '19', '34', '35', '31', '27', '8', '26', '132', '224', '13', '220', '233', '15', '29', '7', '122', '32', '30', '232'}
#
# 4: {'ET', 'AN', 'PDF', 'L', 'B', 'BN', 'PDI', 'RLE', 'LRO', 'RLI', 'LRE', 'LRI', 'AL', 'WS', 'RLO', 'CS', 'ES', 'R', 'S', 'EN', 'ON', 'FSI', 'NSM'}
#
# 6: {'', '3', '7', '4', '5', '2', '9', '8', '1', '0', '6'}
#
# 7: {'', '0', '8', '5', '3', '6', '2', '9', '4', '7', '1'}
#
# 8: various pseudo-numeric values: 100000, 13/2 etc.
#
# 9: {'N', 'Y'}

# GC_None
# GC_Cc
# GC_Cf
# GC_Co
# GC_Cs
# GC_Ll
# GC_Lm
# GC_Lo
# GC_Lt
# GC_Lu
# GC_Mc
# GC_Me
# GC_Mn
# GC_Nd
# GC_Nl
# GC_No
# GC_Pc
# GC_Pd
# GC_Pe
# GC_Pf
# GC_Pi
# GC_Po
# GC_Ps
# GC_Sc
# GC_Sk
# GC_Sm
# GC_So
# GC_Zl
# GC_Zp
# GC_Zs
