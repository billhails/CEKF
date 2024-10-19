import csv

with open('unicode/UnicodeData.txt', newline='') as csvin:
    reader = csv.reader(csvin, delimiter=';')
    i = 0
    print('/* 000000 */ ', end='')
    for row in reader:
        j = int(row[0], 16)
        while i < j:
            if i > 0 and i % 16 == 0:
                print('')
                print('/* {:06X} */ '.format(i), end='')
            if (i > 0xe000 and i < 0xf8ff) or (i > 0xf0000 and i < 0xffffd) or (i > 0x100000 and i < 0x10fffd):
                print("GC_Co, ", end='')
            else:
                print("GC_Cn, ", end='')
            i += 1
        i += 1
        if j > 0 and j % 16 == 0:
            print('')
            print('/* {:06X} */ '.format(j), end='')
        print(f"GC_{row[2]}, ", end='')
    print('')
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
