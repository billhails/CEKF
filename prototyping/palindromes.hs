respondPalindromes = unlines . map (\xs -> if isPalindrome xs then xs else "") . lines  
    where   isPalindrome xs = xs == reverse xs

main = interact respondPalindromes
