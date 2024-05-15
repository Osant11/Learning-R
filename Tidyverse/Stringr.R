# String R ----
https://stringr.tidyverse.org/articles/regular-expressions.html
https://r4ds.had.co.nz/strings.html

# Regular expressions = Concise and flexible tool for describing patterns in strings


library( tidyverse )

str_extract( fruit, "nana" )
# Is shorthand for
str_extract( fruit, regex( "nana" ) )


## BASIC MATCHES ----
x <- c( "apple", "banana", "pear" )
str_extract( x, "an" )
str_view( x, "an" )

# Case sensitive : ignore_case = TRUE
bananas <- c( "banana", "Banana", "BANANA" )
str_detect( bananas, "banana" )
str_detect( bananas, regex( "banana", ignore_case = TRUE ) )

# "." -> Matches any character except a new line : 
str_extract( x, ".a." )
str_extract( x, "a." )
str_view( x, "a." )

# "." -> Match everything, including \n with dotall = TRUE
str_detect( "\nX\n", ".X." )
str_detect( "\nX\n", regex( ".X.", dotall = TRUE ) )


## ESCAPING ----
# How to match "." ? -> "\" to escpace special behaviour of regex but also used a escape symbol in strings => "\\."
dot <- "\\."
writeLines( dot )

str_extract( c( "abc", "a.c", "bef", "a.csd" ), "a\\.c" )
str_view( c( "abc", "a.c", "bef", "a.csd" ), "a\\.c" )

# How to macth "\" ? -> Regular expression "\\" and also needs to escape with "\" => "\\\\"
x <- "a\\b"
writeLines( x )

str_extract( x, "\\\\" )
writeLines( str_extract( x, "\\\\" ) )


## ALTERNATION ----
# "|" -> alternation operator => pick one or more possible matches : "abc|def" will match "abc" or "def"
# "abc|def" is equivalent to "(abc)|(def)" not "ab(c|d)ef"
str_detect( c( "abc", "def", "ghi"), "abc|def" )


## GROUPING ----
# Parenthesis to override the default precedence rules 
str_extract( c( "grey", "gray" ), "gre|ay" )
str_extract( c( "grey", "gray" ), "gr(e|a)y" )


## ANCHORS ----
# By default regex will match any part of a string. -> Anchor to match the start or the end of a string 
# "^" -> Start of string / "$" -> End of string
# To match a literal "$" or "^", you need to escape them, "\$", and "\^".
x <- c( "apple", "banana", "pear", "ban^ana$" )
str_extract( x, "^a" )
str_extract( x, "a$" )
str_extract_all( x, "\\^|\\$" )

# For multiline strings -> "regex( multiline = TRUE )" -> new rules : 
# "^": Start of each line / "$": End of each line / "\A": Start of the input / "\z": End of the input / "Z": End of input but before final line terminator if exists
x <- "Line 1\nLine 2\nLine 3\n"
x
str_extract_all( x, "^Line.." )[[ 1 ]]
str_extract_all( x, regex( "^Line..", multiline = TRUE ) )[[ 1 ]]
str_extract_all( x, regex( "\\ALine..", multiline = TRUE ) )[[ 1 ]]


## REPETITION ----
# To control how many times a pattern matches :
# "?": 0 or 1 / "+": 1 or more / "*": 0 or more 
x <- "1888 is the longest year in Roman numerals: MMDDDDCCCCCCCCCLLLXXXVIII"
str_extract( x, "CC?" )
str_extract( x, "M+D+C+" )
str_extract( x, "CC?L" )
str_extract( x, "M+D+C+L" )
str_extract( x, "F?K?VI" )
str_extract( x, "CLX+" )
str_extract( x, 'C[LX]+' )
str_extract( x, '[CLX]+' )
str_extract( x, 'C(LX)+' )
str_extract( x, '(CLX)+' )

# Number of precise matches : 
# "{n}": Exactly n / "{n,}": n or more / "{n,m}": Between n and m 
str_view( x, "C{2}" )
str_extract( x, "C{2}" )
str_extract( x, "C{4,}" )
str_extract( x, "C{1,2}" )

# Matches possessive with "+" after -> Repetition not be re-tried
# "?+": 0 or 1, possessive / "++": 1 or more possessive / "*+": 0 or more, possessive 
# "{n}+": exactly n, possessive  / "{n,}+: n or more, possessive" / "{n,m}+": Between n and m, possessive


### PREBUILT CLASSES ----
# These all go inside the [] for character classes, i.e. [[:digit:]AX] matches all digits, A, and X

[abc] # matches a, b, or c
[a-z] # matches every character between a and z 
[^abc] # matches anything except a, b, or c
[\^\-] # matches ^ or - 
[:punct:] # punctuation
[:alpha:] # letters
[:lower:] # lowercase letters
[:upper:] # uppercase letters
[:digit:] # digits
[:xdigit:] # hex digits
[:alnum:] # letters and numbers
[:cntrl:] # control characters
[:graph:] # letters, numbers and punctuation
[:print:] # letters, numbers, punctuation and whitespace
[:space:] # space characters ( basically equivalent to \s )
[:blank:] # space and tab

str_extract( x, "[:digit:]+" )
