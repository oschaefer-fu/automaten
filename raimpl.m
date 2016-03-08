|| ------------------------------------------------
|| Reguläre Ausdrücke (c) Oliver Schäfer, März 2016
|| ------------------------------------------------

%include <lwb/automaten>
%include <lwb/automatentools>

ra == (automat,[char])

alpha_ = oneOf "[:alpha:]"  letters
digit_ = oneOf "[:digits:]" digits
upper_ = oneOf "[:upper:]"  capitals
lower_ = oneOf "[:lower:]"  small

raStar  (a,n)           = (regexpSTAR a,   "(" ++ n ++ ")*")
raOr    (a1,n1) (a2,n2) = (regexpOR a1 a2, "(" ++ n1 ++ "|" ++ n2 ++ ")")
raAnd   (a1,n1) (a2,n2) = (regexpOR a1 a2, "(" ++ n1 ++ n2 ++ ")")
matches (a,n)           = accepts a

|| 'allOf xs' liefert einen RA, der auf die durch xs repräsentierte Zeichenkette
|| matcht
allOf name wort
  = (sucher wort, name)
    where
    sucher b = nea qs zs ts
               where 
               qs = [(i,Inner)|i<-[1..#b-1]] ++ [(#b,Accept)]
               zs = ascii
               ts = [(0,zs,0),(#b,zs,#b)] ++ [(i,[b!i],i+1)|i<-[0..#b-1]]

|| 'oneOf xs' liefert einen RA, der auf ein beliebiges Zeichen aus xs matcht.
oneOf name xs
  = (nea qs zs ts, name)
    where
    qs = [(0,Start)] ++ [(i,Accept)|i<-[1..#xs]]
    zs = ascii
    ts = [(0,[xs!i],i+1)|i<-[0..#xs-1]]

showra (a,name) = name
