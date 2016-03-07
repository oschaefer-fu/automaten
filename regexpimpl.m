|| ------------------------------------------------
|| Reguläre Ausdrücke (c) Oliver Schäfer, März 2016
|| ------------------------------------------------

%include <lwb/automaten>
%include <lwb/automatentools>

regexp == automat

alpha_  = oneOf letters
digit_  = oneOf digits
star    = regexpSTAR
matches = accepts

|| 'allOf xs' liefert einen RA, der auf die durch xs repräsentierte Zeichenkette
|| matcht
allOf = sucher
        where
        sucher b = nea qs zs ts
                   where 
                   qs = [(i,Inner)|i<-[1..#b-1]] ++ [(#b,Accept)]
                   zs = ascii
                   ts = [(0,zs,0),(#b,zs,#b)] ++ [(i,[b!i],i+1)|i<-[0..#b-1]]

|| 'oneOf xs' liefert einen RA, der auf ein beliebiges Zeichen aus xs matcht.
oneOf xs = nea qs zs ts
           where
           qs = [(0,Start)] ++ [(i,Accept)|i<-[1..#xs]]
           zs = ascii
           ts = [(0,[xs!i],i+1)|i<-[0..#xs-1]]
