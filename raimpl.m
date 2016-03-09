|| ------------------------------------------------
|| Reguläre Ausdrücke (c) Oliver Schäfer, März 2016
|| ------------------------------------------------

%include <lwb/automaten>
%include <lwb/automatentools>

ra == (automat,[char])

alpha_ = oneOf "[:alpha:]"  letters
digit_ = oneOf "[:digits:]" digits
alnum_ = oneOf "[:alnum:]"  (digits ++ letters)
punct_ = oneOf "[:punct:]"  ".,:;!?"
upper_ = oneOf "[:upper:]"  capitals
lower_ = oneOf "[:lower:]"  small

raPlus (a,n)
  = (fst (raAnd (a,"") (raStar (a,n))), "(" ++ n ++ ")+")

||ra0or1 (a,n)
||  = (ra0or1' a, "(" ++ n ++ ")?")

raStar (a,n)
  = (raStar' a, "(" ++ n ++ ")*")
    where
    raStar' a
      = neaE qs' zs' ts'
        where
        qs' = map (shiftQ 1 . chgStart . chgAccept) qsA ++ [(n,Accept)]
        qsA = states a
        n   = max (map fst qsA) + 2
        shiftQ x (q,t) = (q+x,t)
        chgAccept (n,Accept) = (n,Inner)
        chgAccept (n,t)      = (n,t)
        chgStart  (0,Start)  = (0,Inner)
        chgStart  (n,t)      = (n,t)
        zs' = sigma a
        tsA = transitions a
        ts' = map (shiftT 1) tsA ++ [(0,"",1),(0,"",n),(n,"",0)] ++ [(k+1,"",n)|(k,Accept)<-qsA]
        shiftT x (qi,z,qj) = (qi+x,z,qj+x)

raOr (a1,n1) (a2,n2)
  = (raOr' a1 a2, "(" ++ n1 ++ "|" ++ n2 ++ ")")
    where
    raOr' a b
      = neaE qs' zs' ts'
        where
        qs' = (map (shiftQ 1) . map chgStart) qsA ++ (map (shiftQ n) . map chgStart) qsB
        qsA = states a
        qsB = states b
        n   = max (map fst qsA) + 2
        shiftQ x (q,t) = (q+x,t)
        chgStart (0,Start) = (0,Inner)
        chgStart (n,t)     = (n,t)
        zs' = (sort . mkset) (sigma a ++ sigma b)
        tsA = transitions a
        tsB = transitions b
        ts' = map (shiftT 1) tsA ++ map (shiftT n) tsB ++ [(0,"",1),(0,"",n)]
        shiftT x (qi,z,qj) = (qi+x,z,qj+x)

raAnd (a1,n1) (a2,n2)
  = (raAnd' a1 a2, "(" ++ n1 ++ n2 ++ ")")
    where
    raAnd' a b
      = neaE qs' zs' ts'
        where
        qs' = map chgAccept qsA ++ (map (shiftQ n) . map chgStart) qsB
        qsA = states a
        qsB = states b
        n   = max (map fst qsA) + 2
        shiftQ x (q,t) = (q+x,t)
        chgAccept (n,Accept) = (n,Inner)
        chgAccept (n,t)      = (n,t)
        chgStart  (0,Start)  = (0,Inner)
        chgStart  (n,t)      = (n,t)
        zs' = (sort . mkset) (sigma a ++ sigma b)
        tsA = transitions a
        tsB = transitions b
        ts' = tsA ++ map (shiftT n) tsB ++ [(k,"",n)|(k,Accept)<-qsA]
        shiftT x (qi,z,qj) = (qi+x,z,qj+x)

matches = accepts . fst

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
oneOf name wort
  = (nea qs zs ts, name)
    where
    qs = [(0,Start)] ++ [(i,Accept)|i<-[1..#wort]]
    zs = ascii
    ts = [(0,[wort!i],i+1)|i<-[0..#wort-1]]

showra (a,name) = name
