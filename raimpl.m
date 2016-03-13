|| ------------------------------------------------
|| Reguläre Ausdrücke (c) Oliver Schäfer, März 2016
|| ------------------------------------------------

%include <lwb/automaten>
%include <lwb/automatentools>

ra == (automat,[char])

alpha_ = select "[:alpha:]"  letters
digit_ = select "[:digits:]" digits
alnum_ = select "[:alnum:]"  (digits ++ letters)
punct_ = select "[:punct:]"  ".,:;!?"
upper_ = select "[:upper:]"  capitals
lower_ = select "[:lower:]"  small

raMaxN n r
  = (fst r', "(" ++ snd r ++ "){," ++ shownum n ++ "}"), if #(snd r) > 1
  = (fst r',        snd r ++  "{," ++ shownum n ++ "}"), otherwise
    where
    r' = raOr ([ra0or1 r] ++ [raAnd (rep k r)|k<-[2..n]])

raMinN n r = (fst r', f (snd r))
             where
             f name =        name ++  "{" ++ shownum n ++ ",}", if #name < 2 \/ bekannt name
                    = "(" ++ name ++ "){" ++ shownum n ++ ",}", otherwise
             r' = raStar r,                              if n = 0
                = raPlus r,                              if n = 1
                = raAnd [raAnd (rep (n-1) r), raPlus r], otherwise

raN n r
  = (fst r',        snd r ++  "{" ++ shownum n ++ "}"), if #(snd r) < 2 \/ bekannt (snd r)
  = (fst r', "(" ++ snd r ++ "){" ++ shownum n ++ "}"), otherwise
    where
    r' = raAnd (rep n r)

raMN m n r
  = (fst r',        snd r ++  "{" ++ shownum m ++ "," ++ shownum n ++ "}"), if #(snd r) < 2 \/ bekannt (snd r)
  = (fst r', "(" ++ snd r ++ "){" ++ shownum m ++ "," ++ shownum n ++ "}"), otherwise
    where
    r' = raOr [raAnd (rep k r)|k<-[m..n]]

raPlus (r,name)
  = (fst (raAnd [(r,""),raStar (r,name)]),        name ++  "+"), if #name < 2 \/ bekannt name
  = (fst (raAnd [(r,""),raStar (r,name)]), "(" ++ name ++ ")+"), otherwise

ra0or1 (r,name)
  = (ra0or1' r,        name ++  "?"), if #name < 2 \/ bekannt name
  = (ra0or1' r, "(" ++ name ++ ")?"), otherwise
    where
    ra0or1' a
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
        ts' = map (shiftT 1) tsA ++ [(0,"",1),(0,"",n)] ++ [(k+1,"",n)|(k,Accept)<-qsA]
        shiftT x (qi,z,qj) = (qi+x,z,qj+x)

raStar (r,name)
  = (raStar' r,        name ++  "*"), if #name < 2 \/ bekannt name
  = (raStar' r, "(" ++ name ++ ")*"), otherwise
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

raOr rs
  = (foldr1 raOr' (map fst rs), "(" ++ (concat . (intersperse "|") . (map snd)) rs ++ ")")
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

raAnd rs
  = (foldr1 raAnd' (map fst rs), (concat . (intersperse "") . (map snd)) rs)
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

compose name xs
  = (sucher xs, name), if name ~= ""
  = (sucher xs, xs),   otherwise
    where
    sucher b = nea qs zs ts
               where 
               qs = [(i,Inner)|i<-[1..#b-1]] ++ [(#b,Accept)]
               zs = ascii
               ts = [(i,[b!i],i+1)|i<-[0..#b-1]]

select name xs
  = (nea qs zs ts, name),  if name ~= ""
  = (nea qs zs ts, name'), otherwise
    where
    name' = (("("++) . (++")")) (intersperse '|' xs), if #xs > 1
          = xs, otherwise
    qs = [(0,Start)] ++ [(i,Accept)|i<-[1..#xs]]
    zs = ascii
    ts = [(0,[xs!i],i+1)|i<-[0..#xs-1]]

showra = snd

|| ------------------------ Hilfsfunktionen -----------------------------
intersperse :: * -> [*] -> [*]
intersperse x xs = (tl . concat) [[x,y]|y<-xs]

bekannt :: [char] -> bool
bekannt name = True,  if hd name = '[' & last name = ']'
             = True,  if hd name = '(' & last name = ')'
             = False, otherwise
