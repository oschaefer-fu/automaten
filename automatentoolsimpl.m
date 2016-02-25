|| -----------------------------
|| (c) Oliver Schäfer, FU-Berlin
|| -----------------------------
%include <lwb/ansiseq>
%include <lwb/automaten>
%export unreachable reduce deleps equivalent powersetConstr minimize
        writeToMirandaFile showConfigurations

showConfigurations a w
  = headline ++ "\n" ++ sC [] w [0]
    where
    width = max2 (#w) (#"Restwort")
    fill  = #"Restwort" - #w, if #w <#"Restwort"
          = 0,                otherwise
    headline = rjustify (width+3) "Restwort" ++ " | " ++ "    Zustand" ++ "\n" ++
               rep (width+3) '-'             ++ "-+-" ++ rep 11 '-'
    sC zs' zs     []  = rep (fill+3) ' ' ++ vfaerben zs' Hellblau ++ zs     ++ " | " ++ vfaerben "--- Sackgasse" Rot
    sC zs' []     qs' = rep (fill+3) ' ' ++ vfaerben zs' Hellblau ++           " | " ++ showStates False qs' ++ "\n"
    sC zs' (z:zs) qs' = rep (fill+3) ' ' ++ vfaerben zs' Hellblau ++ (z:zs) ++ " | " ++ showStates True  qs' ++ "\n" ++
                        sC (zs' ++ [z]) zs qs''
                        where
                        qs'' = (sort . mkset . concat) [map fst (startFrom a q [z])|q<-qs']
    showStates t (x:xs)
      = vfaerben (rjustify 3 ("q" ++ shownum x) ++ " " ++ stat x) (color x t) ++ showStates' t xs
        where
        showStates' t []     = []
        showStates' t (x:xs) = "\n" ++ "   " ++ rep width ' ' ++ " | " ++
                               vfaerben (rjustify 3 ("q" ++ shownum x) ++ " " ++ stat x) (color x t) ++
                               showStates' t xs
        stat x = "Inner",  if styp x = Inner
               = "Start",  if styp x = Start
               = "Accept", if styp x = Accept
        color x t = Hellblau,  if  t
                  = Rot,       if ~t & (styp x = Inner \/ styp x = Start)
                  = Gruen,     if ~t &  styp x = Accept
        styp x = (snd . hd) [q|q<-states a;fst q = x]

writeToMirandaFile a name file mtype
  = [Tofile file astring]
    where
    astring = preambel ++
              name ++ " = " ++ atype ++ qsshow (show (states a)) ++ "\n" ++
              tab1 ++ "   " ++ tab2  ++ show   (sigma a)         ++ "\n" ++
              tab1 ++ "   " ++ tab2  ++ tsshow (tsort' (transitions a))
    tab1 = rep (#name) ' '
    tab2 = rep (#atype) ' '
    qsshow []     = error "Automat ohne Zustände (writeToMirandaFile)"
    qsshow (c:cs) = qsshow' (c:cs) []
    qsshow' []           akk = akk
    qsshow' (')':',':cs) akk = qsshow' cs (akk ++ "),\n" ++ tab1 ++ "   " ++ tab2 ++ " ")
    qsshow' (c:cs)       akk = qsshow' cs (akk ++ [c])
    tsshow []     = error "Automat ohne Übergänge (writeToMirandaFile)"
    tsshow (t:ts) = (("["++) . (++"]")) (tsshow' ts t)
    tsshow' [] t = tshow t
    tsshow' ((qi,zs,qj):ts) (qi',zs',qj')
      = tsshow' ts (qi',zs'++zs,qj'),                        if qi = qi' & qj  = qj'
      = tshow (qi',zs',qj') ++ "," ++ tsshow' ts (qi,zs,qj), if qi = qi' & qj ~= qj'
      = tshow (qi',zs',qj') ++ ",\n" ++ tab1 ++ "   " ++ tab2 ++ " " ++ tsshow' ts (qi,zs,qj), otherwise
    tshow (x,y,z) = "(" ++ shownum x ++ "," ++ tochar y ++ "," ++ shownum z ++ ")"
    tochar x = [decode 34] ++ x ++ [decode 34]
    preambel = "%include <lwb/automaten>\n\n", if mtype
             = "",                             otherwise
    atype = "dea ",                                                if isdea a
          = "nea ",                                                if isnea a
          = "neaE ",                                               if isneaE a
          = error "Automatentyp undefiniert (writeToMirandaFile)", otherwise

tsort' = foldr tinsert []
         where
         tinsert (qi,z,qj) []                = [(qi,z,qj)]
         tinsert (qi,z,qj) ((qi',z',qj'):ts) = (qi,z,qj):(qi',z',qj'):ts,         if qi < qi'
                                             = (qi,z,qj):(qi',z',qj'):ts,         if qi = qi' & qj < qj'
                                             = (qi,z,qj):(qi',z',qj'):ts,         if qi = qi' & qj = qj' & z < z'
                                             = (qi',z',qj'):tinsert (qi,z,qj) ts, otherwise

powersetConstr = powersetConstr' . deleps
powersetConstr' a
  = dea qs'' zs'' ts''
    where
    qs'' = pstates (fst ps)
    zs'' = sigma a
    ts'' = snd ps
    pstates qs = map f qs
                 where f (0,[0]) = (0,(snd . hd) (filter ((=0) . fst) (states a)))
                       f (n,qs)  = (n,Inner),  if [q'|q'<-(states a);q<-qs;snd q' = Accept;fst q'=q] = []
                                 = (n,Accept), otherwise
    || Qi: (i,[i]),         falls qi<-states a
    || Qi: (i,[n1,n2,...]), falls mehrelementig
    ||              Qi        Ti Startindex mehrelementiger q's Alphabet Liste zu verarbeitender Anfangszustände
    ps   = powerset [(0,[0])] [] (max (map fst (states a)) + 1) sigma'   [[0]]
    powerset qs ts n zs     []     = (qs,ts)
    powerset qs ts n []     (m:ms) = powerset qs      ts      n     sigma' ms
    powerset qs ts n (z:zs) (m:ms) = powerset (m':qs) (t':ts) n     zs     ((m:ms) ++ [q']), if ~member qs' q' & #q'  = 1
                                   = powerset (m':qs) (t':ts) (n+1) zs     ((m:ms) ++ [q']), if ~member qs' q' & #q' ~= 1
                                   = powerset qs      (t':ts) n     zs     (m:ms),           otherwise
                                     where
                                     qi' = hd [k|(k,m'')<-qs;m''=m]
                                     qj' = fst m',                    if ~member qs' q'
                                         = hd [k|(k,m'')<-qs;m''=q'], otherwise
                                     t' = (qi',z,qj')
                                     m' = (nr q',q'), if #q' = 1
                                        = (n,    q'), otherwise
                                     q' = delta' m z
                                     nr [q] = q
                                     qs' = map snd qs
    sigma' = foldr ((:) . wrap) [] (sigma a)
             where wrap x = [x]
    delta' qs z = (sort . mkset . concat) [delta q z|q<-qs]
                  where
                  delta q z = map thd3 (filter (f q z) (transitions a))
                  f qi zi (qi',zi',qj') = qi=qi' & zi=zi'
                  thd3 (x,y,z) = z

minimize' a = error "Automat kein DEA (minimize)",         if ~isdea a
            = a,                                           if equivalent a = []
            = dea (states a') (sigma a') (transitions a'), otherwise
              where
              a' = foldl mergeStates a (equivalent a)

minimize = reduce . minimize'

|| mergeStates a (qi,qj) fasst die beiden Zustände qi und qj zusammen. Das ist
|| nur (sinnvoll) möglich, wenn qi und qj äquivalent sind. In diesem Fall wird
|| der Zustand mit dem größeren Index gelöscht und der kleinere beibehalten.
|| Die Übergänge werden so modifiziert, dass Verweise auf die größere id in
|| Verweise auf die kleinere id geändert werden.
|| Diese Funktion kann nicht nach außen 'gereicht' werden, da beim Verschmelzen
|| nur eines äquivalenten Zustandspaares ein Zwischen-Automat entstehen kann,
|| der kein DEA ist.
mergeStates :: automat -> (num,num) -> automat
mergeStates a (qi,qj)
  = nea qs' zs' ts'
    where
    qs' = filter ((~=qi') . fst) (states a)
    zs' = sigma a
    ts' = mkset (map f (transitions a))
    f (qa,z,qb) = (qj',z,qj'), if qa=qi' & qb=qi'
                = (qj',z,qb),  if qa=qi'
                = (qa,z,qj'),  if qb=qi'
                = (qa,z,qb),   otherwise
    qi' = max2 qi qj
    qj' = min2 qi qj

|| equivalent a liefert eine Liste von Paaren äquivalenter Zustände (qi,qj)
|| (i<j), die ggf. leer sein kann.
|| Implementierungsidee: Man markiert zuerst alle Paare (i,i) als equivalent
|| und alle Paare (i,j) mit i<j als nicht äquivalent, wenn einer der beiden
|| Zustände in der Menge der Endzustände ist und der andere nicht.
|| Nun wird für alle nicht markierten Zustandspaare untersucht, ob für
|| irgendeinen Buchstaben z des Alphabets das Paar (qi',qj') als äquivalent
|| markiert werden kann, wobei (qi,z,qi') und (qj,z,qj') Übergänge des Automaten
|| sind. Dieser Schritt wird solange wiederholt, bis sich die Menge der Markierungen
|| nicht mehr ändert. Bis zu diesem Zeitpunkt als Unknown markierte Paare sind
|| zueinander äquivalent.
equivalent a
  = error "Automat ist kein DEA (equivalent)",                   if ~isdea a
  = equivalent' a (split (sigma a)) neqListe unListe unListe [], otherwise
    where
    || Akkubelegung:
    ||  zs: Alphabetbuchstaben
    ||  ns: nicht-äquivalenter Paare
    ||  us: noch unverarbeitete Paare
    ||  qs: unverarbeitet (aktueller Durchlauf durch Zeichenliste)
    || qs': im aktuellen Durchlauf bereits getestet und nicht für nicht-äquivalent in diesem Schritt befunden
    equivalent' a zs     ns us []     []  = []
    equivalent' a zs     ns us []     qs' = qs',                                      if us=qs'
                                          = equivalent' a sigma' ns     qs' qs'   [], otherwise
    equivalent' a []     ns us (q:qs) qs' = equivalent' a sigma' ns     us qs     (qs' ++ [q])
    equivalent' a (z:zs) ns us (q:qs) qs' = equivalent' a sigma' (q:ns) us qs     qs', if member ns (nQP q z)
                                          = equivalent' a zs     ns     us (q:qs) qs', otherwise

    neqListe  = [(qi,qj)|qi<-accepted;qj<-naccepted;qi>qj] ++ [(qi,qj)|qi<-naccepted;qj<-accepted;qi>qj]
    unListe   = [(qi,qj)|qi<-accepted;qj<-accepted;qi>qj] ++ [(qi,qj)|qi<-naccepted;qj<-naccepted;qi>qj]
    accepted  = map fst (filter (( =Accept) . snd) (states a))
    naccepted = map fst (filter ((~=Accept) . snd) (states a))
    sigma'    = split (sigma a)
    split = foldr ((:) . wrap) []
            where wrap x = [x]
    nQP (qi,qj) z = (qi',qj'), if qi'>qj'
                  = (qj',qi'), otherwise
                    where
                    qi' = (thd3 . hd) (filter (delta qi z) (transitions a))
                    qj' = (thd3 . hd) (filter (delta qj z) (transitions a))
                    delta q z (qi,z',qj) = q=qi & z=z'
                    thd3 (a,b,c) = c

|| Konvertierung eines NEAe in einen NEA ohne epsilon-Übergänge
|| Implementierungsidee: Die Nachfolgezustände eines über einen epsilon-Übergang
|| mit einem Zustand qi verbundenen Zustand qj werden um die von qi ergänzt. Au-
|| ßerdem müssen alle Zustände, die über einen epsilon-Übergang mit einem akzep-
|| tierenden Zustand verbunden sind, ebenfalls akzeptierende Zustände werden.
deleps a = error "Automat ist ein DEA (deleps)",        if isdea a
||         = error "Automat hat keine Epsilon-Übergänge", if hasNoEps
         = nea qs' zs ts',                              otherwise
           where
           (qs,zs,ts) = (states a,sigma a,transitions a)
||           hasNoEps = [(qi,z,qj)|(qi,z,qj)<-ts;z=""] = []
           ts' = filter notEps (ts ++ epsconnect a)
||           qs' = mkset ([(qj,Accept)|qj<-q0'] ++ foldr f qs q0' -- [(0,Start)])
           qs' = mkset ([(qj,Accept)|qj<-q0'] ++ foldr f qs q0')
           q0' = epsaccept a
           f qi qs = filter ((qi~=) . fst) qs

eps,notEps :: transition -> bool
eps (qi,z,qj) = z = ""
notEps = (~) . eps

|| Suche alle Zustände, die über einen Epsilon-Übergang mit einem akzeptierenden
|| Zustand verbunden sind.
epsaccept :: automat -> [num]
epsaccept  a    = epsaccept' a [q|(q,Accept)<-states a]
epsaccept' a fs
  = epsaccept' a fs', if fs ~= fs'
  = fs,               otherwise
    where
    fs' = mkset (fs ++ [q|(q,t)<-states a;(q,"",qj1)<-transitions a;qj2<-fs;qj1=qj2])

|| Erstelle eine Liste von (neuen) Übergängen (qi,z,qj'), für die gilt, dass es
|| Epsilon-Übergänge (qi,"",qj) und Übergänge (qj,z,qj') gibt.
epsconnect :: automat -> [transition]
epsconnect  a = epsconnect' a (filter ((~).notEps) (transitions a)) []
epsconnect' a []              akk = akk
epsconnect' a ((qi,"",qj):ts) akk
  = epsconnect' a ts (akk ++ ts')
    where
    ts' = concat [map (f qi) (filter ((=q) . fst3) (transitions a))|q<-epsShape a qi]
    fst3 (x,y,z) = x
    f qi (x,y,z) = (qi,y,z)

epsShape :: automat -> num -> [num]
epsShape  a q  = epsShape' a [q] -- [q]
epsShape' a qs = qs,              if qs' = qs
               = epsShape' a qs', otherwise
                 where
                 qs' = (sort . mkset . concat) (map f qs)
                 f q = [qj|(qi,"",qj)<-transitions a;q=qi] ++ [q]

reachable :: automat -> [num]
reachable a
  = reachable' a (sigma a) [0]    []                [0]             (#(states a)-1)
||                         Start  aktuell erreicht  alle erreichten max. Wortlänge
    where
    alle = sort (map fst (states a))
    reachable' a (z:zs) (q:qs) akt all (n+1)
      = reachable' a zs (q:qs) (insert q' akt) all (n+1)
        where
        q' = (fst . hd) (startFrom a q [z])
        insert x []     = [x]
        insert x (y:ys) = x:y:ys,        if x < y
                        = y:insert x ys, if x > y
                        = y:ys,          if x = y
    reachable' a [] (q:qs) akt all (n+1)
      = reachable' a (sigma a) qs akt all (n+1)
    reachable' a (z:zs) [] akt all (n+1)
      = reachable' a (sigma a) akt [] (all++akt) n, if (sort . mkset) (all++akt) ~= alle
      = alle,                                       otherwise
    reachable' a (z:zs) (q:qs) akt all 0 = mkset all

unreachable a = [n|(n,t)<-states a] -- (reachable a)

reduce a = foldl deleteState a (unreachable a)
