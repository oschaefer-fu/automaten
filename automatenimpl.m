|| ----------------------------------------------------------------------------
|| Beginn der Implementierung des ADT automat               (c) O.S. 18.02.2016
|| ----------------------------------------------------------------------------

%include <lwb/ansiseq> || Farben und weitere Schriftattribute in der Konsole

||              Q        Sigma      delta         F       q0    Automatentyp
automat == ([state],[[character]],[transition],[state],state,atyp)
atyp ::= DEA | NEA | NEAe

isdea a = sort [(qi,z)|(qi,ti)<-qs;z<-zs] = sort [(qi,z)|(qi,z,qj)<-ts]
          where (qs,zs,ts,fs,q0,t) = a

isnea a = t = NEA
          where (qs,zs,ts,fs,q0,t) = a

isneaE a = t = NEAe
           where (qs,zs,ts,fs,q0,t) = a

dea  = newAutomat DEA
nea  = newAutomat NEA
neaE = newAutomat NEAe

newAutomat t qs zs ts
  = error "Nur q0 darf Startzustand sein",         if (types Start qs -- [(0,Start)]) ~= []
  = error "Zustandsliste unvollständig",           if ~consistent qs' ts'
  = error "Übergangsfunktion ungültig",            if ~and [subset z zs|(qi,z,qj)<-ts]
  = error "Epsilon-Übergänge nur in NEAe erlaubt", if hasEPS ts & t ~= NEAe
  = (qs',zs',ts',fs',q0,t),                        otherwise
    where
    qs' = qsort qs,             if #(filter ((=0) . fst) qs) > 0
        = qsort ((0,Start):qs), otherwise
    zs' = (f . sort . split) zs
          where f = ("":), if t = NEAe
                  = id,    otherwise
    ts' = tsort t ts
    fs' = types Accept qs'
    q0  = hd (filter ((=0) . fst) qs')

accepts a ws = or ((map ((=Accept) . snd)) (startFrom a 0 ws))

startFrom a n ws
  = error "Automat ohne Endzustand",                      if fs = []
  = error "Angegebener Zustand existiert nicht",          if ~member (map fst qs) n
  = error "Eingegebenes Wort nicht in Kleen'scher Hülle", if ~inKleen (split ws) zs
  = mkset (step ts (split ws) [n]),                       otherwise
    where
    (qs,zs,ts,fs,q0,t)    = a
    inKleen []     zs     = True
    inKleen (w:ws) []     = False
    inKleen (w:ws) (z:zs) = inKleen (filter (~=z) (w:ws)) zs
    epsShape qis = [qj|n<-qis;(qi,z,qj)<-ts;z="";qi=n]
    step ts ws     []  = []
    step ts []     ixs = [(n,t)|(n,t)<-qs;i<-(ixs ++ epsShape ixs);i=n]
    step ts (w:ws) ixs = step ts ws followQs ++ step ts (w:ws) (epsShape ixs)
                         where
                         followQs = (mkset . concat) [nextQ ts n w|n<-ixs]
 
addTransition    a (qi,z,qj) = modifyTransition a (qi,z,qj) True
changeTransition a (qi,z,qj) = modifyTransition a (qi,z,qj) False

modifyTransition a (qi,z,qj) canADD
  = error "Übergangssymbol kein Element des Alphabets", if  ~member (concat zs) (hd z)
  = error "Zustandsnummer(n) gibt es nicht",            if  ~subset [qi,qj] (map fst qs)
  = error "Übergang (qi,z,_) bereits vorhanden",        if  canADD & transList ~= []
  = error "Übergang (qi,z,_) existiert noch nicht",     if ~canADD & transList  = []
  = dea  qs' (concat zs) ts',                           if t = DEA  & ~member ts (qi,z,qj)
  = nea  qs' (concat zs) ts',                           if t = NEA  & ~member ts (qi,z,qj)
  = neaE qs' (concat zs) ts',                           if t = NEAe & ~member ts (qi,z,qj)
  = error "unvorhergesehener Fall in addTransition",    if  canADD
  = error "unvorhergesehener Fall in changeTransition", if ~canADD
    where
    (qs,zs,ts,fs,q0,t) = a
    transList = [(a,b,c)|(a,b,c)<-ts;a=qi;b=z]
||    qs' = filter ((~= Start) . snd) qs
    qs' = qs
    ts' = tsort t ((qi,z,qj):(ts -- transList))

deleteTransition a qi z
  = dea  qs' (concat zs) ts',                            if t = DEA
  = nea  qs' (concat zs) ts',                            if t = NEA
  = neaE qs' (concat zs) ts',                            if t = NEAe
  = error "unvorhergesehener Fall in deleteTransistion", otherwise
    where
    (qs,zs,ts,fs,q0,t) = a
||    qs' = filter ((~= Start) . snd) qs
    qs' = qs
    ts' = [(qi',z',qj')|(qi',z',qj')<-ts;~(qi'=qi & z'=z)]

addState    a (qnum,qtype) = modifyState a qnum qtype True
changeState a  qnum qtype  = modifyState a qnum qtype False

modifyState a qnum qtype canADD
  = error "Index des Startzustandes muss 0 sein",         if  canADD & qtype = Start & qnum ~= 0
  = error "Zustand mit gleichem Index bereits vorhanden", if  canADD & member (map fst qs) qnum
  = error "Automat hat bereits einen Startzustand (q0)",  if ~canADD & qtype = Start
  = error "Zustand mit diesem Index nicht vorhanden",     if ~canADD & ~member (map fst qs) qnum
  = dea  qs' (concat zs) ts,                              if t = DEA
  = nea  qs' (concat zs) ts,                              if t = NEA
  = neaE qs' (concat zs) ts,                              if t = NEAe
  = error "unvorhergesehener Fall in addState",           if  canADD
  = error "unvorhergesehener Fall in changeState",        if ~canADD
    where
    (qs,zs,ts,fs,q0,t) = a
    qs' = qsort ((qnum,qtype):filter ((~=qnum) . fst) (qs -- [(0,Start)]))

deleteState a n
  = error "Startzustand lässt sich nicht löschen", if n = 0
  = dea  qs' (concat zs) ts',                      if t = DEA
  = nea  qs' (concat zs) ts',                      if t = NEA
  = neaE qs' (concat zs) ts',                      if t = NEAe
  = error "unvorhergesehener Fall in deleteState", otherwise
    where
    (qs,zs,ts,fs,q0,t) = a
||    qs' = filter ((~=n) . fst) (qs -- [(0,Start)])
    qs' = filter ((~=n) . fst) qs
    ts' = filter p ts
          where p (qi,z,qj) = qi ~= n & qj ~= n

states      (qs,zs,ts,fs,q0,t) = qs
sigma       (qs,zs,ts,fs,q0,t) = concat zs
transitions (qs,zs,ts,fs,q0,t) = ts

showAccepted ads ws
  = "   " ++ pl  "Wort" ++ " | " ++  ds            ++ "\n" ++
    "   " ++ rep ml '-' ++ "-+-" ++  rep (#ds) '-' ++ "\n" ++
    showAccepted' as ws pl pr
    where
    as = map fst ads
    ds = concat (map (pr . snd) ads)
    ml = max (map (#) ws)
    mr = max2 (#"nein") (max (map ((#) . snd) ads))
    pl = rjustify ml
    pr = (" "++) . (++" ") . (ljustify mr)
    showAccepted' as []     pl pr = []
    showAccepted' as (w:ws) pl pr
      = "   " ++ pl w ++ " | " ++ concat (map (pr . f) as) ++ "\n" ++ showAccepted' as ws pl pr
        where
        f a = "ja  ", if accepts a w
            = "nein",   otherwise

|| Die show-Funktion stellt den  Automaten in Tabellenform dar, in  Analogie zu
|| der in der Vorlesung verwendeten Weise und wie es in der einschlägigen Lite-
|| ratur verbreitet ist. Dabei werden Endzustände mit  einem '*' und der Start-
|| zustand durch ein vorangestelltes '->' gekennzeichnet. Die Epsilon-Übergänge
|| erscheinen am Ende der Tabelle unterhalb von '{}'.
|| Die show-Funktion geht davon aus, dass es nicht mehr als 100 Zustände q0 bis
|| q99 gibt.
showautomat a
  = heading ++ showA qs zs ts
    where
    (qs,zs,ts,fs,q0,t) = a
    nn = 2, if max (map fst qs) < 10
       = 3, otherwise
    info  = " " ++ info1 ++ rep nn ' ' ++ info2 ++ " |"
    info1 = vfaerben "*"  Rot,   if fs = []
          = vfaerben "*"  Gruen, otherwise
    info2 = vfaerben aTYP Rot,   if ~isdea a & t = DEA
          = vfaerben aTYP Gruen, otherwise
            where aTYP = " DEA", if t = DEA
                       = " NEA", if t = NEA
                       = "NEAe", if t = NEAe
    heading = info ++ concat (map f zs) ++ "\n" ++
              " ------" ++ rep nn '-' ++ "+" ++ concat (rep (#zs) ("-" ++ rep nn '-')) ++ "-"  ++ "\n"
              where f z = " " ++ rjustify nn "{}", if z = ""
                        = rep nn ' ' ++ z,         otherwise
    showA []     zs ts = []
    showA (q:qs) zs ts
      = showQ q  zs (filter ((~) . (not q)) ts) True ++
        showA qs zs (filter        (not q)  ts)
        where
        not (qnum,qtyp) (qi,z,qj) = qnum ~= qi
    showQ q zs [] False = []
    showQ q zs ts isfirst
      = ftext ++ stext ++ qtext ++ "|" ++ trans ++ "\n" ++
        showQ q zs (dropfirst ts zs) False
        where
        ftext = " * ", if snd q = Accept & isfirst
              = "   ", otherwise
        stext = "-> ", if fst q = 0 & isfirst
              = "   ", otherwise
        qtext = rjustify nn ("q" ++ shownum (fst q)) ++ " ", if isfirst
              = rep      nn '.'                      ++ " ", otherwise
        trans = concat (myzip (takefirst ts zs) zs)
        myzip qs []     = []
        myzip [] (z:zs) = (" " ++ rep nn '-'):myzip [] zs, if isfirst
                        = (" " ++ rep nn '.'):myzip [] zs, otherwise
        myzip ((qi,z',qj):qs) (z:zs)
          = (" " ++ rjustify nn ("q" ++ shownum qj)):myzip qs zs, if z'  = z
          = (" " ++ rep nn '-'):myzip ((qi,z',qj):qs) zs,         if isfirst
          = (" " ++ rep nn '.'):myzip ((qi,z',qj):qs) zs,         otherwise
        dropfirst ts zs = ts -- takefirst ts zs
        takefirst ts zs = concat [first z ts|z<-zs]
        first z []              = []
        first z ((qi,z ,qj):ts) = [(qi,z,qj)]
        first z ((qi,z',qj):ts) = first z ts

|| ----------------------------------------------------------------------------------
|| Hilfsfunktionen (unterhalb der Schnittstelle, daher ohne Zugriff auf Typ automat)
|| ----------------------------------------------------------------------------------
nextQ :: [transition] -> num -> [character] -> [num]
nextQ ts qi z = map thd (filter p ts)
                where
                thd (k1,k2,k3) = k3
                p (qi',z',qj') = qi' = qi & z' = z

types :: typ -> [state] -> [state]
types t = filter ((=t) . snd)

qsort :: [state] -> [state]
qsort = foldr qinsert []
        where
        qinsert (n,t) [] = [(n,t)]
        qinsert (n,t) ((n',t'):qs)
          = (n,t):(n',t'):qs,                      if n < n'
          = (n',t'):qinsert (n,t) qs,              if n > n'
          = error "Automat mit doppeltem Zustand", otherwise

|| tsort zerlegt die Liste  der Übergänge in  eine mit epsilon-Übergängen (ts')
|| und eine mit den restlichen Übergängen (ts''), damit (sinnvolle) Eingaben in
|| der Form (qi,z1:z2:zs,qj) mögliche sind. Diese zerlegt splitT  in Übergänge,
|| die nur ein Zeichen konsumieren ((qi,z,qj)).
tsort :: atyp -> [transition] -> [transition]
tsort t ts
  = foldr tinsert [] (ts' ++ (splitT [] ts''))
    where
    (ts',ts'') = splitE ts [] []
    splitE []             akk1 akk2 = (akk1,akk2)
    splitE ((qi,z,qj):ts) akk1 akk2 = splitE ts ((qi,z,qj):akk1) akk2, if z = ""
                                    = splitE ts akk1 ((qi,z,qj):akk2), otherwise
    splitT akk []                = akk
    splitT akk ((qi,[],  qj):ts) = splitT akk ts
    splitT akk ((qi,z:zs,qj):ts) = splitT ((qi,[z],qj):akk) ((qi,zs,qj):ts)
    tinsert (qi,z,qj) [] = [(qi,z,qj)]
    tinsert (qi,z,qj) ((qi',z',qj'):ts)
      = (qi,z,qj):(qi',z',qj'):ts,         if qi < qi'
      = (qi',z',qj'):tinsert (qi,z,qj) ts, if qi > qi'
      = error "Automat ist kein DEA",      if qi = qi' & z = z' & t = DEA
      = (qi,z,qj):(qi',z',qj'):ts,         if qi = qi' & z < z'
      = (qi',z',qj'):tinsert (qi,z,qj) ts, if qi = qi' & z > z'
      = (qi,z,qj):(qi',z',qj'):ts,         if qi = qi' & z = z' & qj < qj'
      = (qi',z',qj'):tinsert (qi,z,qj) ts, if qi = qi' & z = z' & qj > qj'
      = (qi',z',qj'):ts,                   if qi = qi' & z = z' & qj = qj'
      = error "Fehler in splitT",          otherwise

|| consistent liefert genau dann True, wenn es keinen Übergang gibt, an dem ein
|| nicht existierender Zustand beteiligt ist.
consistent :: [state] -> [transition] -> bool
consistent qs ts
  = subset ts' qs'
    where
    qs' = map fst qs
    ts' = (mkset . concat) [[qi,qj]|(qi,z,qj)<-ts]

split :: [character] -> [[character]]
split = foldr ((:) . wrap) []
        where wrap x = [x]

subset :: [*] -> [*] -> bool
subset as bs = and [member bs a|a<-as]

hasEPS :: [transition] -> bool
hasEPS ts = [(a,b,c)|(a,b,c)<-ts;b=""] ~= []
