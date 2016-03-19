|| ========================= Miranda-Paket 'automaten' =======================
|| (c) Oliver Schäfer                                             WS 2015/2016
|| ===========================================================================
|| Erstellt in  Anlehnung  an das GO-Paket 'automaten'  aus  der Vorlesung zur
|| Theoretischen Informatik (SS 2014), um nach Umstellung der Kursstruktur von
|| einem 3-jährigen auf einen 2-jährigen Zyklus das Fehlen der notwendigen GO-
|| Kenntnisse zu umgehen.

|| Folgende Typen werden zur Zeit zur Verfügung gestellt:

|| Deterministische, endliche Automaten                                  (DEA)
|| Nicht-deterministische, endliche Automaten                            (NEA)
|| Nicht-deterministische, endliche Automaten mit Epsilon-Übergängen    (NEAe)

|| Zustände eines Automaten  sind durch ihre Nummer und einen Typ gekennzeich-
|| net. Es gibt drei  verschiedene Typen  von Zuständen: Den Startzustand, den
|| inneren Zustand (Inner) und den akzeptierenden Endzustand (Accept). Der Zu-
|| stand  mit der Nummer 0 ist IMMER der Startzustand (0,Start). Andere Start-
|| zustände sind nicht erlaubt.
|| Die Ausgabefunktion 'showautomat' vearbeitet  nur die 100 Zustände q0 - q99
|| 'sauber', bei Indizes >=100 erscheinen die Zustände der Tabelle u. U. nicht
|| mehr gefluchtet; der Korrektheit der Funktionen tut das keinen Abbruch.
typ ::= Inner | Accept | Start              || Für Anwender nur Inner | Accept
state == (num,typ)                          || Bsp.: (3,Inner) oder (0,Accept)

|| Alphabete setzen sich aus Zeichen vom Typ char  zusammen und haben  den Typ
|| [char]. Ein (Eingangs-) Wort setzt sich aus Zeichen  zusammen  und muss ein
|| Element der Kleen'schen  Hülle des Alphabets  sein, hat  daher den gleichen
|| Typ wie dieses.
character == char
alphabet  == [character]
word      == [character]

|| Definition spezieller Alphabete, die  bei  der Konstruktion  von  Automaten
|| verwendet werden können.
digits     = "0123456789"
binary     = "01"
capitals   = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
small      = "abcdefghijklmnopqrstuvwxyz"
whitespace = ['\t',' ']
interpunkt = "!?.,:;_"
math       = "+-*/^"
letters    = capitals ++ small
ascii      = letters ++ digits ++ whitespace ++ interpunkt ++ math

|| Ein Übergang wird  dargestellt durch ein Tripel (qi,a,qj) <- Q x Sigma x Q,
|| mit den Zuständen qi,qj<-Q und dem Zeichen a<-Sigma des Alphabets. Eine Zu-
|| standsübergangsfunktion delta ist eine Menge solcher Übergänge.
|| Da auch nichtdeterministische  endliche Automaten implementiert werden sol-
|| len können, ist das a des Tripels kein Zeichen, sondern eine Liste von Zei-
|| chen. So können auch leere (epsilon-) bzw. mehrdeutige Übergänge realisiert
|| werden. Für DEAs sind Zeichenfolgen einelementig, also z.B. ['a'] = "a".
transition == (num,[character],num)
|| Beispiele: (1,"a",2)  typischer DEA-Übergang (auch bei NEAs möglich)
||            (1,"01",2) mehrdeutiger Übergang  (nur NEA)
||            (1,"",2)   Epsilon-Übergang       (nur NEAeps)

abstype automat
with
  ||         Q         Sigma         delta
  dea :: [state] -> alphabet -> [transition] -> automat
  || Vor.: In 'dea qs zs ts' existiert für jedes Paar (q,z)<-qs x zs genau ein
  ||       Tripel (q,z,q') <-ts. Es  gibt KEINEN ANDEREN Zustand q <- qs außer
  ||       (0,Start), der den Typ 'Start' hat. Für jeden Zustand q <- qs gilt,
  ||       dass  er GENAU EINEN Typ hat. Der Zustand mit dem Index 0 kann auch
  ||       den Typ Accept haben.
  || Erg.: 'dea qs zs ts' liefert  einen  deterministischen Automaten  mit der
  ||       Zustandsmenge qs, dem Alphabet (=Zeichenmenge) ts und den Zustands-
  ||       übergängen ts. Die Menge der Endzustände  ist aus qs abgeleitet und
  ||       umfasst all  diejenigen Zustände, die  als  'Accept' gekennzeichnet
  ||       sind. Der  Startzustand ist immer derjenige mit dem Index 0.

  ||         Q         Sigma         delta
  nea :: [state] -> alphabet -> [transition] -> automat
  || Vor.: Es gibt KEINEN ANDEREN Zustand q <- qs außer (0,Start), der den Typ
  ||       'Start' hat. Für jeden Zustand q <- qs gilt, dass  er  GENAU  EINEN
  ||       Typ hat. Der Zustand mit dem Index 0 kann auch den Typ 'Accept' ha-
  ||       ben. Es gibt keinen Übergang (qi,"",qj) (epsilon-Übergang) in ts.
  || Erg.: 'nea qs zs ts' liefert  einen nicht-deterministischen Automaten mit
  ||       der Zustandsmenge qs,  dem  Alphabet (=Zeichenmenge) ts und den Zu-
  ||       standsübergängen ts. Die Menge der  Endzustände ist aus qs abgelei-
  ||       tet und umfasst all diejenigen Zustände, die  als 'Accept'  gekenn-
  ||       zeichnet sind. Der Startzustand ist immer derjenige mit Index 0.

  ||         Q         Sigma         delta
  neaE :: [state] -> alphabet -> [transition] -> automat
  || Vor.: Siehe 'nea'.
  ||       Außerdem dürfen nun Übergänge der Form (qi,"",qj) (epsilon-Übergän-
  ||       ge angegeben werden.
  || Erg.: Liefert einen nicht-deterministischen, endlichen  Automaten mit ep-
  ||       silon-Übergängen.

  transitions :: automat -> [transition]
  || Vor.: -
  || Erg.: Liefert die Liste aller Übergänge des Automaten.

  states :: automat -> [state]
  || Vor.: -
  || Erg.: Liefert die Menge aller Zustände des Automaten.

  sigma :: automat -> alphabet
  || Vor.: -
  || Erg.: Liefert das Alphabet des Automaten.

  isdea :: automat -> bool
  || Vor.: -
  || Erg.: G.d.w. in 'isdea a' a ein deterministischer, endlicher Automat ist,
  ||       ist True geliefert.

  isnea :: automat -> bool
  || Vor.: -
  || Erg.: G.d.w. in 'isnea a' a  ein nicht-deterministischer, endlicher Auto-
  ||       mat ist, ist True geliefert.

  isneaE :: automat -> bool
  || Vor.: -
  || Erg.: G.d.w. in 'isneaE a' a ein nicht-deterministischer, endlicher Auto-
  ||       mat mit epsilon-Übergängen ist, ist True geliefert.

  accepts :: automat -> word -> bool
  || Vor.: Das Wort ist in der Kleenschen Hülle des Automatenalphabets enthal-
  ||       ten.
  || Erg.: 'accepts a w' liefert  genau  dann 'True', wenn  a nach Abarbeitung
  ||       des Wortes w in einem Endzustand terminiert.

  startFrom :: automat -> num -> word -> [state]
  || Vor.: Das Wort ist in der Kleenschen Hülle des Automatenalphabets enthal-
  ||       ten.
  || Erg.: In 'startFrom a n ws' ist  die Menge der  Zustände q  geliefert, in
  ||       dem sich der Automat - im Zustand mit Index n beginnend - befindet,
  ||       wenn das Eingabewort ws abgearbeitet ist. Gilt 'isdea a=True', ent-
  ||       hält die Rückgabeliste sicher nur einen Zustand.

  addTransition :: automat -> transition -> automat
  || Vor.: In 'addTransition a (qi,z,qj)' gilt  sowohl, dass z<-alphabet a als
  ||       auch qi,qj<-map fst zustaende. Es gibt keinen Übergang (qi',z',qj')
  ||       in a, für den gilt: qi'=qi und z'=z (siehe changeTransition).
  || Erg.: 'addTransistion a t' liefert  einen Automaten, der in allen  Eigen-
  ||       schaften mit a übereinstimmt und zusätzlich den Übergang t enthält.

  changeTransition :: automat -> transition -> automat
  || Vor.: In 'changeTransition a (qi,z,qj)'  ist z <- alphabet a und es exis-
  ||       tiert ein qj' <- map fst (zustaende a), so dass  die  Enthaltensbe-
  ||       ziehung (qi,z,qj') <- transitions a gilt.
  || Erg.: Geliefert ist ein Automat, der in allen  Eigenschaften  mit a über-
  ||       einstimmt und für den (qi,z,qj) <- transitions a gilt,  aber  nicht
  ||       (qi,z,qj') <- transitions a.

  deleteTransition :: automat -> num -> [character] -> automat
  || Vor.: keine
  || Erg.: In 'deleteTransition a n z' ist der Automat geliefert, der in allen
  ||       Eigenschaften  mit a  übereinstimmt. Wenn a einen Übergang (n,z,qj)
  ||       für irgendeinen  Zustand qj <- map fst (zustaende a)  enthielt, ist
  ||       das in 'deleteTransition a n z' nicht der Fall.

  deleteState :: automat -> num -> automat
  || Vor.: n ~= 0
  || Erg.: In 'deleteState a n' ist ein Automat geliefert, der in allen Eigen-
  ||       schaften mit a übereinstimmt, für den aber nicht die  Enthaltensbe-
  ||       ziehung n <- map fst (zustaende a) gilt und für den es keinen Über-
  ||       gang (qi,z,qj) gibt, mit qi = n oder qj = n.

  addState :: automat -> state -> automat
  || Vor.: In 'addState a q' gilt  sowohl fst q ~= 0 als  auch snd q ~= Start.
  ||       'fst q' ist nicht in 'map fst (zustaende a)' enthalten.
  || Erg.: 'addState a q' stimmt in  allen Eigenschaften  mit  dem Automaten a
  ||       überein, hat aber einen weiteren (unerreichbaren) Zustand q.

  changeState :: automat -> num -> typ -> automat
  || Vor.: In 'changeState a n t' gilt n<-map fst (zustaende a) und t ~= Start
  || Erg.: Geliefert ist ein  Automat,  der in allen Eigenschaften mit a über-
  ||       einstimmt, der Typ des Zustandes mit dem Index n ist t.

  showAccepted :: [(automat,[char])] -> [[char]] -> [char]
  || Vor.: Alle Automaten haben (mindestens) einen akzeptierenden  Endzustand.
  ||       In der Eingabeliste ist jedem Automat a eine beschreibende Zeichen-
  ||       kette za zugeordnet.
  || Erg.: In 'showAccepted ass wss' ist für jeden Automat a<-map fst  ass und
  ||       jedes Wort w<-zss getestet, ob der Automat das Wort  akzeptiert und
  ||       das Ergebnis dieser Auswertung  auf dem Bildschirm  ausgegeben. Die
  ||       Ausgabe ist in Tabellenform erfolgt, die Tabellenköpfe sind die den
  ||       Automaten zugeordneten Zeichenketten.

  showautomat :: automat -> [char]
  || Vor.: keine
  || Erg.: Der Automat ist als Übergangstabelle auf dem Bildschirm  angezeigt.
  ||       Startzustand und Endzustände  werden wie in der einschlägigen Lite-
  ||       ratur üblich durch  ein '*' bzw. '->'  kenntlich gemacht. Bei  NEAs
  ||       wird die Zustandsmenge ggf. auf mehrere Zeilen verteilt.
  ||       Enthält der Automat keinen Endzustand, ist dies durch  einen  roten
  ||       Stern '*' gekennzeichnet, ist  der Automat ein DEA und fehlen Über-
  ||       gänge, erscheint der Schriftzug 'DEA' ebenfalls in rot.

%insert "automatenimpl.m"
