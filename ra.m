abstype ra
with
  compose :: [char] -> [char] -> ra
  || Vor.: xs enthält nur druckbare ASCII-Zeichen sowie Tabulatoren '\t' und
  ||       Zeienendenzeichen '\n'.
  || Erg.: In 'compose name xs' ist ein RA geliefert, der auf die Zeichenket-
  ||       te xs passt. Der Bezeichner des RA ist name.

  select  :: [char] -> [char] -> ra
  || Vor.: xs enthält nur druckbare ASCII-Zeichen sowie Tabulatoren '\t' und
  ||       Zeienendenzeichen '\n'.
  || Erg.: In 'select name xs' ist ein RA geliefert, der auf irgendeines der
  ||       Zeichen aus xs passt.

  raOr    :: [ra] -> ra
  || RA:   [r1,r1,r3,...] -> r1|r2|r3|...
  || Vor.: keine
  || Erg.: In 'raOr [r1,r2,r3,...]' ist ein RA geliefert, der auf alle Wörter
  ||       w passt, auf die r1 ODER r2 ODER r3 ODER ... passt.
  ||       Der Bezeichner des RA ist aus den Bezeichnern von r1,r2,r3,... zu-
  ||       sammengesetzt mit eingeschobenen '|'.

  raAnd   :: [ra] -> ra
  || RA:   [r1,r2,r3,...] -> r1r2r3...
  || Vor.: keine
  || Erg.: In 'raAnd [r1,r2,r3,...]' ist  ein  RA geliefert, der auf  all die
  ||       Wörter w1w2w3 passt, für die r1 auf w1 UND r2 auf w2 UND r3 auf w3
  ||       UND ... passen.
  ||       Der Bezeichner des RA ist aus den Bezeichnern von r1,r2,r3,... zu-
  ||       sammengesetzt (konkateniert).

  raStar  :: ra -> ra
  || RA:   r -> r*
  || Vor.: keine
  || Erg.: In 'raStar r' ist ein RA geliefert, der  auf beliebige, auch kein-
  ||       malige Wiederholungen von Wörtern passt, auf die r passt.
  ||       Besteht der Bezeichner <r> von r  nur aus  einem  Zeichen, ist der
  ||       zeichner von 'raStar r' <r>*, ansonsten (<r>)*.

  raPlus  :: ra -> ra
  || RA:   r -> r+
  || Vor.: keine
  || Erg.: In 'raStar r' ist ein RA geliefert, der  auf  beliebige, aber min-
  ||       destens einmalige Wiederholungen von Wörtern passt, auf die der RA
  ||       r passt.
  ||       Besteht der Bezeichner <r> von r  nur aus  einem  Zeichen, ist der
  ||       zeichner von 'raPlus r' <r>+, ansonsten (<r>)+.

  ra0or1  :: ra -> ra
  || RA:   r -> r?
  || Vor.: keine
  || Erg.: In 'raStar r' ist ein  RA geliefert, der  auf kein- oder einmalige
  ||       Wiederholung von Wörtern passt, auf die r passt.
  ||       Besteht der Bezeichner <r> von r  nur aus  einem  Zeichen, ist der
  ||       zeichner von 'ra0or1 r' <r>?, ansonsten (<r>)?.

  raMaxN  :: num -> ra -> ra
  || RA:   n -> r -> r{,n}
  || Vor.: keine
  || Erg.: In 'raMaxN n r' ist ein RA geliefert, der auf kein- bis maximal n-
  ||       malige Wiederholung von Wörtern passt, auf die r passt.
  ||       Besteht der Bezeichner <r> von r  nur aus  einem  Zeichen, ist der
  ||       zeichner von 'raMaxN r' <r>{,n}, ansonsten (<r>){,n}.

  raMinN  :: num -> ra -> ra
  || RA:   r -> r -> r{n,}
  || Vor.: keine
  || Erg.: In 'raMinN n r' ist ein RA  geliefert, der auf mindestens n-malige
  ||       Wiederholung von Wörtern passt, auf die r passt.
  ||       Besteht der Bezeichner <r> von r  nur aus  einem  Zeichen, ist der
  ||       zeichner von 'raMinN r' <r>{n,}, ansonsten (<r>){n,}.

  raN      :: num -> ra -> ra
  || RA:   n -> r -> r{n}
  || Vor.: keine
  || Erg.: In 'raN n r' ist ein RA  geliefert, der auf genau n-malige Wieder-
  ||       holung von Wörtern passt, auf die r passt.
  ||       Besteht der Bezeichner <r> von r  nur aus  einem  Zeichen, ist der
  ||       zeichner von 'raN r' <r>{n}, ansonsten (<r>){n}.

  raMN     :: num -> num -> ra -> ra
  || RA:   m -> n -> r -> r{m,n}
  || Vor.: keine
  || Erg.: In 'raMN m n r' ist ein  RA  geliefert,  der auf mindestens n- und
  ||       höchstens m-malige Wiederholung von Wörtern passt, auf die r passt.
  ||       Besteht der Bezeichner <r> von r  nur aus  einem  Zeichen, ist der
  ||       zeichner von 'raMN r' <r>{m,n}, ansonsten (<r>){m,n}.

  matches :: ra -> [char] -> bool
  showra  :: ra -> [char]

  || spezielle RAs, die in RA-Implementierungen typischerweise verwendet wer-
  || den (siehe z.B. Skript, Seite 3-3).

  alpha_  :: ra
  ||      RA, der auf ein beliebigen Buchstaben matcht.
  digit_  :: ra
  ||      RA, der auf eine beliebige Dezimalziffer matcht.
  upper_  :: ra
  ||      RA, der auf einen beliebigen Großbuchstaben matcht (ohne Umlaute).
  lower_  :: ra
  ||      RA, der auf einen beliebigen Kleinbuchstaben matcht (ohne Umlaute).
  alnum_  :: ra
  ||      RA, der auf einen beliebigen Buchstaben oder Dezimalziffer matcht.
  punct_  :: ra
  ||      RA, der auf ein Zeichen aus ".,:;?!" matcht.

%insert "raimpl.m"
