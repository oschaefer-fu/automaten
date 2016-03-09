abstype ra
with
  ||         name      zeichenkette
  allOf   :: [char] -> [char] -> ra
  oneOf   :: [char] -> [char] -> ra

  || Kombinierer von RAs
  raOr    :: ra -> ra -> ra
  || RA:   a -> b -> a|b
  || Vor.: keine
  || Erg.: In 'raOr a b' ist ein RA geliefert, der auf all die Wörter w passt,
  ||       auf die a ODER b passt.

  raAnd   :: ra -> ra -> ra
  || RA:   a -> b -> ab
  || Vor.: keine
  || Erg.: In 'raAnd a b' ist ein  RA  geliefert, der  auf  all die Wörter vw
  ||       passt, für die auf v UND b auf w passen.

  raStar  :: ra -> ra
  || RA:   a -> a*
  || Vor.: keine
  || Erg.: In 'raStar a' ist ein RA geliefert, der  auf beliebige, auch kein-
  ||       malige Wiederholungen von Wörtern passt, auf die a passt.

  raPlus  :: ra -> ra
  || RA:   a -> a+
  || Vor.: keine
  || Erg.: In 'raStar a' ist ein RA geliefert, der  auf  beliebige, aber min-
  ||       destens einmalige Wiederholungen von Wörtern passt, auf die der RA
  ||       a passt.

  ra0or1  :: ra -> ra
  || RA:   a -> a?
  || Vor.: keine
  || Erg.: In 'raStar a' ist ein  RA geliefert, der  auf kein- oder einmalige
  ||       Wiederholung von Wörtern passt, auf die a passt.

  matches :: ra -> [char] -> bool
  showra  :: ra -> [char]

  || spezielle RAs, die in RA-Implementierungen typischerweise verwendet wer-
  || den (siehe z.B. Skript, Seite 3-3).
  alpha_  :: ra
  digit_  :: ra
  upper_  :: ra
  lower_  :: ra
  alnum_  :: ra
  punct_  :: ra

%insert "raimpl.m"
