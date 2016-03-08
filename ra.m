abstype ra
with
  ||         name      zeichenkette
  allOf   :: [char] -> [char] -> ra
  oneOf   :: [char] -> [char] -> ra

  || Kombinierer von RAs
  raOr    :: ra -> ra -> ra
  || Vor.: keine
  || Erg.: In 'regexOR a b' ist ein NEAe geliefert, der all die  Wörter w akzep-
  ||       tiert, die von a ODER von b akzeptiert werden. Das Alphabet des Auto-
  ||       maten ist die Vereinigung der Alphabete von a und b.

  raAnd   :: ra -> ra -> ra
  || Vor.: keine
  || Erg.: In 'regexAND a b' ist ein NEAe c geliefert, der all die Wörter vw ak-
  ||       zeptiert, für die v von  a UND  w von b akzeptiert werden. Das Alpha-
  ||       bet von c ist die Vereinigung der Alphabete von a und b.

  raStar  :: ra -> ra
  || Vor.: keine
  || Erg.: In 'regexSTAR a' ist ein NEAe geliefert, der beliebige - auch keinma-
  ||       lige - Wiederholungen von  Wörtern akzeptiert, die  a akzeptiert. Das
  ||       Alphabet des Automaten ist das gleiche wie das von a.

  matches :: ra -> [char] -> bool
  showra  :: ra -> [char]

  || spezielle RAs, Spezifikation siehe Skript Seite 3-3
  alpha_  :: ra
  digit_  :: ra
  upper_  :: ra
  lower_  :: ra

%insert "raimpl.m"
