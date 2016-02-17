|| ======================= Miranda-Paket 'automatentools' ====================
|| (c) Oliver Schäfer                                         13. Februar 2016
|| ===========================================================================

deleps :: automat -> automat
|| Vor.: Der Automat ist ein NEA oder ein NEAe.
|| Erg.: In 'deleps a' ist ein zu a äquivalenter, d.h. ein die gleiche Sprache
||       akzeptierender Automat ohne epsilon-Übergänge geliefert.

powersetConstr :: automat -> automat
|| Vor.: Der Automat ist ein NEA oder ein NEAe.
|| Erg.: In 'powersetConstr a' ist der zu a  äquivalente, d.h. der die gleiche
||       Sprache akzeptierende deterministische Automat geliefert. Der Automat
||       ist nicht notwendigerweise minimal.

equivalent :: automat -> [(num,num)]
|| Vor.: Der Automat ist ein DEA.
|| Erg.: Eine Liste von Paaren äquivalenter Zustände (qi,qj) mit qi>qj ist ge-
||       liefert.

minimize,minimize' :: automat -> automat
|| Vor.: Der Automat ist ein DEA.
||       In 'minimize a' sind die Anzahl der Zustände und die Länge des Alpha-
||       bets nicht zu groß, andernfalls terminiert die  Ausführung mit  einem
||       segmentation fault (13.02.2016). Die Verwendung von minimize' kann zu
||       einer erfolgreichen Auswertung führen, allerdings wird  hierbei keine
||       Entfernung unerreichbarer Zustände durchgeführt.
|| Erg.: In 'minimize a' ist der  zu a äquivalente, d.h. der die gleiche Spra-
||       che akzeptierende deterministische  Automat geliefert, der eine mini-
||       male Anzahl Zustände besitzt.

unreachable :: automat -> [num]
|| Vor.: keine
||       Da die Laufzeit quadratisch von  der Anzahl  der Zustände und  linear
||       von der Länge des Alphabets abhängt, führt die Anwendung dieser Funk-
||       tion auf 'zu große' Automaten derzeit zu einem 'segmentation fault'.
|| Erg.: In 'unreachable a' ist eine Liste  von Zustandsindizes geliefert, die
||       von q0 aus nicht erreichbar sind.

reduce :: automat -> automat
|| Vor.: siehe unreachable
|| Erg.: In 'reduce a' ist ein zu a äquivalenter, d.h. ein die gleiche Sprache
||       akzeptierender Automat geliefert, der um die in  a unerreichbaren Zu-
||       stände verkürzt ist.

%insert "automatentoolsimpl.m"
