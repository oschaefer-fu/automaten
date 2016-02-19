|| ======================= Miranda-Paket 'automatentools' ====================
|| (c) Oliver Schäfer                                             Februar 2016
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

minimize :: automat -> automat
|| Vor.: Der Automat ist ein DEA.
|| Erg.: In 'minimize a' ist der  zu a äquivalente, d.h. der die gleiche Spra-
||       che akzeptierende deterministische  Automat geliefert, der eine mini-
||       male Anzahl Zustände besitzt.

unreachable :: automat -> [num]
|| Vor.: keine
|| Erg.: In 'unreachable a' ist eine Liste  von Zustandsindizes geliefert, die
||       von q0 aus nicht erreichbar sind.

reduce :: automat -> automat
|| Vor.: keine
|| Erg.: In 'reduce a' ist ein zu a äquivalenter, d.h. ein die gleiche Sprache
||       akzeptierender Automat geliefert, der um die in  a unerreichbaren Zu-
||       stände verkürzt ist.

%insert "automatentoolsimpl.m"
