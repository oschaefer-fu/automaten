|| ===================== GOTO-Programme =====================
|| (c) Oliver Schäfer                               2014-2016
|| Version vom Fr 6. Mai 22:55:30 CEST 2016
|| ==========================================================

|| --------------------- Definitionen -----------------------
marke       == num
lnr        ::= N | M marke
jump       ::= Next | First | Last | Goto marke
memory      == [num]
instruction == memory -> (jump,memory)      || eine Anweisung
codeline    == (lnr,instruction)
size        =  16                            || Speichergröße
sigmazero   =  rep size 0                         || Speicher
|| Speicheradressen als alg. Datentyp, ggf. an 'size' anpassen
adr ::= X0  | X1  | X2  | X3  | X4  | X5  | X6  | X7  |
        X8  | X9  | X10 | X11 | X12 | X13 | X14 | X15
|| ----------------------------------------------------------

|| Um aus einer Adresse einen Speicherindex zu generieren
idx :: adr -> num
idx  X0 =  0; idx  X1 =  1; idx  X2 =  2; idx  X3 =  3;
idx  X4 =  4; idx  X5 =  5; idx  X6 =  6; idx  X7 =  7;
idx  X8 =  8; idx  X9 =  9; idx X10 = 10; idx X11 = 11;
idx X12 = 12; idx X13 = 13; idx X14 = 14; idx X15 = 15

|| Nachfolger- und Vorgängerfunktion (intern)
succ,pred :: num -> num
succ  n    = n+1
pred  0    = 0
pred (n+1) = n

|| Speicherinhalt holen (intern)
get :: adr -> memory -> num
get i ram = ram!(idx i)

|| Speicherinhalt holen und setzen in einer Anweisung (intern)
getNsetM :: adr -> adr -> (num -> num) -> memory -> (jump,memory)
getNsetM adr0 adr1 p ram = set adr1 (p (get adr0 ram)) ram

|| Speicherinhalt setzen
set :: adr -> num -> memory -> (jump,memory)
set i n ram = (Next, take (idx i) ram ++ [n] ++ drop (idx i+1) ram)

|| Diese Funktionen werden außen verwendet
copy :: adr -> adr -> memory -> (jump,memory)
copy n m = getNsetM n m id

inc,dec :: adr -> memory -> (jump,memory)
inc  n   = getNsetM n n succ
dec  n   = getNsetM n n pred

|| halt-Anweisung
halt :: memory -> (jump,memory)
halt mem = (Last, mem)

|| noop-Anweisung
noop :: memory -> (jump,memory)
noop mem = (Next, mem)

|| Sprungbefehle
goto :: lnr -> memory -> (jump,memory)
goto N     mem = error "Kann nicht zu einer markenlosen Zeile springen"
goto (M n) mem = (Goto n, mem)

ifgoto :: adr -> lnr -> memory -> (jump,memory)
ifgoto i (M n) mem = goto (M n) mem, if (get i mem) = 0
                   = noop mem,       otherwise

|| Das Kernstück der Implementierung ist  das Programm, das sowohl  den
|| Speicher als auch den Programmcode verwaltet.
|| Ein Programm ist eine Anweisungsfolge, die auf einem Speicher arbei-
|| arbeitet, der zu Beginn (in  der Regel) leer ist. Die Anweisungsfol-
|| folge wird im Normalfall nacheinander in der Reihenfolge der Aufzäh-
|| lung  abgearbeitet. Eine Ausnahme macht eine goto-Anweisung, die die
|| Ausführungsreihenfolge ändern kann. Das Programm  endet  mit dem Er-
|| reichen einer halt-Anweisung.
program :: [codeline] -> memory
program  cs = program' (sigmazero, cs) 0
program' (ram, cs) (-1) = ram
program' (ram, cs) pc
  = program' (ram', cs) (pc+1), if action = Next
  = program' (ram', cs) (-1),   if action = Last
  = error "unmöglicher Fall",   if action = First
  = program' (ram', cs) pc',    otherwise
    where f    = snd (cs!pc)
          ram' = snd (f ram)
          pc'  = [n | (n,(marke,anw))<-zip2 (index cs) cs; marke ~= N; ziel action = mark marke]!0
          action = fst (f ram)
          ziel (Goto n) = n
          mark (M n)    = n

|| Hilfsfunktion, um das Ergebnis aus x0 zu extrahieren
result prog = prog!0

|| erlaubt die Anwendung n-stelliger Makros mit n<-{1,2}, die entweder einen
|| oder zwei Rückgabewerte liefern. Im Fall eines Rückgabewertes wird dieser
|| aus X0 extrahiert, bei zwei Rückgabewerten aus X1 und X2.
macro11 out1      f in1     ram = set out1 (result (f (get in1 ram)))               ram
macro21 out1      f in1 in2 ram = set out1 (result (f (get in1 ram) (get in2 ram))) ram
macro12 out1 out2 f in1     ram = (set out1 (erg!1) . snd . set out2 (erg!2)) ram
                                  where erg = f (get in1 ram)
macro22 out1 out2 f in1 in2 ram = (set out1 (erg!1) . snd . set out2 (erg!2)) ram
                                  where erg = f (get in1 ram) (get in2 ram)
