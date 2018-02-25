|| ==================== LOOP-Programme ========================
|| (c) Oliver Schäfer                                 2014-2016
|| Version vom Do 5. Mai 22:37:32 CEST 2016
|| ============================================================

|| ---------------------- Definitionen ------------------------
|| Speichervektor, eingeschränkt auf natürliche Zahlen 
memory == [num]
|| Speichergröße: 16 Bytes (x0...x15)
size =  16
|| Speicheradressen als alg. Datentyp, ggf. an 'size' anpassen
adr ::= X0  | X1  | X2  | X3  | X4  | X5  | X6  | X7  |
        X8  | X9  | X10 | X11 | X12 | X13 | X14 | X15
|| Anweisung, Abbildung eines Speichervektors auf einen anderen
instruction == memory -> memory
|| ------------------------------------------------------------

|| Um aus einer Adresse einen Speicherindex zu generieren
idx :: adr -> num
idx  X0 =  0; idx  X1 =  1; idx  X2 =  2; idx  X3 =  3;
idx  X4 =  4; idx  X5 =  5; idx  X6 =  6; idx  X7 =  7;
idx  X8 =  8; idx  X9 =  9; idx X10 = 10; idx X11 = 11;
idx X12 = 12; idx X13 = 13; idx X14 = 14; idx X15 = 15

|| Nachfolger- und Vorgängerfunktion
succ,pred :: num -> num
succ  n    = n+1
pred  0    = 0
pred (n+1) = n

|| Speicherinhalt holen oder setzen
get :: adr -> memory -> num
get xi ram = ram!(idx xi)

set :: adr -> num -> memory -> memory
set xi n ram = take (idx xi) ram ++ [n] ++ drop (idx xi+1) ram

|| Speicherinhalt holen und setzen in einer Anweisung
getNsetM :: adr -> adr -> (num -> num) -> memory -> memory
getNsetM adr0 adr1 p ram = set adr1 (p (get adr0 ram)) ram

|| der besseren Lesbarkeit wegen ...
copy n m = getNsetM n m id
inc  n   = getNsetM n n succ
dec  n   = getNsetM n n pred

|| Schleifen
|| mit Variableninhalt als Schleifendurchlaufzahl
loop :: adr -> [instruction] -> memory -> memory
loop n fs ram = (iterate (comp fs) ram)!(get n ram)

comp :: [instruction] -> instruction
comp []     = noop
comp (f:fs) = (comp fs) . f

|| leere Operation
noop :: memory -> memory
noop = id

|| Ein Programm ist eine Anweisungsfolge, die auf einem initial leeren Speicher arbeitet
program :: [instruction] -> memory
program is = program' (rep size 0,is)
             where
             program' (ram, [])   = ram
             program' (ram, f:fs) = program' (f ram, fs)

|| Funktion, um das Ergebnis aus x0 zu extrahieren
result = (!0)

|| erlaubt die Anwendung n-stelliger Makros mit n<-{1,2}, die entweder einen
|| oder zwei Rückgabewerte liefern. Im Fall eines Rückgabewertes wird dieser
|| aus X0 extrahiert, bei zwei Rückgabewerten aus X1 und X2.
macro11 out1      f in1     ram = set out1 (result (f (get in1 ram)))               ram
macro21 out1      f in1 in2 ram = set out1 (result (f (get in1 ram) (get in2 ram))) ram
macro12 out1 out2 f in1     ram = comp [ set out1 (erg!1), set out2 (erg!2) ]       ram
                                  where
                                  erg = f (get in1 ram)
macro22 out1 out2 f in1 in2 ram = comp [ set out1 (erg!1), set out2 (erg!2) ]       ram
                                  where
                                  erg = f (get in1 ram) (get in2 ram)
