|| Ansi-Sequenzen zum Setzen einiger Textattribue in der Konsole
|| (c) Oliver Schäfer, FU-Berlin, 04.02.2016

sequenz ::= F color | B color | A att | C common
color   ::= Schwarz | Rot | Gruen | Gelb | Blau | Violett | Hellblau | Weiss
att     ::= Fett | Normal | Blinkend | Unterstrichen
common  ::= Reset | Loeschen

vfaerben,hfaerben :: [char] -> color -> [char]
vfaerben text farbe = ansiseq (F farbe) ++ text ++ ansiseq (C Reset)
hfaerben text farbe = ansiseq (B farbe) ++ text ++ ansiseq (C Reset)

fett :: [char] -> [char]
fett text = ansiseq (A Fett) ++ text ++ ansiseq (A Normal)

positioniere :: num -> num -> [char]
positioniere x y = ['\27'] ++ "[" ++ shownum x ++ ";" ++ shownum y ++ "H"

loesche :: [char]
loesche = ansiseq (C Loeschen)

ansiseq :: sequenz -> [char]
ansiseq = esc . ansiseq'
          where
          esc = (['\27'] ++)
          ansiseq' (F Schwarz)       = "[30m"
          ansiseq' (F Rot)           = "[31m"
          ansiseq' (F Gruen)         = "[32m"
          ansiseq' (F Gelb)          = "[33m"
          ansiseq' (F Blau)          = "[34m"
          ansiseq' (F Violett)       = "[35m"
          ansiseq' (F Hellblau)      = "[36m"
          ansiseq' (F Weiss)         = "[37m"
          ansiseq' (B Schwarz)       = "[40m"
          ansiseq' (B Rot)           = "[41m"
          ansiseq' (B Gruen)         = "[42m"
          ansiseq' (B Gelb)          = "[43m"
          ansiseq' (B Blau)          = "[44m"
          ansiseq' (B Violett)       = "[45m"
          ansiseq' (B Hellblau)      = "[46m"
          ansiseq' (B Weiss)         = "[47m"
          ansiseq' (A Normal)        = "[0m"
          ansiseq' (A Fett)          = "[1m"
          ansiseq' (A Blinkend)      = "[1m"
          ansiseq' (A Unterstrichen) = "[1m"
          ansiseq' (C Reset)         = "[0;49m"
          ansiseq' (C Loeschen)      = "[2J"
