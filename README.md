# automaten
Miranda-Paket für endliche Akzeptoren in der LWB Informatik an der FU-Berlin

    Version 0.41 von Mo 26. Feb 10:00:41 CET 2018

Installationsanweisungen:

Es gibt zwei Wege, das Automatenpaket zu installieren:

(1) Die automatische Version

    Erfordert Vertrauen in den Autor des Paketes und ein Ausführen des Skriptes
    'install' als root:

    # ./install

(2) Der zweite Weg macht alles in 4 Schritten per Hand:

    (a) Die Dateien müssen in das Verzeichnis /usr/lib/miralib/lwb kopiert werden.
        Ggf. muss das Verzeichnis als root erst erstellt werden.

    (b) Für zukünftige Updates ist es günstig, das Verzeichnis lewein zu vermachen.
        Dazu gibt man als root an der Kommandozeile

          chown lewein:lewein /usr/lib/miralib/lwb

    (c) Nun müssen noch die Zwischencodes (*.x-Dateien) erzeugt werden. Wegen
        Schritt (b) kann dies nun auch als Benutzer lewein geschehen. Etwaige
        Fehlermeldungen, die die Dateien automatenimpl.m betreffen, können
        getrost ignoriert werden.

          mira -make /usr/lib/miralib/lwb/*.m

Nach Abschluss der installation können in jeder Miranda-Datei die ADTs 'automat'
und 'ra' verwendet werden, indem man sie inkludiert mittels einer der Anweisungen

     %include <lwb/automaten>
     %include <lwb/ra>

Zusätzliche Automaten-Funktionen erhält man durch

     %include <lwb/automatentools>
