# automaten
Miranda-Paket für endliche Akzeptoren in der LWB Informatik an der FU-Berlin

Installationsanweisungen:

Es gibt zwei Wege, das Automatenpaket zu installieren: Die automatische Version
erfordert Vertrauen in den Autor des Paketes und ein Ausführen des Skriptes
'install' als root:

  # ./install

Der zweite Weg macht alles in 4 Schritten per Hand:

(1) Die Dateien müssen in das Verzeichnis /usr/lib/miralib/lwb kopiert werden.
    Ggf. muss das Verzeichnis als root erst erstellt werden.

(2) Für zukünftige Updates ist es günstig, das Verzeichnis lewein zu vermachen.
    Dazu gibt man als root an der Kommandozeile

      chmod lewein:lewein /usr/lib/miralib/lwb

(3) Nun müssen noch die Zwischencodes (*.x-Dateien) erzeugt werden. Wegen
    Schritt (2) kann dies nun auch als Benutzer lewein geschehen. Etwaige
    Fehlermeldungen, die die Dateien automatenimpl.m betreffen, können
    getrost ignoriert werden.

      mira -make /usr/lib/miralib/lwb/*.m


(4) Nun kann in jeder Miranda-Datei der ADT 'automat' verwendet werden, indem
    man ihn inkludiert mittels der Anweisung:

      %include <lwb/automaten>
