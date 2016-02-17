# automaten
Miranda-Paket für endliche Akzeptoren in der LWB Informatik an der FU-Berlin

Installationsanweisungen:

(1) Die Dateien müssen in das Verzeichnis /usr/lib/miralib/lwb kopiert werden.
    Ggf. muss das Verzeichnis als root erst erstellt werden.

(2) Für zukünftige Updates ist es günstig, das Verzeichnis lewein zu vermachen.
    Dazu gibt man als root an der Kommandozeile

      chmod lewein:lewein /usr/lib/miralib/lwb

(3) Nun müssen noch die Zwischencodes (*.x-Dateien) erzeugt werden. Wegen
    Schritt (2) kann dies nun auch als Benutzer lewein geschehen:

      mira -make /usr/lib/miralib/lwb/*.m

    Etwaige Fehlermeldungen, die die Dateien automatenimpl.m betreffen, können
    getrost ignoriert werden.

(4) Nun kann in jeder Miranda-Datei mittels Include-Anweisung

      %include <lwb/automaten>

    der ADT automat verwendet werden.
