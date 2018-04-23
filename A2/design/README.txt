Zu dem Paket gehoeren die Datein:
    - ggt.cfg
    - koordinator.cfg

    - ggtprozess.erl
    - starter.erl
    - koordinator.erl
    - nameservice.beam
    - man.erl

Sowie Dateien zum testen des Codes und, vom Professor gestellte, Werkzeug dateien ("util").

------------------------
Starten der Applikation:
------------------------
Die Reihenfolge ist wichtig:
    1. Nameservice
    2. Koordinator
    3. Starter
Da sonst evtl. Nachrichten zu früh kommen und somit Komponenten nicht initialisiert werden.

Wenn Python auf dem Windows Gerät vorhanden ist kann man auch:
python startall.py 
    Compiled alle .erl Dateien mit "erl -make".
    Danach werden alle .beam Dateien (außer nameservice.beam und *util.beam) geloescht.
    Ist dazu gedacht, schnell zu pruefen ob alle Dateien compilen.
python startall.py 0
    Siehe ohne Paramter
python startall.py 1
    Compiled alle .erl Dateien und fuehrt alle Tests aus.
    Loescht danach alle .beam (Ausnahmen siehe ohne Paramter) Dateien.
python startall.py 2
    Compiled alle .erl Dateien.
    Leert dann alle vorhandenen .log Dateien.
    Fuehrt die Applikation aus.
python startall.py 3
    Loescht alle .beam (Ausnahmen siehe ohne Paramter), .dump, .log Dateien.
    Raeumt quasi den Ordner auf, so dass nur noch .cfg und .erl Dateien übrig bleiben.

--------------------
Starten der Nodes (In .cfg auf die gesetzten Nodenamen achten!):
--------------------
(w)erl -(s)name <NameserviceNamen> 
(w)erl -(s)name <KoordinatorName> 
(w)erl -(s)name <StarterName> 

Oder wenn Python 3.6 auf Windows vorhanden:
python startall.py 2

--------------------
Starten des Nameservices:
--------------------
1> nameservice:start().

--------------------
Starten des Koordinators:
--------------------
1> koordinator:start().

in der koordinator.cfg:
{nameservicenode, 'ns@Michael-X250'}. 
    Node auf der der Nameservice zu finden ist.
{koordinatorname, koordinator}.
    Der zu registierende Name fuer den Koordinator
{arbeitszeit, 1}.
    Die (Berechnungs) Wartezeit die ueber die Starter an die ggT-Prozesse gesendet werden.
{termzeit, 5}.
    TermZeit die ueber die Starter an die ggT-Prozesse gesendet wird.
{quote, 80}.
    Quote, beschreibt die Anzahl wie viele auf ein Voting mit "voteYes" antworten muessen damit es ein erfolgreiches war, in Prozent
{ggtprozessnummer, 2}.
    Gibt an wie viele ggT-Prozesse pro Starter gestartet werden.
{korrigieren, true}.
    Gibt initial an ob eine Korrektur auf eine falsche Terminierung des ggT-Prozesses gesendet werden soll.


--------------------
Starten der Starter:
--------------------
1> starter:start(AnzahlStarter, ErsteStarterNummer).

Startet ggT-Prozesse und list aus der ggt.cfg:

{nameservicenode, 'ns@Michael-X250'}. 
    Node auf der der Nameservice zu finden ist.
{koordinatorname, koordinator}.
    Der registrierte Name fuer den Koordinator
{praktikumsgruppe, 2}.
    Praktikumsgruppe innerhalb des VSP 2018-SS
{teamnummer, 6}.
    Teamnummer innerhalb des VSP 2018-SS

--------------------
Starten der manuellen Eingabe:
--------------------
1> man:start().

Nun koennen bestimmte Befehle eingegeben werden:
kill
    Sendet 'kill' an den Koordinator.
reset
    Sendet 'reset' an den Koordinator.
prompt
    Sendet 'prompt' an den Koordinator.
nudge
    Sendet 'nudge' an den Koordinator.
step
    Sendet 'step' an den Koordinator.
calc
    Fordert in neuer Terminalzeile eine Zahl.
    Gibt man eine ein und drueckt man dann enter, so wird '{calc, <EingegebeneZahl>}' an den Koordinator gesendet.

-------------
Runterfahren:
-------------
windows shell: Ctrl C
Oder:
per manueller Eingabe 'kill' an Koordinator senden.









