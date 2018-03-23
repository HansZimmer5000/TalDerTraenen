Zu dem Paket gehoeren die Datein:
    - client.cfg
    - server.cfg
    - hbq.cfg

    - client.erl
    - server.erl
    - cmem.erl
    - hbq.erl
    - dlq.erl

Sowie Dateien zum testen des Codes und, vom Professor gestellte, Werkzeug dateien ("util").

------------------------
Starten der Applikation:
------------------------
Die Reihenfolge ist wichtig:
    1. HBQ
    2. Sever
    3. Client
Da sonst evtl. Nachrichten zu früh kommen und somit Komponenten nicht initialisiert werden.
Im Python skript wird nach jedem Komponentenstart eine Sekunde gewartet.
Um sicher zu gehen, dass die Initalnachrichten nicht verloren gehen.
Jedoch kann man auch wesentlich kürzer warten bzw. wenn man die Reihenfolge einhält gar nicht.

Wenn Python auf dem Gerät vorhanden ist kann man auch:
python startall.py 
    Compiled alle .erl Dateien mit "erl -make".
    Danach werden alle .beam Dateien geloescht.
    Ist dazu gedacht, schnell zu pruefen ob alle Dateien compilen.
python startall.py 0
    Siehe ohne Paramter
python startall.py 1
    Compiled alle .erl Dateien und fuehrt alle Tests aus.
    Loescht danach alle .beam Dateien.
python startall.py 2
    Compiled alle .erl Dateien.
    Leert dann alle vorhandenen .log Dateien.
    Fuehrt die Applikation aus.
python startall.py 3
    Loescht alle .beam, .dump, .log Dateien.
    Raeumt quasi den Ordner auf, so dass nur noch .cfg und .erl Dateien übrig bleiben.

--------------------
Starten der Nodes (In .cfg auf die gesetzten Nodenamen achten!):
--------------------
(w)erl -(s)name <hbqNode-Name> 
(w)erl -(s)name <ServerName> 
(w)erl -(s)name <ClientName> 

Oder wenn Python 3.6 vorhanden:
python startall.py 2

--------------------
Starten des Clients:
--------------------
1> client:start().

in der client.cfg:
{clientAnzahl, 10}.
    Anzahl der zu startenden clients.
{lifetime, 60}.
    Lebenszeit eines einzelnen clients
{servername, 'wk'}.
    Registrierter Name des Servers.
{servernode, 'server@Michael-X250'}.
    Node auf dem der Registrierte Server zu finden ist.
{hostname, hostname1}.
    Der zu verwendende Hostname für den Nachrichtentext
{praktikumsgruppe, gruppe2}.
    Die zu verwendende Praktikumsgruppe für den Nachrichtentext
{teamnummer, team6}.
    Die zu verwendende Teamnummer für den Nachrichtentext


--------------------
Starten des Servers:
--------------------
1> server:start().

in der server.cfg:
{servername, 'wk'}.
    Der Registrierte Servernamen.
{latency, 5}.
    Latenz in Sekunden.
    Wird verwendet um nach der letzten empfangenen Nachricht und nach der Latenz herunterzufahren.
{clientlifetime, 4}.
    Erinnerungszeit in Sekunden für die CMEM.
    Gibt an wie lang an einen beliebigen aber bestimmten Client in der CMEM gedacht wird.
{hbqname, 'wk'}.
    Der Registrierte HBQnamen.
{hbqnode, 'hbq@Michael-X250'}.
    Node auf dem die Registrierte HBQ zu finden ist.


--------------------
Starten der HBQ:
--------------------
1> hbq:start().

in der hbq.cfg:
{dlqlimit, 100}.
    Die Groesse der DLQ.
{hbqname, 'wk'}.
    Der Registrierte Name der HBQ.

-------------
Runterfahren:
-------------
windows shell: Ctrl C
Oder:
Warten bis Lebenszeit (lifetime) der clients ausläuft und der Server von selbst herunterläuft.
