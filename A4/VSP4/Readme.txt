Zu dem Paket gehören die Dateien

mware_lib:
ComHandler.java, INameService.java, NameService.java, ObjectBroker.java, 
ServiceListener.java, SkeletonThread.java, Util.java
MiddleWare:
MiddleWare.java, MiddleWareSkeleton.java, NameService.java
math_ops:(vom Compiler erstellen Klassen)
_CalculatorImplBase.java, _CalculatorImplBaseStub,
main:
CalculatorServer.java, Client.java
idl_compiler:
IDLclass.java, IDLCompiler.java, IDLmodule.java, Parser.java, FileCreator.java
test:
Test.java


class.idl

ausführbare Datein:
MiddleWare.jar, Client.jar, Service.jar


----------------------------------------
Wird kein Port mitgegeben wird der standard Port 55555 verwendet
Starten der Middleware:
1> java -jar MiddleWare.jar PORT


----------------------------------------
HOST und PORT von der Middleware
Starten des Clients:
1> java -jar Client.jar HOST PORT DEBUG

----------------------------------------
HOST und PORT von der Middleware
Starten des Services:
1> java -jar Service.jar HOST PORT DEBUG

