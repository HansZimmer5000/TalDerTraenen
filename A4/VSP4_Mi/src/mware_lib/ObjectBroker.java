package mware_lib;

import java.io.IOException;

import nameservice._NameserviceImplBaseStub;

public class ObjectBroker {

	private boolean debug;
	private _NameserviceImplBaseStub ns;

	public ObjectBroker(String host, int port, boolean debug) throws IOException {
		Communicator con = new Communicator(host, port, debug);
		ns = new _NameserviceImplBaseStub(con);
		this.debug = debug;
	}

	public static ObjectBroker init(String serviceHost, int port, boolean debug) throws IOException {
		return new ObjectBroker(serviceHost, port, debug);
	}

	// Liefert den Namensdienst (Stellvetreterobjekt).
	public _NameserviceImplBaseStub getNameService() {
		return this.ns;
	}

	// Beendet die Benutzung der Middleware in dieser Anwendung.
	public void shutDown() throws IOException {
		System.out.println("Object Brooker heruntergefahren");
		return;
	}
}
