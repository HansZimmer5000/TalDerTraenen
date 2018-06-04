package mware_lib;

import java.io.IOException;

public class ObjectBroker {

	private boolean debug;
	private NameService ns;

	public ObjectBroker(String host, int port, boolean debug) throws IOException {
		ComHandler con = new ComHandler(host, port, debug);
		ns = new NameService(con, debug);
		this.debug = debug;
	}

	public static ObjectBroker init(String serviceHost, int port, boolean debug) throws IOException {
		return new ObjectBroker(serviceHost, port, debug);
	}

	// Liefert den Namensdienst (Stellvetreterobjekt).
	public NameService getNameService() {
		return this.ns;
	}

	// Beendet die Benutzung der Middleware in dieser Anwendung.
	public void shutDown() throws IOException {
		System.out.println("Object Brooker heruntergefahren");
		return;
	}
}
