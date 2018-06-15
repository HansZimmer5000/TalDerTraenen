package mware_lib;

/**
 * ObjectBroker class.
 * 
 * @author Mert Siginc
 * In dieser Klasse wird eine Nameservice referenz erzeugt, 
 * welches ein ComHandler beinhaltet mit der IP PORT vom NS.
 *
 */

import java.io.IOException;

public class ObjectBroker {

	private boolean debug;
	private NameService ns;
	ComHandler con;

	public ObjectBroker(String host, int port, boolean debug) throws IOException {
		con = new ComHandler(host, port, debug);
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
		con.shutdown();
		Util.println("Object Brooker heruntergefahren", debug);
		return;
	}
}
