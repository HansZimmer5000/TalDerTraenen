package mware_lib;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;

public class ObjectBroker {

	private boolean debug;
	private Socket sSocket;
	private PrintWriter out;
	private ObjectInputStream objIn;

	public ObjectBroker(String host, int port, boolean debug) throws UnknownHostException, IOException {
		this.debug = debug;
		this.sSocket = new Socket(host, port);
		out = new PrintWriter(new OutputStreamWriter(sSocket.getOutputStream()));
		objIn = new ObjectInputStream(sSocket.getInputStream());
	}

	public static ObjectBroker init(String serviceHost, int listenPort, boolean debug)
			throws UnknownHostException, IOException {
		return new ObjectBroker(serviceHost, listenPort, debug);
	}

	// Liefert den Namensdienst (Stellvetreterobjekt).
	public INameService getNameService() throws IOException, ClassNotFoundException {
		System.out.println("NS Anfrage wurde gestartet");
		out.println("GET_NS");
		out.flush();
		Object remoteNS = objIn.readObject();
		return (INameService) remoteNS;
	}

	// Beendet die Benutzung der Middleware in dieser Anwendung.
	public void shutDown() throws IOException {
		out.println("KILL");
		out.flush();
		out.close();
		objIn.close();
		sSocket.close();
		System.out.println("Object Brooker heruntergefahren");
		return;
	}
}
