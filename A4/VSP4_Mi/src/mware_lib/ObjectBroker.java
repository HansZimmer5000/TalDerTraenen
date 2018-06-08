package mware_lib;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import nameservice._NameserviceImplBaseStub;

public class ObjectBroker {

	private boolean debug;
	private _NameserviceImplBaseStub ns = null;
	private Map<String, String> registeredServices = new HashMap<String, String>();

	public ObjectBroker(String host, int port, boolean debug) throws IOException {
		Communicator con = new Communicator(host, port, debug);
		ns = new _NameserviceImplBaseStub(con);
		this.debug = debug;
	}

	public static ObjectBroker init(String serviceHost, int port, boolean debug) throws IOException {
		return new ObjectBroker(serviceHost, port, debug);
	}

	public void registerNewService(String serviceName, Object service) {
		SkeletonServer serviceServer = new SkeletonServer(service);
		serviceServer.start();

		String serverSocketString = 
				serviceServer.getServerSocket().getInetAddress().getHostAddress() + 
				":" + serviceServer.getServerSocket().getLocalPort();
		
		this.registeredServices.put(serviceName, serverSocketString);
		
		if (this.ns != null) {
			this.ns.rebind(serverSocketString, serviceName);
		}
	}

	public Object getService(String serviceName) {
		Object foundService = this.registeredServices.get(serviceName);

		if (foundService == null && this.ns != null) {
			foundService = this.ns.resolve(serviceName);
		}
		
		return foundService;
	}

	// Beendet die Benutzung der Middleware in dieser Anwendung.
	public void shutDown() throws IOException {
		System.out.println("Object Brooker heruntergefahren");
		return;
	}
}
