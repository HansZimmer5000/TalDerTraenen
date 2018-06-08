package mware_lib;

import java.io.IOException;
import java.util.Map;

import nameservice._NameserviceImplBaseStub;

public class ObjectBroker {

	private boolean debug;
	private _NameserviceImplBaseStub ns;
	private Map<String, Object> registeredObjects;

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

		String calculatorServerSocket = 
				serviceServer.getServerSocket().getInetAddress().getHostAddress()+":"+
				serviceServer.getServerSocket().getLocalPort();
		
		this.ns.rebind(calculatorServerSocket, serviceName);
	}
	
	public Object getService(String serviceName) {
		return this.ns.resolve(serviceName);
	}

	// Beendet die Benutzung der Middleware in dieser Anwendung.
	public void shutDown() throws IOException {
		System.out.println("Object Brooker heruntergefahren");
		return;
	}
}
