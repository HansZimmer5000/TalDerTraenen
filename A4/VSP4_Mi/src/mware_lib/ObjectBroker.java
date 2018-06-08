package mware_lib;

import java.io.IOException;
import java.net.ServerSocket;
import java.util.HashMap;
import java.util.Map;

public class ObjectBroker {

	private boolean debug;
	private Map<String, String> registeredServices = new HashMap<String, String>();

	public ObjectBroker(boolean debug) throws IOException {
		this.debug = debug;
	}

	public void startAndRegisterNewService(String serviceName, Object service) {
		String serverSocketString;
		
		serverSocketString = this.startNewService(service);
		this.registerNewService(serviceName, serverSocketString);
	}
	
	public String startNewService(Object service, ServerSocket serverSocket) {
		SkeletonServer serviceServer = new SkeletonServer(service, serverSocket);
		serviceServer.start();

		String serverSocketString = 
				serviceServer.getServerSocket().getInetAddress().getHostAddress() + 
				":" + serviceServer.getServerSocket().getLocalPort();
		return serverSocketString;
	}
	
	public String startNewService(Object service) {
		SkeletonServer serviceServer = new SkeletonServer(service);
		serviceServer.start();

		String serverSocketString = 
				serviceServer.getServerSocket().getInetAddress().getHostAddress() + 
				":" + serviceServer.getServerSocket().getLocalPort();
		return serverSocketString;
	}
	
	public void registerNewService(String serviceName, String serverSocketString) {
		this.registeredServices.put(serviceName, serverSocketString);
	}

	public Object getService(String serviceName) {
		Object foundService = this.registeredServices.get(serviceName);
		
		return foundService;
	}

	// Beendet die Benutzung der Middleware in dieser Anwendung.
	public void shutDown() throws IOException {
		System.out.println("Object Brooker heruntergefahren");
	}
}
