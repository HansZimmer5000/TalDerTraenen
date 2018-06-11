package mware_lib;

import java.io.IOException;
import java.net.ServerSocket;
import java.util.HashMap;
import java.util.Map;

public class ObjectBroker {

	private Map<String, String> registeredServices;
	private SocketCommunicator communicator;

	private ObjectBroker() {
		this.registeredServices = new HashMap<>();
		this.communicator = SocketCommunicator.init();
	}
	
	public static ObjectBroker init() {
		return new ObjectBroker();
	}

	public void startAndRegisterNewService(String serviceName, Object service) {
		String serverSocketString;
		
		serverSocketString = this.startNewService(service);
		this.registerNewService(serviceName, serverSocketString);
	}
	
	public String startNewService(Object service, ServerSocket serverSocket) {
		SkeletonServer serviceServer = SkeletonServer.init(service, serverSocket);
		return startNewService(serviceServer);
	}
	
	public String startNewService(Object service) {
		SkeletonServer serviceServer = SkeletonServer.init(service);
		return startNewService(serviceServer);
	}
	
	private String startNewService(SkeletonServer serviceServer) {
		serviceServer.start();
		
		String serverSocketString = serviceServer.getServerSocketAsString();
		return serverSocketString;
	}
	
	public void registerNewService(String serviceName, String serverSocketString) {
		this.registeredServices.put(serviceName, serverSocketString);
	}

	public String getService(String serviceName) {
		String foundService = this.registeredServices.get(serviceName);
		return foundService;
	}
	
	public String sendMethodAndParametersToServiceAndWaitForAnswer(String serviceServerSocketString, String methodName, String parameters) {
		return this.communicator.sendMethodAndParametersToServiceAndWaitForAnswer(serviceServerSocketString, methodName, parameters);
	}

	// Beendet die Benutzung der Middleware in dieser Anwendung.
	public void shutDown() throws IOException {
		this.communicator.closeAllSockets();
		System.out.println("ObjectBroker is shutdown.");
	}
}
