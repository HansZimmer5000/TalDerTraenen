package starter;

import java.io.IOException;
import java.net.ServerSocket;
import java.util.HashMap;
import java.util.Map;

import mware_lib.*;
import name_ops._NameImplBase;

public class NameService extends _NameImplBase{
	
	Map<String, String> services;
	String serviceServerSocketString;
	
	public NameService() throws IOException {
		this.services = new HashMap<String, String>();
		
		ObjectBroker objectBroker = new ObjectBroker();
		ServerSocket nameServiceServerSocket = new ServerSocket(_NameImplBase.NAMESERVICEPORT);
		String nameServiceServerSocketString = objectBroker.startNewService(this, nameServiceServerSocket);
		System.out.println("Server wurde gestartet und hoert auf: " + nameServiceServerSocketString);
		
		objectBroker.registerNewService("nameservice", nameServiceServerSocketString);
		objectBroker.shutDown();
	}
	
	@Override
	public synchronized void rebind(String servantSocket, String name) {
		services.put(name, servantSocket);
		System.out.println("New registration in nameservice: " + name);
	}
	
	@Override
	public synchronized Object resolve(String name) {
		return services.get(name);
	}
	
	public static void main(String[] args) throws IOException{
		new NameService();
		System.out.println("NameService registered locally.");
	}
}
