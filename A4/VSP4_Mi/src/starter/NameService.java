package starter;

import java.io.IOException;
import java.net.ServerSocket;
import java.util.HashMap;
import java.util.Map;

import mware_lib.*;
import name_ops._NameImplBase;

public class NameService extends _NameImplBase{
	
	Map<String, String> services;
	
	public NameService(ObjectBroker objectBroker) throws IOException {
		this.services = new HashMap<String, String>();
		
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
		ObjectBroker objectBroker = ObjectBroker.init();
		new NameService(objectBroker);
		System.out.println("NameService registered locally.");
	}
}
