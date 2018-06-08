package starter;

import java.io.IOException;
import java.net.ServerSocket;
import java.util.HashMap;
import java.util.Map;

import mware_lib.*;
import nameservice._NameserviceImplBase;

public class Nameservice implements _NameserviceImplBase{
	
	Map<String, String> services = new HashMap<String, String>();
		
	@Override
	public synchronized void rebind(String servantSocket, String name) {
		services.put(name, servantSocket);
		System.out.println(name+ " wurde im Nameservice registriert");
		
	}

	@Override
	public synchronized Object resolve(String name) {
		return services.get(name);
	}
	
	public static void main(String[] args) throws IOException{
		int port = 55555;

		ObjectBroker objBroker = new ObjectBroker(false);
		Nameservice nameservice = new Nameservice();
		
		ServerSocket nameserviceServerSocket = new ServerSocket(port);
		String nameserviceServerSocketString = objBroker.startNewService(nameservice, nameserviceServerSocket);
		System.out.println("Server wurde gestartet und hoert auf: " + nameserviceServerSocketString);
		
		objBroker.registerNewService("nameservice", nameserviceServerSocketString);
		System.out.println("Service wurde angemeldet");
		
		objBroker.shutDown();
		
	}
}
