package starter;

import java.io.IOException;
import java.net.ServerSocket;
import java.util.HashMap;
import java.util.Map;

import mware_lib.*;
import nameservice._NameImplBase;

public class NameService implements _NameImplBase{
	
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
		NameService nameservice = new NameService();
		
		ServerSocket nameServiceServerSocket = new ServerSocket(port);
		String nameServiceServerSocketString = objBroker.startNewService(nameservice, nameServiceServerSocket);
		System.out.println("Server wurde gestartet und hoert auf: " + nameServiceServerSocketString);
		
		objBroker.registerNewService("nameservice", nameServiceServerSocketString);
		System.out.println("Service wurde angemeldet");
		
		objBroker.shutDown();
		
	}
}
