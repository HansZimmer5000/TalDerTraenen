package starter;

import java.io.IOException;
import java.net.ServerSocket;
import java.util.HashMap;
import java.util.Map;

import mware_lib.*;
import nameservice._NameserviceImplBase;

public class NameserviceServer implements _NameserviceImplBase{
	
	Map<String, String> services = new HashMap<String, String>();
		
	@Override
	public synchronized void rebind(String servantSocket, String name) {
		services.put(name, servantSocket);
		System.out.println(name+ " wurde im NS registriert");
		
	}

	@Override
	public synchronized Object resolve(String name) {
		return services.get(name);
	}
	
	public static void main(String[] args) throws IOException{
		int port = 55555;
		NameserviceServer nameservice = new NameserviceServer();
		
		ServerSocket nsServer = new ServerSocket(port);
		System.out.println(
				"Server wurde gestartet und h�rt auf: " + nsServer.getInetAddress() + ":" + nsServer.getLocalPort());
		
		SkeletonServer serviceServer = new SkeletonServer(nameservice, nsServer);
		serviceServer.start();
	}
}
