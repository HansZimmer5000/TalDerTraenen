package starter;

import java.io.IOException;
import java.net.ServerSocket;
import java.util.HashMap;
import java.util.Map;

import mware_lib.*;
import name_ops._NameImplBase;

public class NameService extends _NameImplBase {

	Map<String, String> registeredServices;

	public NameService(ObjectBroker objectBroker) throws IOException {
		this.registeredServices = new HashMap<String, String>();

		ServerSocket nameServiceServerSocket = new ServerSocket(_NameImplBase.NAMESERVICEPORT);
		String nameServiceServerSocketString = objectBroker.startNewService(this, nameServiceServerSocket);
		System.out.println("Server started and listening to: " + nameServiceServerSocketString);

		objectBroker.registerNewService("nameservice", nameServiceServerSocketString);
	}

	@Override
	public synchronized void rebind(String servantSocket, String name) {
		registeredServices.put(name, servantSocket);
		System.out.println("New registration in nameservice: " + name);
	}

	@Override
	public synchronized Object resolve(String name) {
		return registeredServices.get(name);
	}

	public static void main(String[] args) throws IOException {
		ObjectBroker objectBroker = ObjectBroker.init();
		new NameService(objectBroker);
		objectBroker.shutDown();
		System.out.println("NameService registered locally.");
	}
}
