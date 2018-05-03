package mware_lib;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.Map;

public class NameService extends UnicastRemoteObject implements INameService {

	private static final long serialVersionUID = -8397203729215629656L;
	private Map<String, Object> register = new HashMap<String, Object>();

	protected NameService() throws RemoteException {
		super();
	}

	@Override
	synchronized public void rebind(Object servant, String name) throws RemoteException{
		register.put(name, servant);
		System.out.println("Service mit dem Namen " + name + " wurde registriert!");
	}

	@Override
	synchronized public Object resolve(String name) throws RemoteException{
		return register.get(name);
	}

}
