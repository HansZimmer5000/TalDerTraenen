package MiddleWare;

import java.util.HashMap;
import java.util.Map;

import mware_lib.*;

public class NameService implements INameService{
	
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
}
