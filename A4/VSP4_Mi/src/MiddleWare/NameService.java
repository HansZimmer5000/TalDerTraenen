package MiddleWare;

import java.util.HashMap;
import java.util.Map;

import mware_lib.*;

public class NameService implements INameService{
	
	Map<String, Object> services = new HashMap<String, Object>();
		
	@Override
	public synchronized void rebind(Object servant, String name) {
		services.put(name, servant);
		System.out.println(name+ " wurde im NS registriert");
		
	}

	@Override
	public synchronized Object resolve(String name) {
		return services.get(name);
	}

}
