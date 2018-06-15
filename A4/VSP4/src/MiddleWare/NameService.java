package MiddleWare;

import java.util.HashMap;
import java.util.Map;

import mware_lib.INameService;

/**
 * Nameservice class.
 * 
 * @author Mert Siginc In dieser Klasse wird der Namenservice realisiert Die
 *         Methoden sind synchronized, da gleichzeitige zugriffe möglich sind
 *
 */
public class NameService implements INameService {

	Map<String, Object> services = new HashMap<String, Object>();

	@Override
	public synchronized void rebind(Object servant, String name) {
		services.put(name, servant);
		System.out.println("MW> " + name + " wurde im NS registriert");

	}

	@Override
	public synchronized Object resolve(String name) {
		System.out.println("MW> " + name + " wird rausgesucht");
		if (services.containsKey(name)) {
			return services.get(name);
		} else {
			return "noEntry";
		}
	}

}
