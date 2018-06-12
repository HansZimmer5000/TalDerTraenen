package mware_lib;

/**
 * Nameservice class.
 * 
 * @author Mert Siginc
 * 
 * In dieser Klasse werden die Anfragen an den ComHandler weitergegeben.
 *
 */

public class NameService implements INameService {

	private boolean debug;
	private ComHandler conNS;

	public NameService(ComHandler con, boolean debug) {
		this.debug = debug;
		this.conNS = con;
		Util.println("NS-Referen wurde erzeugt", this.debug);
	}
	
	@Override
	public void rebind(Object servant, String name) {
		conNS.sendToNs(servant, name, "rebind");
		return;
	}

	@Override
	public Object resolve(String name) {
		Object ret = conNS.sendToNs(null, name, "resolve");
		return ret;
	}

}
