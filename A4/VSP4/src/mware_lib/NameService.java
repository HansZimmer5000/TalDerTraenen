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
		try {
			conNS.sendToNs(servant, name, "rebind");
		} catch (MwareException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return;
	}

	@Override
	public Object resolve(String name) {
		Object ret;
		try {
			ret = conNS.sendToNs(null, name, "resolve");
			return ret;
		} catch (MwareException e) {
			// TODO Auto-generated catch block
			Util.println("C> Es gabs keinen Eintrag im NS", debug);
			return e;
		}
	}

}
