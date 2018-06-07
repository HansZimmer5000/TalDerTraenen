package mware_lib;

public class NameService implements INameService {

	private boolean debug;
	private Communicator conNS;

	public NameService(Communicator con, boolean debug) {
		this.debug = debug;
		this.conNS = con;
	}

	@Override
	public void rebind(String servantSocket, String name) {
		conNS.sendToNs(servantSocket, name, "rebind");
		return;
	}

	@Override
	public Object resolve(String name) {
		Object ret = conNS.sendToNs(null, name, "resolve");
		return ret;
	}

}
