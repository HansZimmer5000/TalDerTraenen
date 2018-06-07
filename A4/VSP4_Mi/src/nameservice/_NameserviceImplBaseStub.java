package nameservice;

import mware_lib.Communicator;

public class _NameserviceImplBaseStub implements _NameserviceImplBase {

	private Communicator conNS;

	public _NameserviceImplBaseStub(Object con) {
		this.conNS = (Communicator) con;
	}

	@Override
	public void rebind(String servantSocket, String name) {
		conNS.sendToNs(servantSocket, name, "rebind");
	}

	@Override
	public Object resolve(String name) {
		Object ret = conNS.sendToNs(null, name, "resolve");
		return ret;
	}

}
