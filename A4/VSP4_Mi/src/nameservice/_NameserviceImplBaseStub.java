package nameservice;

import mware_lib.Communicator;

public class _NameserviceImplBaseStub implements _NameserviceImplBase {

	private Communicator conNS;

	public _NameserviceImplBaseStub(Object con) {
		this.conNS = (Communicator) con;
	}

	@Override
	public void rebind(String servantSocket, String name) {
		conNS.sendToService("rebind", "String " + servantSocket + ",String " + name);
	}

	@Override
	public Object resolve(String name) {
		String socketString = conNS.sendToService("resolve", "String " + name);
		
		String[] splitSocketString = socketString.split(":");
		String host = splitSocketString[0];
		int port = Integer.valueOf(splitSocketString[1]);
		
		Communicator connectionToService = new Communicator(host, port);
		return connectionToService;
	}

}
