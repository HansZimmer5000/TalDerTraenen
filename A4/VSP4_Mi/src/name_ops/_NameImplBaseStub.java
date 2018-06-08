package name_ops;

import mware_lib.SocketCommunicator;

public class _NameImplBaseStub extends _NameImplBase {

	private SocketCommunicator conNS;

	public _NameImplBaseStub(Object con) {
		this.conNS = (SocketCommunicator) con;
	}

	public void rebind(String servantSocket, String name) {
		conNS.sendToService("rebind", "String " + servantSocket + ",String " + name);
	}

	public Object resolve(String name) {
		String socketString = conNS.sendToService("resolve", "String " + name);
		
		String[] splitSocketString = socketString.split(":");
		String host = splitSocketString[0];
		int port = Integer.valueOf(splitSocketString[1]);
		
		SocketCommunicator connectionToService = new SocketCommunicator(host, port);
		return connectionToService;
	}

}
