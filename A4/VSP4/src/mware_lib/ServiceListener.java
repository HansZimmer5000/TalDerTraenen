package mware_lib;

import java.io.IOException;
import java.net.ServerSocket;

public class ServiceListener extends Thread {

	private ServerSocket ss;
	private final int PORT_START = 50000;
	private Object servant;

	public ServiceListener(Object servant) {
		this.servant = servant; 
		ss = getServerWithAviablePort(PORT_START);
		System.out.println("Service Server wurde gestartet");
	}

	@Override
	public void run() {
		while (!this.isInterrupted()) {
			try {
				new SkeletonThread(ss.accept(), this.servant).start();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private ServerSocket getServerWithAviablePort(int port) {
		try {
			return new ServerSocket(port);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			return getServerWithAviablePort(port + 1);
		}
	}

	public ServerSocket getServerSocket() {
		return ss;
	}
}
