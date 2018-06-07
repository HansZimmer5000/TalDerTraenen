package mware_lib;

import java.io.IOException;
import java.net.ServerSocket;

public class SkeletonServer extends Thread {

	private ServerSocket ss;
	private final int PORT_START = 50000;
	private Object servant;

	public SkeletonServer(Object servant, ServerSocket ss){
		this.servant = servant;
		this.ss = ss;
		System.out.println("Service Server wurde gestartet: " + this.ss.getInetAddress() + ":" + this.ss.getLocalPort());
	}
	
	public SkeletonServer(Object servant) {
		this.servant = servant; 
		ss = getServerWithAviablePort(PORT_START);
		System.out.println("Service Server wurde gestartet: " + this.ss.getInetAddress() + ":" + this.ss.getLocalPort());
	}

	@Override
	public void run() {
		while (!this.isInterrupted()) {
			try {
				new SkeletonThread(ss.accept(), this.servant).start();
				System.out.println("Got new Connection / Request! " + this.servant.getClass());
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		try {
			this.ss.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private ServerSocket getServerWithAviablePort(int port) {
		try {
			return new ServerSocket(port);
		} catch (IOException e) {
			return getServerWithAviablePort(port + 1);
		}
	}

	public ServerSocket getServerSocket() {
		return ss;
	}
}
