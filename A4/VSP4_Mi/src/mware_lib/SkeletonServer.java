package mware_lib;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public class SkeletonServer extends Thread {

	private final static int PORT_START = 15001;
	private ServerSocket serverSocket;
	private Object servant;

	public static SkeletonServer init(Object servant, ServerSocket serverSocket) {
		return new SkeletonServer(servant, serverSocket);
	}

	public static SkeletonServer init(Object servant) {
		ServerSocket serverSocket = createServerSocketWithNextAvailablePort(PORT_START);
		return init(servant, serverSocket);
	}

	private SkeletonServer(Object servant, ServerSocket serverSocket) {
		this.servant = servant;
		this.serverSocket = serverSocket;
		System.out.println("Service Server started at: " + this.serverSocket.getInetAddress() + ":"
				+ this.serverSocket.getLocalPort());
	}

	@Override
	public void run() {
		Socket newClientSocket;
		while (!this.isInterrupted()) {
			try {
				newClientSocket = serverSocket.accept();
				System.out.println("Got new Connection / Request!");
				SkeletonThread.init(newClientSocket, this.servant).start();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		try {
			this.serverSocket.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private static ServerSocket createServerSocketWithNextAvailablePort(int port) {
		try {
			return new ServerSocket(port);
		} catch (IOException e) {
			return createServerSocketWithNextAvailablePort(port + 1);
		}
	}
	
	public String getServerSocketAsString() {
		return this.serverSocket.getInetAddress().getHostAddress() + 
				":" + this.serverSocket.getLocalPort();
	}
}
