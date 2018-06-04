package mware_lib;

import java.io.IOException;
import java.io.PrintStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class MiddleWare extends Thread {

	private static final int DEFAULT_PORT = 55555;
	private static int port;
	private boolean online = true;
	private INameService nameService;
	private PrintStream syso = System.out;

	private MiddleWare(int port) throws RemoteException {
		this.port = port;
		nameService = new NameService();
		System.out.println("Init done");
	}

	@Override
	public void run() {
		try {
			ServerSocket sSocket = new ServerSocket(port);
			System.out.println(
					"Server wurde gestartet und hoert auf: " + sSocket.getInetAddress() + ":" + sSocket.getLocalPort());

			while (online) {
				Socket newClientSocket = sSocket.accept();
				new SkeletonThread(newClientSocket, nameService, syso).start();
			}

			sSocket.close();

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public static void main(String[] args) throws RemoteException {
		int port = getPort(args);
		System.out.println("Got Port " + port);
		new MiddleWare(port).start();
	}

	private static int getPort(String[] args) {
		if (args.length == 1) {
			return new Integer(args[0]);
		} else {
			return DEFAULT_PORT;
		}
	}

}
