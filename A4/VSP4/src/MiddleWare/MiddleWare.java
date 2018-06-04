package MiddleWare;

import java.io.IOException;
import java.io.PrintStream;
import java.net.ServerSocket;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

import mware_lib.INameService;
import mware_lib.NameService;
import mware_lib.SkeletonThread;

public class MiddleWare extends Thread {

	private static final int DEFAULT_PORT = 55555;
	private static int port;
	private boolean online = true;
	private INameService nameService;
	private PrintStream syso = System.out;

	private MiddleWare(int port) throws RemoteException {
		this.port = port;
		//nameService = new NameService();
	}

	@Override
	public void run() {
		try {
			ServerSocket sSocket = new ServerSocket(port);
			System.out.println(
					"Server wurde gestartet und hört auf: " + sSocket.getInetAddress() + ":" + sSocket.getLocalPort());

			while (online) {
				new SkeletonThread(sSocket.accept(), nameService, syso).start();
			}

			sSocket.close();

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public static void main(String[] args) {
		try {
			if (args.length == 1) {
				new MiddleWare(new Integer(args[0])).start();
			} else {
				new MiddleWare(DEFAULT_PORT).start();
			}
		} catch (NumberFormatException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (RemoteException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
