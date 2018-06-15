package MiddleWare;

import java.io.IOException;
import java.net.ServerSocket;

import mware_lib.Util;

/**
 * Middleware main class.
 * 
 * @author Mert Siginc In dieser Klasse startet man die Middleware
 *
 */

public class MiddleWare extends Thread {

	private int port;
	private NameService nameService;
	private Boolean online = true;

	private MiddleWare(int port) {
		this.port = port;
		nameService = new NameService();
	}

	@Override
	public void run() {
		try {
			ServerSocket nsServer = new ServerSocket(port);
			System.out.println("MW> Server wurde gestartet und hört auf: " + nsServer.getInetAddress() + ":"
					+ nsServer.getLocalPort());

			while (online) {
				new MiddleWareSkeleton(nsServer.accept(), nameService).start();
			}

			nsServer.close();

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public static void main(String[] args) {

		new MiddleWare(Util.checkPort(args)).start();

	}

}
