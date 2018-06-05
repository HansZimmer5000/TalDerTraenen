package MiddleWare;

import java.io.IOException;
import java.net.ServerSocket;

public class MiddleWare extends Thread {

	private static final int DEFAULT_PORT = 55555;
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
			System.out.println(
					"Server wurde gestartet und hört auf: " + nsServer.getInetAddress() + ":" + nsServer.getLocalPort());

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
		if (args.length == 1) {
			new MiddleWare(new Integer(args[0])).start();
		} else {
			new MiddleWare(DEFAULT_PORT).start();
		}
	}

}
