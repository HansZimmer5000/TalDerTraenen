package mware_lib;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.net.UnknownHostException;

public class Communicator {

	private String host;
	private int port;
	private Boolean debug;
	private SkeletonServer serviceServer;

	public Communicator(String host, int port, Boolean debug) {
		this.host = host;
		this.port = port;
		this.debug = debug;
	}

	public Object sendToNs(Object servant, String nsName, String command) {
		try {
			Socket socketToNS = new Socket(this.host, this.port);
			BufferedReader is = new BufferedReader(new InputStreamReader(socketToNS.getInputStream()));
			BufferedWriter os = new BufferedWriter(new OutputStreamWriter(socketToNS.getOutputStream()));
			switch (command) {
			case "rebind":
				System.out.println("rebind anfrage versendet");
				serviceServer = new SkeletonServer(servant);
				serviceServer.start();

				// TODO:: Protokoll definieren Host | Port | NsName | Command(rebind)
				os.write(serviceServer.getServerSocket().getInetAddress().getHostAddress() + "|"
						+ serviceServer.getServerSocket().getLocalPort() + "|" + nsName + "|" + command);
				os.flush();
				is.close();
				os.close();
				socketToNS.close();
				return null;
			case "resolve":
				System.out.println("resolve anfrage versendet");
				// TODO:: Protokoll definieren null | null | NsName | Command(resolve)
				os.write("null" + "|" + "null" + "|" + nsName + "|" + command + "\n");
				os.flush();
				String answerFromNS = is.readLine();
				String nsAnswerSplited[] = answerFromNS.split("\\|");
				System.out.println("entferntes Object erhalten: " + answerFromNS);
				Communicator retObject = new Communicator(nsAnswerSplited[0], new Integer(nsAnswerSplited[1]), this.debug);

				is.close();
				os.close();
				socketToNS.close();
				return retObject;
			}
		} catch (UnknownHostException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return 1;
	}

	public Object sendToService(String methodeName, String params) {
		try {
			Socket socketToService = new Socket(this.host, this.port);
			BufferedReader is = new BufferedReader(new InputStreamReader(socketToService.getInputStream()));
			BufferedWriter os = new BufferedWriter(new OutputStreamWriter(socketToService.getOutputStream()));

			System.out.println("Anfrage an Service gesendet	");
			os.write(methodeName + "|" + params + "\n");
			os.flush();

			System.out.println("Warte auf Antwort");
			String msgFromNS = is.readLine();
			System.out.println("Antwort erhalten: " + msgFromNS);
			is.close();
			os.close();
			socketToService.close();
			return msgFromNS;

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	public String getHost() {
		return host;
	}

	public int getPort() {
		return port;
	}

}
