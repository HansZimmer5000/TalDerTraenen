package mware_lib;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;

/**
 * ComHandler class.
 * 
 * @author Mert Siginc
 * 
 *         In dieser Klasse wird die kommunikation zwischen MW und Service
 *         behandelt
 *
 */

public class ComHandler {

	private String host;
	private int port;
	private Boolean debug;
	private ServiceListener serviceServer;
	private Socket socketOut;
	private BufferedReader is;
	private BufferedWriter os;

	public ComHandler(String host, int port, Boolean debug) {
		this.host = host;
		this.port = port;
		this.debug = debug;
		try {
			socketOut = new Socket(this.host, this.port);
			is = new BufferedReader(new InputStreamReader(socketOut.getInputStream()));
			os = new BufferedWriter(new OutputStreamWriter(socketOut.getOutputStream()));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public Object sendToNs(Object servant, String nsName, String command) {
		try {
			switch (command) {
			case "rebind":
				Util.println("rebind anfrage versendet", debug);
				serviceServer = new ServiceListener(servant, debug);
				serviceServer.start();
				os.write(serviceServer.getServerSocket().getInetAddress().getHostAddress() + "|"
						+ serviceServer.getServerSocket().getLocalPort() + "|" + nsName + "|" + command);
				os.flush();
				shutdown();
				return null;
			case "resolve":
				Util.println("resolve anfrage versendet", debug);
				os.write("null" + "|" + "null" + "|" + nsName + "|" + command + "\n");
				os.flush();
				String answerFromNS = is.readLine();
				Util.println("entferntes Object erhalten: " + answerFromNS, debug);
				shutdown();
				return answerFromNS;
			}
			Util.println("falscher command übergeben " + command, debug);
			shutdown();
			return new MwareException().toString();
		} catch (IOException e) {
			e.printStackTrace();

		}

		return 1;
	}

	public Object sendToService(String methodeName, String params) {
		try {
			Util.println("Anfrage an Service gesendet", debug);
			os.write(methodeName + "|" + params + "\n");
			os.flush();
			String msgFromNS = is.readLine();
			if (testException(msgFromNS)) {
				Util.println("Exception erhalten: " + msgFromNS, debug);
				return msgFromNS;
			}
			Util.println("Antwort erhalten von Service erhalten: " + msgFromNS, debug);
			shutdown();
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

	public void shutdown() throws IOException {
		is.close();
		os.close();
		socketOut.close();
	}

	private Boolean testException(String msg) {
		return (msg == new MwareException().toString());
	}

}
