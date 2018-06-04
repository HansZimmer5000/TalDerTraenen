package mware_lib;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.net.UnknownHostException;

public class ComHandler {

	private String host;
	private int port;
	private Boolean debug;
	private ServiceListener serviceServer;

	public ComHandler(String host, int port, Boolean debug) {
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
				serviceServer = new ServiceListener(servant);
				serviceServer.start();

				// TODO:: Protokoll definieren Host | Port | NsName | Command(rebind)
				os.write(serviceServer.getServerSocket().getInetAddress().getHostAddress() + "|"
						+ serviceServer.getServerSocket().getLocalPort() + "|" + nsName + "|" + command);
				is.close();
				os.close();
				socketToNS.close();

				break;
			case "resolve":
				// TODO:: Protokoll definieren null | null | NsName | Command(resolve)
				os.write("null" + "|" + "null" + "|" + nsName + "|" + command);
				String nsAnswerSplited[] = is.readLine().split("|");
				ComHandler retObject = new ComHandler(nsAnswerSplited[0], new Integer(nsAnswerSplited[1]), this.debug);

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
		return null;
	}

	public Object sendToService(String methodeName, String[] params) {
		Socket socketToService;
		try {
			socketToService = new Socket(this.host, this.port);
			BufferedReader is = new BufferedReader(new InputStreamReader(socketToService.getInputStream()));
			BufferedWriter os = new BufferedWriter(new OutputStreamWriter(socketToService.getOutputStream()));

			os.write(methodeName + "|" + params.toString());
			return is.readLine();

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
