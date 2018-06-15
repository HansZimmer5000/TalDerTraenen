package MiddleWare;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;

/**
 * MiddlewareSkeleton class.
 * 
 * @author Mert Siginc Diese Klasse beantwortet die Anfragen der Clients und
 *         Services
 *
 */

public class MiddleWareSkeleton extends Thread {

	private Socket client;
	private NameService ns;
	private BufferedWriter os;
	private BufferedReader is;

	public MiddleWareSkeleton(Socket acceptedSocket, NameService nameService) {
		this.client = acceptedSocket;
		this.ns = nameService;
		System.out.println("MW> ein client hat sich verbunden");
	}

	@Override
	public void run() {

		try {
			is = new BufferedReader(new InputStreamReader(client.getInputStream()));
			os = new BufferedWriter(new OutputStreamWriter(client.getOutputStream()));
			String msgFromClient;
			while (client.isConnected()) {
				// Protokoll == Host | Port | NsName | Command(rebind)
				msgFromClient = is.readLine();
				if (msgFromClient != "" && msgFromClient != null) {
					System.out.println("MW> received Msg:" + msgFromClient);
					String[] splitedMsg = msgFromClient.split("\\|");
					// wird geprüft welcher command gesendet wurde
					switch (splitedMsg[3]) {
					case "rebind":
						ns.rebind(new String(splitedMsg[0] + "|" + splitedMsg[1]), splitedMsg[2]);
						shutdown();
						return;

					case "resolve":
						String msgToClinet = (String) ns.resolve(splitedMsg[2]);
						os.write(msgToClinet);
						os.flush();
						System.out.println("MW> Service wurde an Client weitergeleitet " + msgToClinet);
						shutdown();
						return;
					}
				}
			}

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return;
		}

	}

	private void shutdown() throws IOException {
		is.close();
		os.close();
		client.close();
		this.interrupt();
	}

}
