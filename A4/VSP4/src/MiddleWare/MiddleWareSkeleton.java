package MiddleWare;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;

public class MiddleWareSkeleton extends Thread {

	private Socket client;
	private NameService ns;
	private BufferedWriter os;
	private BufferedReader is;

	public MiddleWareSkeleton(Socket acceptedSocket, NameService nameService) {
		this.client = acceptedSocket;
		this.ns = nameService;
		System.out.println("ein client hat sich verbunden");
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
					System.out.println("received Msg:" + msgFromClient);
					String[] splitedMsg = msgFromClient.split("\\|");
					switch (splitedMsg[3]) {
					case "rebind":
						ns.rebind(new String(splitedMsg[0] + "|" + splitedMsg[1]), splitedMsg[2]);
						is.close();
						os.close();
						client.close();
						return;

					case "resolve":
						String msgToClinet = (String) ns.resolve(splitedMsg[2]);
						os.write(msgToClinet);
						os.flush();
						System.out.println("Service wurde an Client weitergeleitet " + msgToClinet);
						is.close();
						os.close();
						client.close();
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

}
