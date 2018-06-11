package mware_lib;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map.Entry;

public class SocketCommunicator {

	private HashMap<String, SocketHolder> hostPortToSocket;

	private SocketCommunicator() {
		this.hostPortToSocket = new HashMap<>();
	}

	public static SocketCommunicator init() {
		return new SocketCommunicator();
	}

	public String sendMethodAndParametersToServiceAndWaitForAnswer(String serviceServerSocketString, String methodeName,
			String parameters) {

		String result = null;
		try {
			SocketHolder socketToService = getOrCreateSocket(serviceServerSocketString);
			sendToSocket(socketToService, methodeName, parameters);
			result = waitForResult(socketToService);
		} catch (IOException e) {
			e.printStackTrace();
		}
		return result;
	}

	private void sendToSocket(SocketHolder socketHolder, String methodeName, String parameters) throws IOException {
		BufferedWriter out = socketHolder.getOut();

		out.write(methodeName + "(" + parameters + ")\n");
		out.flush();
		//out.close();
	}

	private String waitForResult(SocketHolder socketHolder) throws IOException {
		BufferedReader in = socketHolder.getIn();

		System.out.println("Wait for response.");
		String result = in.readLine();

		//in.close();
		return result;
	}

	public void closeAllSockets() throws IOException {
		SocketHolder currentSocket;
		for (Entry<String, SocketHolder> pair : this.hostPortToSocket.entrySet()) {
			currentSocket = pair.getValue();
			this.sendToSocket(currentSocket, "shutdown", "");
			currentSocket.close();
		}
		System.out.println("All Sockets Closed!");
	}

	private SocketHolder getOrCreateSocket(String serviceServerSocketString) {
		SocketHolder foundSocketHolder = null;

		foundSocketHolder = this.hostPortToSocket.get(serviceServerSocketString);

		if (foundSocketHolder == null) {
			try {
				String host = serviceServerSocketString.split(":")[0];
				int port = Integer.valueOf(serviceServerSocketString.split(":")[1]);

				Socket createdSocket = new Socket(host, port);
				SocketHolder createdSocketHolder = new SocketHolder(createdSocket);
				this.hostPortToSocket.put(serviceServerSocketString, createdSocketHolder);

				foundSocketHolder = createdSocketHolder;
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		return foundSocketHolder;
	}
}
