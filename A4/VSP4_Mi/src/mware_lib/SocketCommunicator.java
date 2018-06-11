package mware_lib;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map.Entry;

public class SocketCommunicator {

	private HashMap<String, SocketHolder> hostPortToSocketHolder;

	private SocketCommunicator() {
		this.hostPortToSocketHolder = new HashMap<>();
	}

	public static SocketCommunicator init() {
		return new SocketCommunicator();
	}

	public synchronized String sendMethodAndParametersToServiceAndWaitForAnswer(String serviceServerSocketString, String methodeName,
			String parameters) {

		String result = null;
		try {
			SocketHolder socketToService = getOrCreateSocketHolder(serviceServerSocketString);
			sendToSocket(socketToService, methodeName, parameters);
			result = waitForResult(socketToService);
		} catch (IOException e) {
			e.printStackTrace();
		}
		return result;
	}

	private void sendToSocket(SocketHolder socketHolder, String methodeName, String parameters) throws IOException {
		BufferedWriter writer = socketHolder.getWriter();

		writer.write(methodeName + "(" + parameters + ")\n");
		writer.flush();
		//out.close();
	}

	private String waitForResult(SocketHolder socketHolder) throws IOException {
		BufferedReader reader = socketHolder.getReader();

		System.out.println("Wait for response.");
		String result = reader.readLine();

		//in.close();
		return result;
	}

	public void closeAllSockets() throws IOException {
		SocketHolder currentSocketHolder;
		for (Entry<String, SocketHolder> pair : this.hostPortToSocketHolder.entrySet()) {
			currentSocketHolder = pair.getValue();
			this.sendToSocket(currentSocketHolder, "shutdown", "");
			currentSocketHolder.close();
		}
		this.hostPortToSocketHolder.clear();
		System.out.println("All Sockets Closed!");
	}

	private SocketHolder getOrCreateSocketHolder(String serviceServerSocketString) {
		SocketHolder foundSocketHolder = null;

		foundSocketHolder = this.hostPortToSocketHolder.get(serviceServerSocketString);

		if (foundSocketHolder == null) {
			try {
				String[] splitServiceServerSocketString = serviceServerSocketString.split(":");
				String host = splitServiceServerSocketString[0];
				int port = Integer.valueOf(splitServiceServerSocketString[1]);

				Socket createdSocket = new Socket(host, port);
				SocketHolder createdSocketHolder = new SocketHolder(createdSocket);
				this.hostPortToSocketHolder.put(serviceServerSocketString, createdSocketHolder);

				foundSocketHolder = createdSocketHolder;
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		return foundSocketHolder;
	}
}
