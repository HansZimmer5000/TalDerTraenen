package mware_lib;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Map.Entry;

public class SocketCommunicator {

	private HashMap<String, Socket> hostPortToSocket;

	public SocketCommunicator() {
		this.hostPortToSocket = new HashMap<>();
	}

	public String sendMethodAndParametersToServiceAndWaitForAnswer(String serviceServerSocketString, String methodeName,
			String parameters) {
		
		Socket socketToService = getOrCreateSocket(serviceServerSocketString);
		
		String result = null;
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader(socketToService.getInputStream()));
			BufferedWriter out = new BufferedWriter(new OutputStreamWriter(socketToService.getOutputStream()));
			
			out.write(methodeName + "(" + parameters + ")\n");
			out.flush();

			System.out.println("Wait for response.");
			result = in.readLine();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return result;
	}

	public void closeAllSockets() throws IOException {
		for (Entry<String, Socket> pair : this.hostPortToSocket.entrySet()) {
			pair.getValue().close();
		}
		System.out.println("All Sockets Closed!");
	}

	private Socket getOrCreateSocket(String serviceServerSocketString) {
		Socket foundSocket = null;

		foundSocket = this.hostPortToSocket.get(serviceServerSocketString);

		if (foundSocket == null) {
			try {
				String host = serviceServerSocketString.split(":")[0];
				int port = Integer.valueOf(serviceServerSocketString.split(":")[1]);
				
				Socket createdSocket = new Socket(host, port);
				this.hostPortToSocket.put(serviceServerSocketString, createdSocket);
				
				foundSocket = createdSocket;
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
		return foundSocket;
	}
}
