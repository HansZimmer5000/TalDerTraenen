package mware_lib;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.net.UnknownHostException;

public class SocketCommunicator {

	private Socket socketToService;
	private BufferedReader in;
	private BufferedWriter out;

	public SocketCommunicator(String host, int port) {
		try {
			this.socketToService = new Socket(host, port);
			this.in = new BufferedReader(new InputStreamReader(socketToService.getInputStream()));
			this.out = new BufferedWriter(new OutputStreamWriter(socketToService.getOutputStream()));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public String sendToService(String methodeName, String params) {
		// TODO: Close Sockets at some point!
		String result = null;
		try {
			this.out.write(methodeName + "(" + params + ")\n");
			this.out.flush();

			System.out.println("Warte auf Antwort");
			result = this.in.readLine();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return result;
	}
}
