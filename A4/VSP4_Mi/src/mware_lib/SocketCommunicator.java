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

	public SocketCommunicator(String host, int port) {
		try {
			this.socketToService = new Socket(host, port);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public String sendToService(String methodeName, String params) {
		try {
			BufferedReader is = new BufferedReader(new InputStreamReader(socketToService.getInputStream()));
			BufferedWriter os = new BufferedWriter(new OutputStreamWriter(socketToService.getOutputStream()));

			System.out.println("Anfrage an Service gesendet	");
			os.write(methodeName + "(" + 
						params + 
					")\n");
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
}
