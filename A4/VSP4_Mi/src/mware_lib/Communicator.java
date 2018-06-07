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

	public Object sendToNs(String servantSocket, String nsName, String command) {
		try {
			Socket socketToNS = new Socket(this.host, this.port);
			BufferedReader in = new BufferedReader(new InputStreamReader(socketToNS.getInputStream()));
			BufferedWriter out = new BufferedWriter(new OutputStreamWriter(socketToNS.getOutputStream()));
			
			switch (command) {
			case "rebind":
				System.out.println("rebind anfrage versendet");

				out.write("rebind(" +
							"String " +
								servantSocket + 
							",String " +
								nsName +
						")\n");
				//os.write(serviceServer.getServerSocket().getInetAddress().getHostAddress() + "|"
				//		+ serviceServer.getServerSocket().getLocalPort() + "|" + nsName + "|" + command);
				out.flush();
				in.close();
				out.close();
				break;
			case "resolve":
				out.write("resolve(" + 
							"String " + nsName + 
						")\n");
				//os.write("null" + "|" + "null" + "|" + nsName + "|" + command + "\n");
				out.flush();
				System.out.println("send (to: " + socketToNS.getInetAddress() +":"+socketToNS.getPort() + "):   -- resolve(" + "String " + nsName + ")");
				System.out.println(socketToNS.getInetAddress() + ":" + socketToNS.getPort());
				
				String answerFromNS = in.readLine();
				String nsAnswerSplited[] = answerFromNS.split(":");
				System.out.println("entferntes Object erhalten: " + answerFromNS);
				Communicator retObject = new Communicator(nsAnswerSplited[0], new Integer(nsAnswerSplited[1]), this.debug);

				in.close();
				out.close();
				socketToNS.close();
				return retObject;
			}
			socketToNS.close();
			
		} catch (UnknownHostException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}

	public Object sendToService(String methodeName, String params) {
		try {
			Socket socketToService = new Socket(this.host, this.port);
			BufferedReader is = new BufferedReader(new InputStreamReader(socketToService.getInputStream()));
			BufferedWriter os = new BufferedWriter(new OutputStreamWriter(socketToService.getOutputStream()));

			System.out.println("Anfrage an Service gesendet	");
			os.write(methodeName + "(" + 
						params + 
					")\n");
			//os.write(methodeName + "|" + params + "\n");
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
