package mware_lib;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;

public class SkeletonThread extends Thread {

	private Socket cSocket;
	private BufferedReader in;
	private BufferedWriter out;
	private Object servant;

	public SkeletonThread(Socket clientSocket, Object servant) throws IOException {
		this.cSocket = clientSocket;
		this.in = new BufferedReader(new InputStreamReader(cSocket.getInputStream()));
		this.out = new BufferedWriter(new OutputStreamWriter(cSocket.getOutputStream()));
		this.servant = servant;		
	}

	@Override
	public void run() {
		String msgFromClient;
		while (cSocket.isConnected()) {
			try {
				msgFromClient = in.readLine();
				//TODO:: PROTOKOLL BESTIMMEN!!!!
				if (msgFromClient.equals("doMethod")) {
					out.write( this.servant.getClass().getMethod("methodeName", "param1".getClass(), "param2".getClass()).toString());		
				}  else {
					System.out.println("get an unkown command " + msgFromClient + " from  " + cSocket.getInetAddress());
				}

			} catch (IOException | NoSuchMethodException | SecurityException e) {
				System.out.println(cSocket.getInetAddress() + " disconected!");
				return;
			}
		}
	}
}
