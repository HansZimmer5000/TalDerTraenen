package mware_lib;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.ObjectOutputStream;
import java.io.PrintStream;
import java.net.Socket;
import java.rmi.NoSuchObjectException;
import java.rmi.server.UnicastRemoteObject;

public class SkeletonThread extends Thread {

	private Socket cSocket;
	private BufferedReader in;
	private ObjectOutputStream objOut;
	private INameService nameService;
	private PrintStream syso;

	public SkeletonThread(Socket clientSocket, INameService nameService, PrintStream syso) throws IOException {
		this.cSocket = clientSocket;
		this.in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
		this.objOut = new ObjectOutputStream(clientSocket.getOutputStream());
		this.nameService = nameService;
		this.syso = syso;
		syso.println(clientSocket.getInetAddress() + " hat sich verbunden!");
	}

	@Override
	public void run() {
		String msgFromClient;
		while (cSocket.isConnected()) {
			try {
				msgFromClient = in.readLine();
				handleMsgFromClient(msgFromClient);


			} catch (IOException e) {
				syso.println(cSocket.getInetAddress() + " disconected!");
				return;
			}
		}
	}

	private void handleMsgFromClient(String msgFromClient) throws NoSuchObjectException, IOException {
		syso.println("folgende Nachricht wurde erhalten " + msgFromClient);
		if (msgFromClient.equals("GET_NS")) {
			getNameService();
		} else if (msgFromClient.equals("KILL")) {
			kill();
		} else {
			syso.println("get an unkown command " + msgFromClient + " from  " + cSocket.getInetAddress());
		}
		
	}

	private void getNameService() throws NoSuchObjectException, IOException {
		objOut.writeObject(UnicastRemoteObject.toStub(nameService));
		syso.println("NS Referenz wurde an " + cSocket.getInetAddress() + " gesendet!");
	}

	private void kill() throws IOException {
		in.close();
		objOut.close();
		cSocket.close();
		syso.println(cSocket.getInetAddress() + " disconected!");
		return;
	}
	

}
