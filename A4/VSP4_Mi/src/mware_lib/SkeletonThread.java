package mware_lib;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.Socket;

public class SkeletonThread extends Thread {

	private Socket socketToClient;
	private BufferedReader in;
	private BufferedWriter out;
	private Object servant;

	private SkeletonThread(Socket clientSocket, Object servant) throws IOException {
		this.socketToClient = clientSocket;
		this.in = new BufferedReader(new InputStreamReader(socketToClient.getInputStream()));
		this.out = new BufferedWriter(new OutputStreamWriter(socketToClient.getOutputStream()));
		this.servant = servant;
	}

	public static SkeletonThread init(Socket clientSocket, Object servant) throws IOException {
		return new SkeletonThread(clientSocket, servant);
	}

	@Override
	public void run() {
		String incomingMessage;
		boolean socketIsOpen = this.socketToClient.isConnected();
		while (!this.isInterrupted() && socketIsOpen) {
			try {
				incomingMessage = in.readLine();
				if (incomingMessage != null) {
					socketIsOpen = executeInput(incomingMessage);
				}
			} catch (IOException e) {
				socketIsOpen = false;
			}
		}
		System.out.println("Connection is closed due to shutdown.");
	}

	private boolean executeInput(String msgFromClient) {
		System.out.println("Recevied message from Client: " + msgFromClient);
		boolean socketIsStillOpen = true;
		
		try {
			// TODO Oder mit Reflection (oder ohne) in Server Klasse (dann mit Verwendung
			// von einem Service Interface)
			ReceivedMessage message = new ReceivedMessage(msgFromClient);
			
			if (message.getMethodName().equals("shutdown")) {
				socketIsStillOpen = false;
			} else {
				Object returnValue = reflectAndInvokeMethod(servant.getClass(), message);
				this.out.write(returnValue + "\n");
				this.out.flush();
			}
		} catch (SecurityException | IllegalArgumentException | IOException e) {
			e.printStackTrace();
		}
		return socketIsStillOpen;
	}

	private Object reflectAndInvokeMethod(Class<?> reflectedClass, ReceivedMessage message) {
		Object returnValue = null;
		String methodName = message.getMethodName();
		Class<?>[] parameterClasses = message.getParameterClasses();
		Object[] parameterValues = message.getParameterValues();

		try {
			Method method = reflectedClass.getMethod(methodName, parameterClasses);
			returnValue = method.invoke(servant, parameterValues);
		} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException
				| SecurityException e) {
			e.printStackTrace();
		}
		return returnValue;
	}
}
