package mware_lib;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.Socket;
import java.util.Arrays;

public class SkeletonThread extends Thread {

	private Socket cSocket;
	private BufferedReader in;
	private BufferedWriter out;
	private Object servant;

	private SkeletonThread(Socket clientSocket, Object servant) throws IOException {
		this.cSocket = clientSocket;
		this.in = new BufferedReader(new InputStreamReader(cSocket.getInputStream()));
		this.out = new BufferedWriter(new OutputStreamWriter(cSocket.getOutputStream()));
		this.servant = servant;
	}
	
	public static SkeletonThread init(Socket clientSocket, Object servant) throws IOException {
		return new SkeletonThread(clientSocket, servant);
	}

	@Override
	public void run() {
		String incomingMessage;
		while (!this.isInterrupted()) {
			try {
				//System.out.println("Waiting for input");

				incomingMessage = in.readLine();
				if (incomingMessage != null) {
					executeInput(incomingMessage);
				}
				;
			} catch (IOException e) {
				System.out.println(cSocket.getInetAddress() + " disconected!");
			}
		}
	}

	private void executeInput(String msgFromClient) {
		System.out.println("Recevied message from Client: " + msgFromClient);

		try {
			// TODO Oder mit Reflection (oder ohne) in Server Klasse (dann mit Verwendung
			// von einem Service Interface)
			Message message = new Message(msgFromClient);
			Object returnValue = reflectAndInvokeMethod(servant.getClass(), message);

			this.out.write(returnValue + "\n");
			this.out.flush();
		} catch (SecurityException | IllegalArgumentException | IOException e) {
			e.printStackTrace();
		}

	}

	private Object reflectAndInvokeMethod(Class<?> reflectedClass, Message message) {
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
