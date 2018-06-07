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

	public SkeletonThread(Socket clientSocket, Object servant) throws IOException {
		this.cSocket = clientSocket;
		this.in = new BufferedReader(new InputStreamReader(cSocket.getInputStream()));
		this.out = new BufferedWriter(new OutputStreamWriter(cSocket.getOutputStream()));
		this.servant = servant;
		System.out.println("Client hat sich angemeldet");
	}

	@Override
	public void run() {
		String msgFromClient;
		while (!this.isInterrupted()) {
			try {
				msgFromClient = in.readLine();
				System.out.println("Got New Message from Client! " + msgFromClient);
				if (msgFromClient != null) {

					String[] splitedMsg = msgFromClient.split("\\|");
					Object[] paramsRaw = splitedMsg[1].replaceAll(" ", "").split("\\,");
					Object[] params = new Object[paramsRaw.length];
					Class<?> paramClasses[] = new Class[paramsRaw.length];
					for (int i = 0; i < paramsRaw.length; i++) {
						System.out.println("_" + paramsRaw[i] + "_");
						try {
							params[i] = Double.parseDouble((String) paramsRaw[i]);
							paramClasses[i] = double.class;
						} catch (NumberFormatException | NullPointerException e) {
							if (paramsRaw[i] instanceof Integer) {
								paramClasses[i] = int.class;
								params[i] = new Integer((int) paramsRaw[i]);
							} else if (paramsRaw[i] instanceof String) {
								paramClasses[i] = String.class;
								params[i] = params[i];
							}
						}
					}

					try {
						System.out.println("looking for Method named: " + splitedMsg[0] + " with "
								+ Arrays.asList(paramClasses).toString() + " Parameters values:"
								+ Arrays.asList(paramsRaw).toString());
						Method myMethod = servant.getClass().getDeclaredMethod(splitedMsg[0], paramClasses);
						System.out.println(myMethod);
						System.out.println("Send Ergebnis an Client!");
						out.write(myMethod.invoke(servant, params) + "\n");
						out.flush();

					} catch (NoSuchMethodException | SecurityException | IllegalAccessException
							| IllegalArgumentException | InvocationTargetException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

				}

			} catch (IOException e) {
				System.out.println(cSocket.getInetAddress() + " disconected!");
				return;
			}
		}
	}
}
