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

/**
 * SkeletonThread class.
 * 
 * @author Mert Siginc In dieser Klasse werden die Anfragen vom Client
 *         beantwortet
 *
 */

public class SkeletonThread extends Thread {

	private Socket cSocket;
	private BufferedReader in;
	private BufferedWriter out;
	private Object servant;
	private Boolean debug;

	public SkeletonThread(Socket clientSocket, Object servant, Boolean debug) throws IOException {
		this.debug = debug;
		this.cSocket = clientSocket;
		this.in = new BufferedReader(new InputStreamReader(cSocket.getInputStream()));
		this.out = new BufferedWriter(new OutputStreamWriter(cSocket.getOutputStream()));
		this.servant = servant;
		Util.println("S> Client hat sich angemeldet", debug);
	}

	@Override
	public void run() {
		String msgFromClient;
		while (!this.isInterrupted()) {
			try {
				msgFromClient = in.readLine();
				if (msgFromClient != null) {
					Util.println("Nachricht vom Client erhalten: " + msgFromClient, debug);
					String[] splitedMsg = msgFromClient.split("\\|");
					Object[] paramsRaw = splitedMsg[1].replaceAll(" ", "").split("\\,");
					Object[] params = new Object[paramsRaw.length];
					Class<?> paramClasses[] = new Class[paramsRaw.length];
					for (int i = 0; i < paramsRaw.length; i++) {
						Util.println("_" + paramsRaw[i] + "_", debug);
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
						Util.println("looking for Method named: " + splitedMsg[0] + " with "
								+ Arrays.asList(paramClasses).toString() + " Parameters values:"
								+ Arrays.asList(paramsRaw).toString(), debug);
						Method myMethod = servant.getClass().getDeclaredMethod(splitedMsg[0], paramClasses);
						Util.println(myMethod.toString(), debug);
						Util.println("Send Ergebnis an Client!", debug);
						out.write(myMethod.invoke(servant, params) + "\n");
						out.flush();
						shutdown();

					} catch (NoSuchMethodException | SecurityException | IllegalAccessException
							| IllegalArgumentException | InvocationTargetException e) {
						out.write(new MwareException().toString());
						shutdown();
					}

				}

			} catch (IOException e) {
				Util.println(cSocket.getInetAddress() + " disconected!", debug);
				return;
			}
		}
	}

	private void shutdown() throws IOException {
		in.close();
		out.close();
		cSocket.close();
		this.interrupt();
	}
}
