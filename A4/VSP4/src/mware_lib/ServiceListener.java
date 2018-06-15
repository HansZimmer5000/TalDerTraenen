package mware_lib;

import java.io.IOException;
import java.net.ServerSocket;

/**
 * ServiceListener class.
 * 
 * @author Mert Siginc In dieser Klasse wird eine neue Schnittstelle erzeugt um
 *         den Service für Clients anzubieten. Die Portnummern fangen bei 50000
 *         an und iterieren nach oben, bis sie ein freien Port finden.
 */

public class ServiceListener extends Thread {

	private ServerSocket ss;
	private final int PORT_START = 50000;
	private Object servant;
	private Boolean debug;

	public ServiceListener(Object servant, Boolean debug) {
		this.debug = debug;
		this.servant = servant;
		ss = getServerWithAviablePort(PORT_START);
		Util.println("S> Service Server wurde gestartet", debug);
	}

	@Override
	public void run() {
		while (!this.isInterrupted()) {
			try {
				new SkeletonThread(ss.accept(), this.servant, debug).start();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	// sucht nach einen port beginnt bei 50000
	private ServerSocket getServerWithAviablePort(int port) {
		try {
			return new ServerSocket(port);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			return getServerWithAviablePort(port + 1);
		}
	}

	public ServerSocket getServerSocket() {
		return ss;
	}
}
