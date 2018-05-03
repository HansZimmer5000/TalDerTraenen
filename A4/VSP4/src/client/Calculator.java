package client;

import java.io.IOException;
import java.net.UnknownHostException;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class Calculator extends UnicastRemoteObject implements math_ops._CalculatorImplBase {

	private static final long serialVersionUID = -7247642077236723001L;

	protected Calculator() throws RemoteException {
		super();
	}

	public double add(double a, double b) {
		return a + b;
	}

	public static void main(String[] args) throws UnknownHostException, IOException, ClassNotFoundException {

		ObjectBroker objBroker = ObjectBroker.init("", 55555, false);
		mware_lib.INameService nameSvc = objBroker.getNameService();
		System.out.println(nameSvc);
		nameSvc.rebind((Object) new Calculator(), "zumsel");
		objBroker.shutDown();
		System.out.println("Service wurde angemeldet");
	}

}
