package client;

import java.io.IOException;
import java.net.UnknownHostException;
import java.rmi.RemoteException;

import math_ops._CalculatorImplBase;
import mware_lib.INameService;
import mware_lib.ObjectBroker;

public class Client {

	public static void main(String[] args) throws UnknownHostException, IOException, ClassNotFoundException {

		ObjectBroker objBroker = initObjectBroker();
		
		INameService nameSvc = getNameService(objBroker);
		
		Object rawObjRef = getZumselObjRef(nameSvc);
		_CalculatorImplBase remoteObj = getRemoteObj(rawObjRef);
		
		calculateAddition(remoteObj);
		
		shutdownObjBroker(objBroker);

	}
	
	private static ObjectBroker initObjectBroker() throws UnknownHostException, IOException {
		ObjectBroker objBroker = ObjectBroker.init("localhost", 55555, false);
		return objBroker;
	}

	private static INameService getNameService(ObjectBroker objBroker) throws ClassNotFoundException, IOException {
		INameService nameSvc = objBroker.getNameService();
		System.out.println(nameSvc);
		System.out.println("NS-Referenz erhalten!");
		return nameSvc;
	}


	private static Object getZumselObjRef(INameService nameSvc) throws RemoteException {
		Object rawObjRef = nameSvc.resolve("zumsel");
		System.out.println(rawObjRef);
		System.out.println("Service-Referenz erhalten!");
		return rawObjRef;
	}
	
	private static _CalculatorImplBase getRemoteObj(Object rawObjRef) throws RemoteException {
		_CalculatorImplBase remoteObj = _CalculatorImplBase.narrowCast(rawObjRef);
		System.out.println(rawObjRef);
		return remoteObj;
	}
	
	private static void calculateAddition(_CalculatorImplBase remoteObj) throws RemoteException {
		System.out.println("Methodenaufruf wird initiiert");
		double calcNum = remoteObj.add(20d,30d);
		System.out.println("Rechnung wurde durchgef�hr das Ergebnis ist: " + calcNum);
	}
	
	private static void shutdownObjBroker(ObjectBroker objBroker) throws IOException {
		objBroker.shutDown();
	}
}
