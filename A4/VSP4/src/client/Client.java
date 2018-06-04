package client;

import java.io.IOException;
import java.net.UnknownHostException;
import math_ops._CalculatorImplBase;
import mware_lib.ObjectBroker;

public class Client {

	public static void main(String[] args) throws UnknownHostException, IOException, ClassNotFoundException {

		ObjectBroker objBroker = ObjectBroker.init("localhost", 55555, false);
		mware_lib.INameService nameSvc = objBroker.getNameService();
		System.out.println(nameSvc);

		System.out.println("NS-Referenz erhalten!");
		Object rawObjRef = nameSvc.resolve("zumsel");
		System.out.println(rawObjRef);
		System.out.println("Service-Referenz erhalten!");
		_CalculatorImplBase remoteObj = _CalculatorImplBase.narrowCast(rawObjRef);

		System.out.println("Methodenaufruf wird initiiert");
		double calcNum = remoteObj.add(20d,30d);
		System.out.println("Rechnung wurde durchgeführ das Ergebnis ist: " + calcNum);

		objBroker.shutDown();
	
		
	}

}
