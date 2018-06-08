package starter;

import java.io.IOException;

import math_ops._CalculatorImplBase;
import mware_lib.SocketCommunicator;
import name_ops._NameImplBase;
import mware_lib.ObjectBroker;

public class CalculatorClient {

	public static void main(String[] args) throws IOException  {

		ObjectBroker objBroker = new ObjectBroker(false);
		
		SocketCommunicator nameserviceCommunicator = new SocketCommunicator("", 55555);
		_NameImplBase nameserviceClient = _NameImplBase.narrowCast(nameserviceCommunicator);
		
		Object rawObjRef = objBroker.getService("calculator");
		if(rawObjRef == null) {
			rawObjRef = nameserviceClient.resolve("calculator");
		}
		_CalculatorImplBase remoteObj = _CalculatorImplBase.narrowCast(rawObjRef);

		System.out.println("Methodenaufruf wird initiiert");
		double calcNum = remoteObj.add(20,30);
		System.out.println("Rechnung wurde durchgef�hrt das Ergebnis ist: " + calcNum);

		objBroker.shutDown();
	}
}
