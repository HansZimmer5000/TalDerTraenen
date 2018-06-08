package starter;

import java.io.IOException;

import calculator._CalculatorImplBase;
import mware_lib.Communicator;
import mware_lib.ObjectBroker;
import nameservice._NameserviceImplBase;

public class CalculatorClient {

	public static void main(String[] args) throws IOException  {

		ObjectBroker objBroker = new ObjectBroker(false);
		
		Communicator nameserviceCommunicator = new Communicator("", 55555);
		_NameserviceImplBase nameserviceClient = _NameserviceImplBase.narrowCast(nameserviceCommunicator);
		
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
