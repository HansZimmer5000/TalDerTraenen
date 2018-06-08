package starter;

import java.io.IOException;

import calculator._CalculatorImplBase;
import mware_lib.ObjectBroker;
import nameservice._NameserviceImplBase;

public class CalculatorClient {

	public static void main(String[] args) throws IOException  {

		ObjectBroker objBroker = ObjectBroker.init("localhost", 55555, false);
		
		Object rawObjRef = objBroker.getService("calculator");
		_CalculatorImplBase remoteObj = _CalculatorImplBase.narrowCast(rawObjRef);

		System.out.println("Methodenaufruf wird initiiert");
		double calcNum = remoteObj.add(20,30);
		System.out.println("Rechnung wurde durchgef�hrt das Ergebnis ist: " + calcNum);

		objBroker.shutDown();
	}

}
