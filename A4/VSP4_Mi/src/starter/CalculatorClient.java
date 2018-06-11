package starter;

import java.io.IOException;

import math_ops._CalculatorImplBase;
import mware_lib.SocketCommunicator;
import name_ops._NameImplBase;
import mware_lib.ObjectBroker;

public class CalculatorClient {

	ObjectBroker objectBroker;
	_CalculatorImplBase remoteObject;

	public CalculatorClient() {
		this.objectBroker = new ObjectBroker();

		SocketCommunicator nameserviceCommunicator = new SocketCommunicator("", _NameImplBase.NAMESERVICEPORT);
		_NameImplBase nameserviceClient = _NameImplBase.narrowCast(nameserviceCommunicator);

		// TODO: Begruendung, warum wie ne Map (getService) in ObjectBroker haben
		// TODO: Generell: Variablennamen umbauen.
		Object objectReference = getObjectReference(objectBroker, nameserviceClient);
		if (objectReference == null) {
			System.out.println("Couldn't find Object Reference!");
		} else {
			this.remoteObject = _CalculatorImplBase.narrowCast(objectReference);
		}
	}

	private Object getObjectReference(ObjectBroker objBroker, _NameImplBase nameserviceClient) {
		Object rawObjRef = this.objectBroker.getService("calculator");
		if (rawObjRef == null) {
			rawObjRef = nameserviceClient.resolve("calculator");
		}
		return rawObjRef;
	}

	public void testfunction() throws IOException {
		System.out.println("Rechnung wurde durchgefuehrt das Ergebnis ist: " + remoteObject.add(20, 30));
		System.out.println("Rechnung wurde durchgefuehrt das Ergebnis ist: " + remoteObject.div(10, 3));
		objectBroker.shutDown();
	}

	public static void main(String[] args) throws IOException {
		new CalculatorClient().testfunction();
	}
}
