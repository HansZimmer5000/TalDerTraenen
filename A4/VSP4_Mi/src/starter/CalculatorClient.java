package starter;

import java.io.IOException;

import math_ops._CalculatorImplBase;
import name_ops._NameImplBase;
import mware_lib.ObjectBroker;
import mware_lib.ObjectReference;

public class CalculatorClient {

	ObjectBroker objectBroker;
	_CalculatorImplBase remoteObject;

	public CalculatorClient(ObjectBroker objectBroker) {
		this.objectBroker = objectBroker;
		this.remoteObject = null;
	}

	private void setObjectReferenceViaBrokerAndNameService(_NameImplBase nameserviceClient) {
		ObjectReference objectReference = null;
		String objectReferenceServerSocketString = this.objectBroker.getService(_CalculatorImplBase.SERVICE_NAME);
		if (objectReferenceServerSocketString == null) {
			objectReferenceServerSocketString = (String) nameserviceClient.resolve("calculator");
		}

		if (objectReferenceServerSocketString != null) {
			objectReference = ObjectReference.init(objectBroker, objectReferenceServerSocketString);
			this.remoteObject = _CalculatorImplBase.narrowCast(objectReference);
		} else {
			System.out.println("Couldn't find Object Reference!");
		}
	}

	public void testfunction() throws IOException {
		System.out.println("Caluclation (add(20,30)) is done: " + remoteObject.add(20, 30));
		System.out.println("Calculation (div(10,3)) is done: " + remoteObject.div(10, 3));
		objectBroker.shutDown();
	}

	public static void main(String[] args) throws IOException {
		ObjectBroker objectBroker = ObjectBroker.init();
		
		ObjectReference nameServiceObjectReference = ObjectReference.init(objectBroker, "", _NameImplBase.NAMESERVICEPORT);
		_NameImplBase nameserviceClient = _NameImplBase.narrowCast(nameServiceObjectReference);

		CalculatorClient testclient = new CalculatorClient(objectBroker);
		testclient.setObjectReferenceViaBrokerAndNameService(nameserviceClient);
		testclient.testfunction();
	}
}
