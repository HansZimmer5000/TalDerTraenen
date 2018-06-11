package starter;

import java.io.IOException;

import math_ops._CalculatorImplBase;
import name_ops._NameImplBase;
import mware_lib.ObjectBroker;
import mware_lib.ObjectReference;

public class CalculatorClient {

	ObjectBroker objectBroker;
	_CalculatorImplBase calculator;

	public CalculatorClient(ObjectBroker objectBroker) {
		this.objectBroker = objectBroker;
		this.calculator = null;
	}

	private void setObjectReferenceViaBrokerAndNameService(_NameImplBase nameserviceClient) {
		ObjectReference objectReference = null;
		String objectReferenceServerSocketString = this.objectBroker.getService(_CalculatorImplBase.SERVICE_NAME);
		if (objectReferenceServerSocketString == null) {
			objectReferenceServerSocketString = (String) nameserviceClient.resolve(_CalculatorImplBase.SERVICE_NAME);
		}

		if (objectReferenceServerSocketString != null) {
			objectReference = ObjectReference.init(objectBroker, objectReferenceServerSocketString);
			this.calculator = _CalculatorImplBase.narrowCast(objectReference);
		} else {
			System.out.println("Couldn't find Object Reference!");
		}
	}

	public void testfunction() throws IOException {
		System.out.println("Caluclation (add(20,30)) is done: " + calculator.add(20, 30));
		System.out.println("Calculation (div(10,3)) is done: " + calculator.div(10, 3));
	}

	public static void main(String[] args) throws IOException {
		ObjectBroker objectBroker = ObjectBroker.init();
		
		_NameImplBase nameClient = NameClient.createNameClient(objectBroker);

		CalculatorClient testclient = new CalculatorClient(objectBroker);
		testclient.setObjectReferenceViaBrokerAndNameService(nameClient);
		testclient.testfunction();
		
		objectBroker.shutDown();
	}
}
