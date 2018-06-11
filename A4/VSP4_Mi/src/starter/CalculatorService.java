package starter;

import java.io.IOException;

import math_ops._CalculatorImplBase;
import mware_lib.ObjectBroker;
import mware_lib.ObjectReference;
import name_ops._NameImplBase;

public class CalculatorService extends _CalculatorImplBase {

	String serviceServerSocketString;
	
	public CalculatorService(ObjectBroker objectBroker) throws IOException {
		this.serviceServerSocketString = objectBroker.startNewService(this);
		objectBroker.registerNewService(_CalculatorImplBase.SERVICE_NAME, this.serviceServerSocketString);
	}
	
	private String getServiceServerSocketString() {
		return this.serviceServerSocketString;
	}
	
	public int add(int a, int b) {
		return a + b;
	}
	
	public double div(double a, double b) {
		return a / b;
	}

	public static void main(String[] args) throws IOException, ClassNotFoundException {
		ObjectBroker objectBroker = ObjectBroker.init();
		
		CalculatorService calculatorServer = new CalculatorService(objectBroker);
		String calculatorServiceServerSocketString = calculatorServer.getServiceServerSocketString();
		
		ObjectReference nameServiceObjectReference = ObjectReference.init(objectBroker, "", _NameImplBase.NAMESERVICEPORT);
		_NameImplBase nameserviceClient = _NameImplBase.narrowCast(nameServiceObjectReference);
		//TODO: Register in objectBroker
		nameserviceClient.rebind(calculatorServiceServerSocketString, _CalculatorImplBase.SERVICE_NAME);
		
		objectBroker.shutDown();
		System.out.println("CalculatorService registered locally and in nameservice.");
	}

}
