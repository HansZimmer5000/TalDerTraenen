package starter;

import java.io.IOException;

import math_ops.*;
import mware_lib.*;
import name_ops._NameImplBase;
import name_ops._NameImplBaseStub;

public class CalculatorService extends _CalculatorImplBase {

	String serviceServerSocketString;
	
	public CalculatorService() throws IOException {
		ObjectBroker objectBroker = new ObjectBroker();
		this.serviceServerSocketString = objectBroker.startNewService(this);
		objectBroker.registerNewService("calculator", this.serviceServerSocketString);
		objectBroker.shutDown();
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
		SocketCommunicator nameserviceCommunicator = new SocketCommunicator("", _NameImplBase.NAMESERVICEPORT);
		_NameImplBase nameserviceClient = _NameImplBase.narrowCast(nameserviceCommunicator);
		
		CalculatorService calculatorServer = new CalculatorService();
		String calculatorServiceServerSocketString = calculatorServer.getServiceServerSocketString();
		
		nameserviceClient.rebind(calculatorServiceServerSocketString, "calculator");
		
		System.out.println("CalculatorService registered locally and in nameservice.");
	}

}
