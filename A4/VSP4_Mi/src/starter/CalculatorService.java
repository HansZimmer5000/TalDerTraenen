package starter;

import java.io.IOException;

import math_ops.*;
import mware_lib.*;
import name_ops._NameImplBase;
import name_ops._NameImplBaseStub;

public class CalculatorService extends _CalculatorImplBase {
	
	protected CalculatorService() {
		super();
	}

	public int add(int a, int b) {
		return a + b;
	}
	
	public double div(double a, double b) {
		return a / b;
	}

	public static void main(String[] args) throws IOException, ClassNotFoundException {
		ObjectBroker objBroker = new ObjectBroker();
		
		SocketCommunicator nameserviceCommunicator = new SocketCommunicator("", 15000);
		_NameImplBase nameserviceClient = _NameImplBase.narrowCast(nameserviceCommunicator);
		
		CalculatorService calculatorServer = new CalculatorService();
		
		String calculatorServiceServerSocketString = objBroker.startNewService(calculatorServer);
		objBroker.registerNewService("calculator", calculatorServiceServerSocketString);
		nameserviceClient.rebind(calculatorServiceServerSocketString, "calculator");
		
		objBroker.shutDown();
		System.out.println("Service wurde angemeldet");
	}

}
