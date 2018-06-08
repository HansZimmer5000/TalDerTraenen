package starter;

import java.io.IOException;

import math_ops.*;
import mware_lib.*;
import name_ops._NameImplBase;
import name_ops._NameImplBaseStub;

public class CalculatorService implements _CalculatorImplBase {
	
	protected CalculatorService() {
		super();
	}

	public int add(int a, int b) {
		return a + b;
	}

	public static void main(String[] args) throws IOException, ClassNotFoundException {
		ObjectBroker objBroker = new ObjectBroker(false);
		
		SocketCommunicator nameserviceCommunicator = new SocketCommunicator("", 55555);
		_NameImplBase nameserviceClient = _NameImplBase.narrowCast(nameserviceCommunicator);
		
		CalculatorService calculatorServer = new CalculatorService();
		
		String calculatorServiceServerSocketString = objBroker.startNewService(calculatorServer);
		objBroker.registerNewService("calculator", calculatorServiceServerSocketString);
		nameserviceClient.rebind(calculatorServiceServerSocketString, "calculator");
		
		objBroker.shutDown();
		System.out.println("Service wurde angemeldet");
	}

}
