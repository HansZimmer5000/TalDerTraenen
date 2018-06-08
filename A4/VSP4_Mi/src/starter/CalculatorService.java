package starter;

import java.io.IOException;

import calculator.*;
import mware_lib.*;
import nameservice._NameserviceImplBase;
import nameservice._NameserviceImplBaseStub;

public class CalculatorService implements _CalculatorImplBase {
	
	protected CalculatorService() {
		super();
	}

	public int add(int a, int b) {
		return a + b;
	}

	public static void main(String[] args) throws IOException, ClassNotFoundException {
		ObjectBroker objBroker = new ObjectBroker(false);
		
		Communicator nameserviceCommunicator = new Communicator("", 55555);
		_NameserviceImplBase nameserviceClient = _NameserviceImplBase.narrowCast(nameserviceCommunicator);
		
		CalculatorService calculatorServer = new CalculatorService();
		
		String calculatorServiceServerSocketString = objBroker.startNewService(calculatorServer);
		objBroker.registerNewService("calculator", calculatorServiceServerSocketString);
		nameserviceClient.rebind(calculatorServiceServerSocketString, "calculator");
		
		objBroker.shutDown();
		System.out.println("Service wurde angemeldet");
	}

}
