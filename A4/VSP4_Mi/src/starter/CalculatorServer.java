package starter;

import java.io.IOException;

import calculator.*;
import mware_lib.*;
import nameservice._NameserviceImplBaseStub;

public class CalculatorServer implements _CalculatorImplBase {
	
	protected CalculatorServer() {
		super();
	}

	public int add(int a, int b) {
		return a + b;
	}

	public static void main(String[] args) throws IOException, ClassNotFoundException {
		ObjectBroker objBroker = ObjectBroker.init("", 55555, false);
		CalculatorServer calculatorServer = new CalculatorServer();
		
		objBroker.registerNewService("calculator", calculatorServer);
		
		objBroker.shutDown();
		System.out.println("Service wurde angemeldet");
	}

}
