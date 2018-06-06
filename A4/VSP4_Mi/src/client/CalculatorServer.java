package client;

import java.io.IOException;
import mware_lib.*;
import math_ops.*;

public class CalculatorServer implements _CalculatorImplBase {
	
	protected CalculatorServer() {
		super();
	}

	public double add(double a, double b) {
		return a + b;
	}

	public static void main(String[] args) throws IOException, ClassNotFoundException {

	
		ObjectBroker objBroker = ObjectBroker.init("", 55555, false);
		NameService nameSvc = objBroker.getNameService();
		nameSvc.rebind(new CalculatorServer(), "zumsel");
		objBroker.shutDown();
		System.out.println("Service wurde angemeldet");
	}

}
