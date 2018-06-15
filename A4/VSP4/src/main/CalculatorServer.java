package main;

import java.io.IOException;
import mware_lib.*;
import math_ops.*;

/**
 * Service main class.
 * 
 * @author Mert Siginc
 * In dieser Klasse befinden sich die main Methode zum Service
 *
 */

public class CalculatorServer implements _CalculatorImplBase {
	
	protected CalculatorServer() {
		super();
	}

	public double add(double a, double b) {
		return a + b;
	}

	public static void main(String[] args) throws IOException, ClassNotFoundException {
	
		ObjectBroker objBroker = ObjectBroker.init(args[0], Integer.parseInt(args[1]), Boolean.parseBoolean(args[2]));
		NameService nameSvc = objBroker.getNameService();
		nameSvc.rebind(new CalculatorServer(), "zumsel");
		objBroker.shutDown();
		Util.println("S> Service wurde angemeldet", false);
		
	}

}
