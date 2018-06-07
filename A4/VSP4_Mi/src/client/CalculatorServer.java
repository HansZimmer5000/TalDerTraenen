package client;

import java.io.IOException;
import mware_lib.*;
import math_ops.*;

public class CalculatorServer implements _CalculatorImplBase {
	
	protected CalculatorServer() {
		super();
	}

	public int add(int a, int b) {
		return a + b;
	}

	public static void main(String[] args) throws IOException, ClassNotFoundException {

	
		ObjectBroker objBroker = ObjectBroker.init("", 55555, false);
		NameService nameSvc = objBroker.getNameService();
		CalculatorServer calculatorServer = new CalculatorServer();
		
		SkeletonServer serviceServer = new SkeletonServer(calculatorServer);
		serviceServer.start();


		String calculatorServerSocket = 
				serviceServer.getServerSocket().getInetAddress().getHostAddress()+":"+
				serviceServer.getServerSocket().getLocalPort();
		
		nameSvc.rebind(calculatorServerSocket, "zumsel");
		objBroker.shutDown();
		System.out.println("Service wurde angemeldet");
	}

}
