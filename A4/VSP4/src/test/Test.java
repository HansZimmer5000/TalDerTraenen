package test;

import java.io.IOException;

import junit.framework.TestCase;
import math_ops._CalculatorImplBase;
import mware_lib.MwareException;
import mware_lib.ObjectBroker;

public class Test extends TestCase {

	private String[] args = new String[3];

	@Override
	protected void setUp() throws Exception {
		args[0] = "localhost";
		args[1] = "55580";
		args[2] = "false";
	}

	public void testEntfernterAufruf() throws NumberFormatException, IOException, ClassNotFoundException {
		MiddleWare.MiddleWare.main(args);
		main.CalculatorServer.main(args);
		ObjectBroker objBroker = ObjectBroker.init(args[0], Integer.parseInt(args[1]), Boolean.parseBoolean(args[2]));
		mware_lib.INameService nameSvc = objBroker.getNameService();
		Object rawObjRef = nameSvc.resolve("zumsel");
		_CalculatorImplBase remoteObj = _CalculatorImplBase.narrowCast(rawObjRef);
		assertTrue(remoteObj.add(50.4d, 55.5d) == 105.9d);
	}

	public void testNsRegister() throws NumberFormatException, IOException, ClassNotFoundException {
		ObjectBroker objBroker = ObjectBroker.init(args[0], Integer.parseInt(args[1]), Boolean.parseBoolean(args[2]));
		mware_lib.INameService nameSvc = objBroker.getNameService();
		nameSvc.rebind(new Object(), "mertTest");
		objBroker = ObjectBroker.init(args[0], Integer.parseInt(args[1]), Boolean.parseBoolean(args[2]));
		nameSvc = objBroker.getNameService();
		Object rawObjRef = nameSvc.resolve("mertTest");
		assertTrue(rawObjRef != null && rawObjRef != "" && rawObjRef != new MwareException().toString());
	}

}
