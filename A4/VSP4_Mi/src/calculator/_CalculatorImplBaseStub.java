package calculator;

import java.util.ArrayList;
import java.util.Arrays;

import mware_lib.Communicator;

public class _CalculatorImplBaseStub implements _CalculatorImplBase {

	private Communicator rawObject;

	public _CalculatorImplBaseStub(Object rawObjectRef) {
		this.rawObject = (Communicator) rawObjectRef;
	}

	@Override
	public int add(int a, int b) {
		String params;
		
		params = "int " + a + ",int " + b;
		
		System.out.println(params);
		return new Integer((String) rawObject.sendToService("add", params));

		// if (result instanceof SomeException112) {
		// throw ((SomeException112) result);
		// } else {
		// return ((String) result);
		// }
	}

}
