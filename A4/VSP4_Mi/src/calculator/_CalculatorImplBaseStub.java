package calculator;

import mware_lib.Communicator;

public class _CalculatorImplBaseStub implements _CalculatorImplBase {

	private Communicator rawObject;

	public _CalculatorImplBaseStub(Object rawObjectRef) {
		this.rawObject = (Communicator) rawObjectRef;
	}

	@Override
	public int add(int a, int b) {
		String params = "int " + a + ",int " + b;
		return Integer.valueOf((String) rawObject.sendToService("add", params));
	}

}
