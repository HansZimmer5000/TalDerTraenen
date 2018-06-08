package math_ops;

import mware_lib.SocketCommunicator;

public class _CalculatorImplBaseStub implements _CalculatorImplBase {

	private SocketCommunicator rawObject;

	public _CalculatorImplBaseStub(Object rawObjectRef) {
		this.rawObject = (SocketCommunicator) rawObjectRef;
	}

	@Override
	public int add(int a, int b) {
		String params = "int " + a + ",int " + b;
		return Integer.valueOf((String) rawObject.sendToService("add", params));
	}

}
