package math_ops;

import mware_lib.SocketCommunicator;

public class _CalculatorImplBaseStub extends _CalculatorImplBase {

	private SocketCommunicator rawObject;

	public _CalculatorImplBaseStub(Object rawObjectRef) {
		this.rawObject = (SocketCommunicator) rawObjectRef;
	}

	@Override
	public int add(int a, int b) {
		String params = "int " + a + ",int " + b;
		return Integer.valueOf((String) rawObject.sendToService("add", params));
	}
	
	@Override
	public double div(double a, double b) {
		String params = "double " + a + ",double " + b;
		return Double.valueOf((String) rawObject.sendToService("div", params));
	}

}
