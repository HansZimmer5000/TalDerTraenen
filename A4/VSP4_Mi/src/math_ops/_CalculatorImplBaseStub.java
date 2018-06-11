package math_ops;

import mware_lib.ObjectReference;

public class _CalculatorImplBaseStub extends _CalculatorImplBase {

	private ObjectReference objectReference;

	public _CalculatorImplBaseStub(Object objectReference) {
		this.objectReference = (ObjectReference) objectReference;
	}

	@Override
	public int add(int a, int b) {
		String params = "int " + a + ",int " + b;
		return Integer.valueOf((String) objectReference.sendMethodAndParametersToServiceAndWaitForAnswer("add", params));
	}
	
	@Override
	public double div(double a, double b) {
		String params = "double " + a + ",double " + b;
		return Double.valueOf((String) objectReference.sendMethodAndParametersToServiceAndWaitForAnswer("div", params));
	}

}
