package math_ops;

import mware_lib.ComHandler;

public class _CalculatorImplBaseStub implements _CalculatorImplBase {

	private ComHandler rawObject;

	public _CalculatorImplBaseStub(Object rawObjectRef) {
		this.rawObject = (ComHandler) rawObjectRef;
	}

	@Override
	public double add(double a, double b) {
		return (double) rawObject.sendToService(null, null);

		// if (result instanceof SomeException112) {
		// throw ((SomeException112) result);
		// } else {
		// return ((String) result);
		// }
	}

}
