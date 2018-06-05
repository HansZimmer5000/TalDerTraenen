package math_ops;

import java.util.ArrayList;
import java.util.Arrays;

import mware_lib.ComHandler;

public class _CalculatorImplBaseStub implements _CalculatorImplBase {

	private ComHandler rawObject;

	public _CalculatorImplBaseStub(Object rawObjectRef) {
		this.rawObject = (ComHandler) rawObjectRef;
	}

	@Override
	public double add(double a, double b) {
		ArrayList<Double> params = new ArrayList<Double>(Arrays.asList(a,b));
		String answerString = params.toString();
		answerString = answerString.substring(1, answerString.length()-1);
		System.out.println(answerString);
		return new Double((String) rawObject.sendToService("add", answerString));

		// if (result instanceof SomeException112) {
		// throw ((SomeException112) result);
		// } else {
		// return ((String) result);
		// }
	}

}
