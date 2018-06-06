package math_ops;

public interface _CalculatorImplBase{

	public abstract double add(double a, double b);
	
	public static _CalculatorImplBase narrowCast(Object rawObjectRef){
		return new _CalculatorImplBaseStub(rawObjectRef);
		
	}

}
