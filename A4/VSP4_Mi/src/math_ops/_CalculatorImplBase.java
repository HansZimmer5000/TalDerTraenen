package math_ops;

public interface _CalculatorImplBase{

	public abstract int add(int a, int b);
	
	public static _CalculatorImplBase narrowCast(Object rawObjectRef){
		return new _CalculatorImplBaseStub(rawObjectRef);
	}

}
