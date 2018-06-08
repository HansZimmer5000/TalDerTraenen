package math_ops;
public abstract class _CalculatorImplBase{
    public abstract int add(int a, int b);
    public static _CalculatorImplBase narrowCast(Object rawObjectReference){
    	return new _CalculatorImplBaseStub(rawObjectReference);
    };
}
