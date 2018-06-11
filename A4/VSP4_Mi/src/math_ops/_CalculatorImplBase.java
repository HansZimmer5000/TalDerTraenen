package math_ops;
public abstract class _CalculatorImplBase{
	public static String SERVICE_NAME = "calculator";
	
    public abstract int add(int a, int b);
    public abstract double div(double a, double b);
    public static _CalculatorImplBase narrowCast(Object rawObjectReference){ return new _CalculatorImplBaseStub(rawObjectReference);};
}
