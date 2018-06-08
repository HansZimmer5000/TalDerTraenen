package math_ops
public abstract class _CalculatorImplBase{
    public abstract double add(int _int, int _int);
    public abstract String getStr(double _double);
    public static _CalculatorImplBase narrowCast(Object rawObjectReference){...};
}
