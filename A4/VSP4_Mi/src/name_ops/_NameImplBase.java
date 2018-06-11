package name_ops;
public abstract class _NameImplBase{
	public static final int NAMESERVICEPORT = 15000;
	
    public abstract void rebind(String a, String b);
    public abstract Object resolve(String a);
    public static _NameImplBase narrowCast(Object rawObjectReference){ return new _NameImplBaseStub(rawObjectReference);};
}
