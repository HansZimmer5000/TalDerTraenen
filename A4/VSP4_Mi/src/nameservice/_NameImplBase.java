package nameservice;

public interface _NameImplBase{
	
	public abstract void rebind(String servantSocket, String name);
	public abstract Object resolve(String name);
	
	public static _NameImplBase narrowCast(Object rawObjectRef){
		return new _NameImplBaseStub(rawObjectRef);
	}

}
