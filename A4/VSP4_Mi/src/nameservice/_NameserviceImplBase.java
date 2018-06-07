package nameservice;

public interface _NameserviceImplBase{
	
	public abstract void rebind(String servantSocket, String name);
	public abstract Object resolve(String name);
	
	public static _NameserviceImplBase narrowCast(Object rawObjectRef){
		return new _NameserviceImplBaseStub(rawObjectRef);
	}

}
