package mware_lib;

public interface INameService{
	
	public abstract void rebind(Object servant, String name);
	public abstract Object resolve(String name);

}
