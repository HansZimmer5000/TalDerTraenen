package mware_lib;

public interface INameService{
	
	public abstract void rebind(String servantSocket, String name);
	public abstract Object resolve(String name);

}
