package mware_lib;

/**
 * Nameservice Interface.
 * 
 * @author Mert Siginc
 *
 */

public interface INameService{
	
	public abstract void rebind(Object servant, String name);
	public abstract Object resolve(String name);

}
