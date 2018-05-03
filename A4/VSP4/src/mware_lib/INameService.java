package mware_lib;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface INameService extends Remote{
	
	public abstract void rebind(Object servant, String name) throws RemoteException;
	public abstract Object resolve(String name) throws RemoteException;

}
