package math_ops;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface _CalculatorImplBase extends Remote {

	public abstract double add(double a, double b) throws RemoteException;
	
	public static _CalculatorImplBase narrowCast(Object rawObjectRef) throws RemoteException{
		return (_CalculatorImplBase) rawObjectRef;
		
	}

}
