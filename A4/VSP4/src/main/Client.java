package main;

import java.io.IOException;

import math_ops._CalculatorImplBase;
import mware_lib.ObjectBroker;

/**
 * Client main class.
 * 
 * @author Mert Siginc In dieser Klasse startet man den Client
 *
 */

public class Client {

	public static void main(String[] args) throws IOException {
		if(args.length <3) {
			System.out.println("wrong num of args");
			return;
		}
		ObjectBroker objBroker = ObjectBroker.init(args[0], Integer.parseInt(args[1]), Boolean.parseBoolean(args[2]));
		mware_lib.INameService nameSvc = objBroker.getNameService();

		System.out.println("C> NS-Referenz erhalten!");
		Object rawObjRef = nameSvc.resolve("zumsel");
		_CalculatorImplBase remoteObj = _CalculatorImplBase.narrowCast(rawObjRef);

		System.out.println("C> Methodenaufruf wird initiiert");
		// TODO:: Parameter als methoden parameter
		double calcNum = remoteObj.add(1550.4d, 55.5d);
		System.out.println("C> Rechnung wurde durchgeführt das Ergebnis ist: " + calcNum);

		objBroker.shutDown();

	}

}
