package mware_lib;

public class ObjectReference {

	private ObjectBroker objectBroker;
	private String serviceServerSocketString;

	private ObjectReference(ObjectBroker objectBroker, String serviceServerSocketString){
		this.objectBroker = objectBroker;
		this.serviceServerSocketString = serviceServerSocketString;
	}
	
	public static ObjectReference init(ObjectBroker objectBroker, String host, int port) {
		return ObjectReference.init(objectBroker, host + ":" + port);
	}
	
	public static ObjectReference init(ObjectBroker objectBroker, String serviceServerSocketString){
		return new ObjectReference(objectBroker, serviceServerSocketString);
	}
	
	public String sendMethodAndParametersToServiceAndWaitForAnswer(String methodName, String parameters) {
		return this.objectBroker.sendMethodAndParametersToServiceAndWaitForAnswer(this.serviceServerSocketString, methodName, parameters);
	}
}
