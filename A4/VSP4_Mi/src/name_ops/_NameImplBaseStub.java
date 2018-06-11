package name_ops;

import mware_lib.ObjectReference;

public class _NameImplBaseStub extends _NameImplBase {

	private ObjectReference objectReference;

	public _NameImplBaseStub(Object con) {
		this.objectReference = (ObjectReference) con;
	}

	public void rebind(String servantSocket, String name) {
		String methodName = "rebind";
		String parameters = "String " + servantSocket + ",String " + name;
		objectReference.sendMethodAndParametersToServiceAndWaitForAnswer(methodName, parameters);
	}

	public Object resolve(String name) {
		String methodName = "resolve";
		String parameters = "String " + name;
		String socketString = objectReference.sendMethodAndParametersToServiceAndWaitForAnswer(methodName, parameters);
		return socketString;
	}

}
