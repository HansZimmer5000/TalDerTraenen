package mware_lib;

public class Message {

	private String methodName;
	private Object[] parameterValues;
	private Class<?>[] parameterClasses;
	
	public Message(String message) {
		// message Example: "rebind(String 127.0.0.1:55555, String calculator)"
		String[] splitMessage = message.split("\\(");
		String methodName = splitMessage[0];
		this.methodName = methodName;
		
		if (message.contains("()")) {
			this.parameterClasses = new Class<?>[0];
			this.parameterValues = new Object[0];
		} else {
			String params = splitMessage[1].split("\\)")[0];
			String[] parameter = params.split(",");
			
			Class<?>[] paramaterClasses = new Class<?>[parameter.length];
			Object[] parameterValues = new Object[parameter.length];
			Object[] classesAndValues = new Object[2];

			String currentParameter, currentParameterClassString, currentParameterValueString;
			Class<?> currentParameterClass;
			Object currentParameterValue;

			for (int parameterIndex = 0; parameterIndex < parameter.length; parameterIndex++) {
				currentParameter = parameter[parameterIndex];
				currentParameterClassString = currentParameter.split(" ")[0];
				currentParameterValueString = currentParameter.split(" ")[1];

				switch (currentParameterClassString) {
				case ("String"):
					currentParameterClass = String.class;
					currentParameterValue = currentParameterValueString;
					paramaterClasses[parameterIndex] = currentParameterClass;
					parameterValues[parameterIndex] = currentParameterValue;
					break;
				case ("int"): // no break, so it continues to Integer
				case ("Integer"):
					currentParameterClass = int.class;
					currentParameterValue = Integer.parseInt(currentParameterValueString);
					paramaterClasses[parameterIndex] = currentParameterClass;
					parameterValues[parameterIndex] = currentParameterValue;
					break;
				case ("double"):
				case ("Double"):
					currentParameterClass = double.class;
					currentParameterValue = Double.parseDouble(currentParameterValueString);
					paramaterClasses[parameterIndex] = currentParameterClass;
					parameterValues[parameterIndex] = currentParameterValue;
					break;
				}
			}
			this.parameterClasses = paramaterClasses;
			this.parameterValues = parameterValues;
		}
	}
	
	public String getMethodName(){
		return this.methodName;
	}
	
	public Object[] getParameterValues(){
		return this.parameterValues;
	}
	
	public Class<?>[] getParameterClasses(){
		return this.parameterClasses;
	};

	private static void printMessage(Message message){
		printAllArrayElements(message.getParameterClasses());
		printAllArrayElements(message.getParameterValues());
	}
	
	private static void printAllArrayElements(Object[] arr){
		for(int i = 0; i < arr.length ; i++){
			System.out.print("," + arr[i]);
		}
		System.out.println("");
	}
	
	public static void main(String[] args) {
		Message currentMessage;

		System.out.println("--1");
		currentMessage = new Message("rebind()");
		printMessage(currentMessage);

		System.out.println("--2");
		currentMessage =  new Message("rebind(String a)");
		printMessage(currentMessage);
		
		System.out.println("--3");
		currentMessage =  new Message("rebind(String a,String b)");
		printMessage(currentMessage);
	}
}
