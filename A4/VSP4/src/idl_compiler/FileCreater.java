package idl_compiler;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;

import idl_compiler.IDLCompiler.MethodData;
import idl_compiler.IDLCompiler.SupportedDataTypes;

public class FileCreater {

	String moduleName;
	String className;
	IDLclass idlClass;
	ArrayList<String> linesToWrite;

	public FileCreater(String moduleName, IDLclass idlClass) {
		this.moduleName = moduleName;
		this.className = "_" + idlClass.getClassName() + "ImplBase";
		this.idlClass = idlClass;
	}

	public void createFile() {
		this.linesToWrite = startCompiler();
		Path path = Paths.get("");
		path.resolve("src/" + this.moduleName).toFile().mkdirs();

		Path file = Paths.get("src/" + this.moduleName + "/" + this.className + ".java");
		try {
			Files.write(file, this.linesToWrite, Charset.forName("UTF-8"));
		} catch (IOException e) {
			e.printStackTrace();
		}
		System.out.println(this.className + " wurde erstellet");
	}

	public void createStub() {
		this.linesToWrite = startStubCompiler();
		Path path = Paths.get("");
		path.resolve("src/" + this.moduleName).toFile().mkdirs();

		Path file = Paths.get("src/" + this.moduleName + "/" + this.className + "Stub.java");
		try {
			Files.write(file, this.linesToWrite, Charset.forName("UTF-8"));
		} catch (IOException e) {
			e.printStackTrace();
		}
		System.out.println(this.className + "Stub.java wurde erstellet");
	}

	public ArrayList<String> startCompiler() {
		ArrayList<String> compiledString = new ArrayList<>();

		compiledString.add("package " + this.moduleName + ";\n");
		compiledString.add("public interface " + this.className + "{\n");
		compiledString.add(addAllMethods());
		compiledString.add(addNarrowCast());
		compiledString.add("}");

		return compiledString;
	}

	public ArrayList<String> startStubCompiler() {
		ArrayList<String> compiledString = new ArrayList<>();

		compiledString.add("package " + this.moduleName + ";\n");
		compiledString.add("import mware_lib.ComHandler;\n");
		compiledString.add("import java.util.ArrayList;\n" + 
							"import java.util.Arrays;\n");
		compiledString.add("public class " + this.className + "Stub implements " + this.className + "{\n");
		compiledString.add("private ComHandler rawObject;");
		compiledString.add(stubConstructor());
		compiledString.add(addAllStubMethods());
		compiledString.add("}");

		return compiledString;
	}

	private String addAllStubMethods() {
		String retString = "";
		for (MethodData method : idlClass.getMethods()) {
			retString += "public " + new String(method.getReturnType().toString()).toLowerCase() + " "
					+ method.getName() + "(" + parseParams(method.getParamTypes()) + ") {\n";
			retString += "ArrayList<Object> params = new ArrayList<Object>(Arrays.asList(a,b));\n";
			retString += "String answerString = params.toString();\n";
			retString += "answerString = answerString.substring(1, answerString.length()-1);\n";
			retString += "return new Double((String) rawObject.sendToService(\"add\", answerString));\n}";

		}
		return retString;
	}

	private String stubConstructor() {
		String retString = "";
		retString += "public " + this.className + "Stub(Object rawObjectRef) {\n";
		retString += "String nsAnswerSplited[] = ((String) rawObjectRef).split(\"\\\\|\");\n";
		retString += "rawObject = new ComHandler(nsAnswerSplited[0], new Integer(nsAnswerSplited[1]), false);\n}";

		return retString;
	}

	private String addAllMethods() {
		String retString = "";
		for (MethodData method : idlClass.getMethods()) {
			retString += "public abstract " + new String(method.getReturnType().toString()).toLowerCase() + " "
					+ method.getName() + "(" + parseParams(method.getParamTypes()) + ");\n";
		}
		return retString;
	}

	private String parseParams(SupportedDataTypes[] paramTypes) {
		int startCharNumber = 97;
		int charNumber = startCharNumber;
		String paramString = "";
		String currentCompiledDataTypeString;
		for (SupportedDataTypes currentDataType : paramTypes) {
			if (paramString != "") {
				paramString += ", ";
			}
			currentCompiledDataTypeString = IDLCompiler.getSupportedJavaDataTypeName(currentDataType);
			paramString += currentCompiledDataTypeString + " " + Character.toString((char) charNumber);
			charNumber = charNumber + 1;
		}
		return paramString;
	}

	private String addNarrowCast() {
		String retString = "";
		retString += "public static " + this.className + " narrowCast(Object rawObjectRef){\n";
		retString += "\t return new " + this.className + "Stub(rawObjectRef);\n}";

		return retString;
	}
}
