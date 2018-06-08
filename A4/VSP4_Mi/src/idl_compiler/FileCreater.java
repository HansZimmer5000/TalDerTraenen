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
	String fileName;
	IDLclass toCompilingClass;
	ArrayList<String> linesToWrite;
	
	public FileCreater(String moduleName, IDLclass toCompilingClass) {
		this.moduleName = moduleName;
		this.className = "_" + toCompilingClass.getClassName() + "ImplBase";
		this.fileName = this.className + ".java";
		this.toCompilingClass = toCompilingClass;
		this.linesToWrite = compileIDLClass();
	}
	
	public void writeFile() {
		Path file = Paths.get(this.fileName);
		try {
			Files.write(file, this.linesToWrite, Charset.forName("UTF-8"));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public ArrayList<String> compileIDLClass(){
		ArrayList<String> compiledLinesForNewFile = new ArrayList<>();
		
		compiledLinesForNewFile.add(createPackageLine());
		compiledLinesForNewFile.addAll(createClassLines());
		
		return compiledLinesForNewFile;
	}

	private String createPackageLine() {
		return "package " + this.moduleName;
	}
	
	private ArrayList<String> createClassLines() {
		ArrayList<String> classLines = new ArrayList<>();
		
		String classLine = createClassLine();
		ArrayList<String> methodLines = createMethodLines();
		String classEndLine = "}";
		
		classLines.add(classLine);
		classLines.addAll(methodLines);
		classLines.add(classEndLine);
		
		return classLines;
	}

	private String createClassLine() {
		return "public abstract class " + this.className + "{";
	}

	private ArrayList<String> createMethodLines() {
		ArrayList<String> methodLines = new ArrayList<>();
		
		String currentMethodLine;
		MethodData[] methods = this.toCompilingClass.getMethods();
		for(MethodData currentMethod : methods) {
			
			String returnType = IDLCompiler.getSupportedJavaDataTypeName(currentMethod.getReturnType());
			String methodName = currentMethod.getName();
			String params = compileParams(currentMethod.getParamTypes());
			currentMethodLine = createMethodLine("abstract", returnType, methodName, params);
			methodLines.add(currentMethodLine);
		}
		currentMethodLine = createMethodLine("static", this.className, "narrowCast", "Object rawObjectReference", "{...}");
		methodLines.add(currentMethodLine);
		
		return methodLines;
	}

	private String createMethodLine(String methodType, String returnType, String methodName, String params) {
		//public abstract int add(int a, int b);
		return createMethodLine(methodType, returnType, methodName, params, "");
	}
	
	private String createMethodLine(String methodType, String returnType, String methodName, String params, String body) {
		return "    public " + methodType + " " + returnType + " " + methodName + "(" + params + ")" + body + ";";
	}
	
	private String compileParams(SupportedDataTypes[] paramTypes) {
		String paramString = "";
		String currentCompiledDataTypeString;
		for(SupportedDataTypes currentDataType : paramTypes) {
			if(paramString != ""){
				paramString += ", ";
			}
			currentCompiledDataTypeString = IDLCompiler.getSupportedJavaDataTypeName(currentDataType);
			paramString += currentCompiledDataTypeString + " " + createParamName(currentCompiledDataTypeString);
		}
		return paramString;
	}

	private String createParamName(String string) {
		String restString = string.substring(1, string.length());
		String firstChar = string.substring(0,1);
		String firstCharLittle = firstChar.toLowerCase();
		if(firstChar.equals(firstCharLittle)) {
			return "_" + string;
		} else {
			return firstCharLittle + restString;
		}
		
	}
}
