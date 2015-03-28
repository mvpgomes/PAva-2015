package ist.meic.pa;

import ist.meic.pa.editor.ConstructorCallEditor;
import ist.meic.pa.editor.MethodCallEditor;
import javassist.*;

import java.util.Arrays;

public class DebuggerCLI {
    public static void main(String[] args) throws Throwable {
        if (args.length < 1) {
            System.err.println("Not enough arguments.");
            System.err.println("Expected <to be debugged app name>");
            return;
        }

        final String dAppName = args[0];
        final String[] dAppArgs = Arrays.copyOfRange(args, 1, args.length);

        ClassEditor t = new ClassEditor(Arrays.asList(new ConstructorCallEditor(), new MethodCallEditor()));
        ClassPool cp = ClassPool.getDefault();
        Loader cl = new Loader();
        cl.addTranslator(cp, t);

        // Since the ClassEditor only instruments method calls after the main method is invoked, and the main method of
        // the class to be debugged must be instrumented, a fake class with a main method is created which invokes the
        // to be debugged main method afterwards.
        CtClass newClass = cp.makeClass("DebuggerCLI-Fake");
        CtMethod newMethod = CtNewMethod.make(String.format("public static void main(String[] args) { %s.main(args); }", dAppName), newClass);
        newClass.addMethod(newMethod);

        cl.run("DebuggerCLI-Fake", dAppArgs);
    }
}

