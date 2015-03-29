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

        // The order of the editors must be preserved because the constructor editor instruments the constructor call with
        // a method call that can be later instrumented by the method editor.
        ClassEditor t = new ClassEditor(Arrays.asList(new MethodCallEditor(), new ConstructorCallEditor()));
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

