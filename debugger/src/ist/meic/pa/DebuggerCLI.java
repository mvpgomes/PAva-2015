package ist.meic.pa;

import javassist.ClassPool;
import javassist.Loader;

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

        ClassEditor t = new ClassEditor();
        ClassPool cp = ClassPool.getDefault();
        Loader cl = new Loader();
        cl.addTranslator(cp, t);
        cl.run(dAppName, dAppArgs);
    }
}

