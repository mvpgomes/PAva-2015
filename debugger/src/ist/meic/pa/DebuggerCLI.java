package ist.meic.pa;

import javassist.ClassPool;
import javassist.Loader;

import java.util.Arrays;

public class DebuggerCLI {

    // Assumes the app to be debugged has a non-empty package name.
    public static void main(String[] args) throws Throwable {
        if (args.length < 1) {
            System.err.println("Not enough arguments.");
            System.err.println("Expected <app> <to be debugged app name>");
            System.exit(-1);
        }

        final String dAppName = args[0];
        final String dPackageName = dAppName.replaceFirst("^(\\w+)((\\.\\w+)*)\\.\\w+$", "$1$2");
        final String[] dAppArgs = Arrays.copyOfRange(args, 1, args.length);
        System.out.println("Debugging app: " + dAppName);
        System.out.println("Debugging app package name: " + dPackageName);

        // instrument future <dPackageName> classes
        DebuggerTranslator t = new DebuggerTranslator(dPackageName);
        ClassPool cp = ClassPool.getDefault();
        Loader cl = new Loader();
        cl.addTranslator(cp, t);
        cl.run(dAppName, dAppArgs);
    }
}
