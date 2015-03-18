package ist.meic.pa;

import javassist.*;

public class DebuggerTranslator implements Translator {
    private final String packageName;

    public DebuggerTranslator(String packageName) {
        this.packageName = packageName;
    }

    @Override
    public void start(ClassPool classPool) throws NotFoundException, CannotCompileException {
        // Empty on purpose
    }

    @Override
    public void onLoad(ClassPool classPool, String className) throws NotFoundException, CannotCompileException {
        if (className.startsWith(packageName)) {
            CtClass c = classPool.get(className);
            instrumentMethods(c);
        }
    }

    private static void instrumentMethods(CtClass c) {
        try {
            c.getDeclaredMethod("main").insertBefore("{ System.out.println(\"Before\"); }");
        } catch (Throwable t) {
            throw new RuntimeException(t);
        }
    }
}
