package ist.meic.pa;

import javassist.*;

public class DebuggerTranslator implements Translator {
    private final String packageName;

    public DebuggerTranslator(String packageName) {
        this.packageName = packageName;
    }

    private static void instrumentMethods(CtClass c) {
        if (c == null || c.getName().equals("java.lang.Object")) {
            return;
        }

        // instrument parent
        try {
            instrumentMethods(c.getSuperclass());
        } catch (NotFoundException e) {
            // should not happen
            throw new RuntimeException(e);
        }

        try {
            final CtClass etype = ClassPool.getDefault().get("java.lang.Throwable");
            for (CtMethod m : c.getDeclaredMethods()) {
                if (!m.isEmpty()) {
                    final String instanceCmd = Modifier.isStatic(m.getModifiers()) ? "null" : "$0";
                    final String returnCmd = m.getReturnType().getSimpleName().equals("void") ? "null" : "$type";
                    m.insertBefore("{ " +
                            "           ist.meic.pa.Debugger.addCall($class, " + instanceCmd + ", " +
                            "                                        \"" + m.getName() + "\"," +
                            "                                        $sig, $args, " + returnCmd + ");" +
                            "}");

                    // Note that CtMethod.addCatch must throw exception or return value.
                    m.addCatch("{ " +
                            "       ist.meic.pa.Debugger.inspect();" +
                            "       throw $e;" +
                            "}", etype);
                }
            }
        } catch (Throwable t) {
            // should not happen
            throw new RuntimeException(t);
        }
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
}
