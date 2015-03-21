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
            for (CtMethod m : c.getDeclaredMethods()) {
                if (!m.isEmpty()) {
                    injectAddCallStack(m);
                    injectInspect(m);
                }
            }
        } catch (Throwable t) {
            // should not happen
            throw new RuntimeException(t);
        }
    }

    private static void injectAddCallStack(CtMethod m) {
        try {
            final String instanceCmd = Modifier.isStatic(m.getModifiers()) ? "null" : "$0";
            final boolean isVoid = m.getReturnType().getSimpleName().equals("void");
            final String returnCmd = isVoid ? "null" : "$type";
            m.insertBefore("{ " +
                    "           ist.meic.pa.Debugger.addCall($class, " + instanceCmd + ", " +
                    "                                        \"" + m.getName() + "\"," +
                    "                                        $sig, $args, " + returnCmd + ");" +
                    "}");
        } catch (Throwable t) {
            throw new RuntimeException(t);
        }
    }

    // TODO some examples on how to generate code to handle primitive return types.
    private static void injectInspect(CtMethod m) {
        try {
            final String returnType = m.getReturnType().getSimpleName();
            final CtClass etype = ClassPool.getDefault().get("java.lang.Throwable");

            // Note that CtMethod.addCatch must throw exception or return value.
            switch (returnType) {
                case "void":
                    m.addCatch("{ ist.meic.pa,Debugger.setThrowedException($e); ist.meic.pa.Debugger.inspect(); return; }", etype);
                    break;
                case "int":
                    m.addCatch("{ return ((Integer)ist.meic.pa.Debugger.inspect()).intValue(); }", etype);
                    break;
                default:
                    throw new RuntimeException("There is no support for that type.");
            }

        } catch (Throwable t) {
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
